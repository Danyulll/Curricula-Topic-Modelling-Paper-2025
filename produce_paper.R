# ============================================================================
# LDA Topic Modeling Analysis (VEM only; outputs under clean/)
# ============================================================================

suppressPackageStartupMessages({
  library(quanteda)
  library(quanteda.textstats)
  library(textstem)
  library(dplyr)
  library(ggplot2)
  library(tm)
  library(stringr)
  library(tidyr)
  library(tidytext)
  library(topicmodels)
  library(ldatuning)
  library(LDAvis)
  library(stringi)
  library(reshape2)
  library(scales)
  library(proxy)      # for jsPCA in LDAvis
  library(slam)       # row_sums / col_sums for DTM
})

# ---- dirs (clean/*) ----
dir.create("clean/images", showWarnings = FALSE, recursive = TRUE)
dir.create("clean/rdata",  showWarnings = FALSE, recursive = TRUE)
dir.create("clean/viz",    showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load / Build Document-Term Matrix
# ============================================================================

dtm_path_clean <- "clean/rdata/spdtm_all_r.RData"

have_dtm <- file.exists(dtm_path_clean)

if (!have_dtm) {
  load("./degree_corpus_total.RData")
  colnames(degree_corpus_total) <- c("doc_id", "text")
  degree_corpus_total <- degree_corpus_total |>
    filter(text != "") |>
    distinct(doc_id, .keep_all = TRUE)
  
  # Define custom stopwords
  custom_sw <- c(
    "fall","winter","spring","summer","term","terms",
    "course","courses","offered","offering","topic","topics",
    "introduction","overview","session","sessions",
    "lecture","lectures","lab","labs","seminar","seminars",
    "credit","credits","prerequisite","prerequisites",
    "department","departments","study","studies","explore","explores",
    "design","designed","focus","focuses","emphasis","based",
    "students","student","will","uses","use","includes","including",
    "assignment","assignments","assessment","assessments","evaluation","evaluations",
    "project","projects","reading","readings","laboratory","laboratories",
    "tutorial","tutorials","online","inperson","delivery","delivered",
    "method","methods","skill","skills","competence","competencies",
    "knowledge","understanding","principle","principles","theory","theories",
    "practice","practices","application","applications","case","cases",
    "example","examples","methodology","methodologies","approach","approaches",
    "analyses","researchers","activity","activities","practicum","externship",
    "internship","fieldwork","placement","site","sites","discussions","feedback",
    "grade","grades","exam","exams","quiz","quizzes","midterm","midterms",
    "final","finals","presentation","presentations","guide","guides",
    "manual","manuals","textbook","textbooks","unit","units","module","modules",
    "section","sections","theme","themes","outline","outlines","foundation",
    "foundations","requirement","requirements","major","program","faculty",
    "science","technique","meaningful","way","honours","require","pass","phy",
    "week","time","include","introduce","hold","life","option","work","apply",
    "problem","solve","representation","manipulation","real","world","rich",
    "discipline","intend","former","honour","high","level","basic","concept","may","enter","grant","offer","list_stack_queue", "queue_tree", "associate", "file", "execute", "compilation",
    "load", "library", "build", "provide", "experience", "field", "unique", "large",
    "concept", "cover", "demonstrate", "connect", "upper", "low", "bound", "level",
    "present", "mathematical_reasoning", "graph_tree", "basic", "correspond",
    "property", "population", "selection", "enable", "good", "critically", "evaluate",
    "performance", "define", "exploit", "certain", "simple", "situation", "strong",
    "underpinning", "estimation_hypothesis_testing", "experimental", "large_number",
    "collect", "social", "administrative", "record", "multiple", "additional", "solution",
    "pitfall", "motivation", "variety", "phenomenon", "discuss", "path", "non", "rigorous","treatment",
    "achieve", "ensure", "facilitate", "support", "improve", "enhance", "develop",
    "encourage", "apply", "use", "implement", "perform", "conduct", "assess",
    "analyze", "investigate", "explore", "examine", "illustrate", "interpret",
    "identify", "determine", "measure", "obtain", "select", "specify", "justify",
    "involve", "coordinate", "manage", "organize", "integrate", "include", "highlight","datum","m","rn"
  )
  
  all_sw <- c(stopwords("en"), custom_sw)
  
  # Preprocess text
  texts_lemma <- degree_corpus_total$text |>
    tolower() |>
    lemmatize_strings()
  
  # Tokenize and clean
  toks <- quanteda::tokens(
    texts_lemma,
    remove_punct = TRUE,
    remove_numbers = TRUE
  )
  
  # Keep only alphabetic tokens of length ≥2
  toks <- tokens_select(
    toks,
    pattern = "^[a-z]{2,}$",
    valuetype = "regex"
  )
  
  # Remove stopwords
  toks_clean <- tokens_remove(toks, pattern = all_sw, valuetype = "fixed")
  
  # Find bigram collocations
  collocs <- textstat_collocations(
    toks_clean,
    size = 2,
    min_count = 500
  )
  
  # Remove duplicate patterns and keep top 50
  collocs <- collocs[!grepl("^(\\S+) \\1$", collocs$collocation), ]
  top50 <- collocs |>
    arrange(-z) |>
    slice_head(n = 50)
  
  # Create compound tokens
  bigram_patterns <- phrase(top50$collocation)
  bigram_patterns <- bigram_patterns[c(-12,-18,-14,-6,-5,-27,-24,-16,-9,-21)]
  toks_compound <- tokens_compound(toks_clean, pattern = bigram_patterns)
  
  # Create document-feature matrix
  dfmat <- dfm(toks_compound)
  dfmat <- dfm_remove(dfmat, pattern = all_sw, valuetype = "fixed")
  dfmat <- dfm_trim(dfmat, min_termfreq = 5)
  
  # Convert to tm-style DTM and save
  rownames(dfmat) <- degree_corpus_total$doc_id
  dtm <- convert(dfmat, to = "tm")
  
  save(dtm, file = dtm_path_clean)
  
} else {
  if (file.exists(dtm_path_clean)) {
    load(dtm_path_clean) # loads dtm
  } else {
    load(dtm_legacy)     # loads dtm from legacy path
    # also save a copy into clean/ for future runs
    save(dtm, file = dtm_path_clean)
  }
}

# ============================================================================
# Model Tuning (optional)
# ============================================================================

set.seed(472238)
k_seq <- 2:25

tune_path <- "clean/rdata/tuning_results.RData"
if (file.exists(tune_path)) {
  load(tune_path) # loads 'tuning_results'
} else {
  tuning_results <- FindTopicsNumber(
    dtm,
    topics = k_seq,
    control = list(seed = 472238),
    mc.cores = 13L,
    verbose = TRUE,
    metrics = c("CaoJuan2009", "Deveaud2014")
  )
  save(tuning_results, file = tune_path)
}

df <- tuning_results
if ("LDA_model" %in% names(df)) df$LDA_model <- NULL
metric_cols <- setdiff(names(df), "topics")
df[metric_cols] <- lapply(df[metric_cols], function(col) rescale(col, to = c(0, 1)))
df_long <- melt(df, id.vars = "topics", variable.name = "metric", value.name = "value")

metric_levels <- unique(df_long$metric)
shape_vals <- setNames(seq(15, 15 + length(metric_levels) - 1), metric_levels)

tuning_plot <- ggplot(df_long, aes(x = topics, y = value, group = metric)) +
  geom_line(color = "black") +
  geom_point(aes(shape = metric), size = 3, color = "black") +
  scale_shape_manual(values = shape_vals) +
  scale_x_continuous(breaks = unique(df_long$topics)) +
  labs(x = "Number of topics", y = "Rescaled score", shape = "Metric") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        legend.position = "right")

ggsave("clean/images/lda_tuning.pdf", tuning_plot, width = 8, height = 5, units = "in")

# ============================================================================
# Fit VEM Models (k = 3 and k = 10)
# ============================================================================

vem3_path  <- "clean/rdata/lda_out_3_vem.RData"
vem10_path <- "clean/rdata/lda_out_10_vem.RData"

if (file.exists(vem3_path)) {
  load(vem3_path)  # loads lda.out.3.vem
} else {
  lda.out.3.vem <- LDA(dtm, 3, method = "VEM", control = list(seed = 4722, estimate.beta = TRUE))
  save(lda.out.3.vem, file = vem3_path)
}

if (file.exists(vem10_path)) {
  load(vem10_path) # loads lda.out.10.vem
} else {
  lda.out.10.vem <- LDA(dtm, 10, method = "VEM", control = list(seed = 20250706, estimate.beta = TRUE))
  save(lda.out.10.vem, file = vem10_path)
}

# ============================================================================
# LDAvis (VEM)
# ============================================================================

doc_length <- slam::row_sums(dtm) |> as.integer()
term_freq  <- slam::col_sums(dtm) |> as.integer()
vocab      <- dtm$dimnames$Terms

jsPCA <- function(phi) {
  dist.mat <- proxy::dist(x = phi)
  pca.fit  <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}

# k = 3
phi_3_vem   <- posterior(lda.out.3.vem)$terms
theta_3_vem <- posterior(lda.out.3.vem)$topics
json_lda_3_vem <- LDAvis::createJSON(
  phi = phi_3_vem, theta = theta_3_vem, vocab = vocab,
  doc.length = doc_length, term.frequency = term_freq
)
LDAvis::serVis(json_lda_3_vem, out.dir = "ldavis/lda_3_vem", open.browser = T)

# k = 10
phi_10_vem   <- posterior(lda.out.10.vem)$terms
theta_10_vem <- posterior(lda.out.10.vem)$topics
json_lda_10_vem <- LDAvis::createJSON(
  phi = phi_10_vem, theta = theta_10_vem, vocab = vocab,
  doc.length = doc_length, term.frequency = term_freq, mds.method = jsPCA
)
LDAvis::serVis(json_lda_10_vem, out.dir = "ldavis/lda_10_vem", open.browser = T)




# ============================================================================
# Top-Term Plots (VEM)
# ============================================================================

# k = 3
top_terms_3_vem <- tidy(lda.out.3.vem, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  mutate(term = reorder_within(term, beta, topic))

terms_plot_3_vem <- ggplot(top_terms_3_vem, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 Words per Topic (3-Topic VEM)", x = NULL, y = "Probability")

ggsave("clean/images/top_terms_3_topics_vem.png", terms_plot_3_vem, width = 10, height = 6, dpi = 600)

# k = 10
top_terms_10_vem <- tidy(lda.out.10.vem, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 30) |>
  ungroup() |>
  mutate(term = reorder_within(term, beta, topic))

# Desired display order by label:
# Machine Learning, Calculus, Linear Algebra, Algorithms & Data Structures,
# Probability, Applied Statistics, Business Analytics, Software Engineering,
# Information Systems, Scientific Computing
topic_order_nums <- c(7, 5, 9, 8, 2, 3, 10, 6, 1, 4)

top_terms_10_vem <- tidy(lda.out.10.vem, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 30) |>
  ungroup() |>
  mutate(
    term  = reorder_within(term, beta, topic),
    topic = factor(topic, levels = topic_order_nums)
  )

topic_labels_vem <- c(
  "1"  = "Information Systems",
  "2"  = "Probability",
  "3"  = "Applied Statistics",
  "4"  = "Scientific Computing",
  "5"  = "Calculus",
  "6"  = "Software Engineering",
  "7"  = "Machine Learning",
  "8"  = "Algorithms & Data Structures",
  "9"  = "Linear Algebra",
  "10" = "Business Analytics"
)

terms_plot_10_vem <- ggplot(top_terms_10_vem, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic,
             scales = "free",
             labeller = labeller(topic = topic_labels_vem)) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 30 Words per Topic (10-Topic VEM)", x = NULL, y = "Probability") +
  theme(
    strip.text  = element_text(size = 8),
    plot.title  = element_text(size = 14),
    axis.text.y = element_text(size = 7)
  )

ggsave("clean/images/top_terms_10_topics_vem.png", terms_plot_10_vem, width = 15, height = 10, dpi = 600)

# ============================================================================
# University Topic Distributions (VEM only)
# ============================================================================

universities <- c("Berkeley","Concordia","Laurier","Manitoba","SFU",
                  "Toronto","Waterloo","Western","UBCO")

# make_uni_plot <- function(lda_fit, k, title, topic_names) {
#   uni_topics <- tidy(lda_fit, matrix = "gamma")
#   out <- lapply(universities, function(uni) {
#     regex <- paste0("^", uni)
#     gamma_vals <- vapply(1:k, function(i) {
#       test <- subset(uni_topics, topic == i)
#       idx  <- which(str_detect(test$document, regex))
#       sum(test[idx, "gamma", drop = TRUE])
#     }, numeric(1))
#     if (sum(gamma_vals) > 0) gamma_vals <- gamma_vals / sum(gamma_vals)
#     data.frame(document = rep(uni, k), topic = topic_names, gamma = gamma_vals)
#   })
#   vizdf <- do.call(rbind, out)
#   vizdf <- na.omit(vizdf)
#   
#   ggplot(vizdf %>% mutate(topic = reorder(topic, gamma)),
#          aes(topic, gamma, fill = factor(document))) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~ document, scales = "fixed") +
#     labs(x = "Topic", y = "Document-Topic Probabilities", title = title) +
#     coord_flip()
# }

make_uni_plot <- function(lda_fit, k, title, topic_names, topic_display_order = NULL) {
  uni_topics <- tidy(lda_fit, matrix = "gamma")
  out <- lapply(universities, function(uni) {
    regex <- paste0("^", uni)
    gamma_vals <- vapply(1:k, function(i) {
      test <- subset(uni_topics, topic == i)
      idx  <- which(str_detect(test$document, regex))
      sum(test[idx, "gamma", drop = TRUE])
    }, numeric(1))
    if (sum(gamma_vals) > 0) gamma_vals <- gamma_vals / sum(gamma_vals)
    data.frame(document = rep(uni, k), topic = topic_names, gamma = gamma_vals)
  })
  vizdf <- do.call(rbind, out) |> na.omit()
  
  if (!is.null(topic_display_order)) {
    # Fixed order (e.g., for K = 10) with FIRST item shown at the TOP
    vizdf$topic <- factor(vizdf$topic, levels = topic_display_order)
    
    ggplot(vizdf, aes(topic, gamma, fill = factor(document))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ document, scales = "fixed") +
      labs(x = "Topic", y = "Document-Topic Probabilities", title = title) +
      # key line: reverse the discrete scale so first level appears at the TOP after flipping
      scale_x_discrete(limits = rev(topic_display_order)) +
      coord_flip()
  } else {
    # Data-driven per-panel order (keeps your K = 3 behavior)
    ggplot(vizdf %>% mutate(topic = reorder(topic, gamma)),
           aes(topic, gamma, fill = factor(document))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ document, scales = "fixed") +
      labs(x = "Topic", y = "Document-Topic Probabilities", title = title) +
      coord_flip()
  }
  
}


# k = 3
topic_names_3 <- c("Computer Science","Math & Statistics","Machine Learning")
uni_plot_3_vem <- make_uni_plot(
  lda_fit = lda.out.3.vem, k = 3,
  title = "University Topic Distributions (3-Topic VEM)",
  topic_names = topic_names_3
)
ggsave("clean/images/university_topics_3_vem.png", uni_plot_3_vem, width = 10, height = 6, dpi = 600)

# k = 10
topic_names_10 <- c(
  "1"  = "Information Systems",
  "2"  = "Probability",
  "3"  = "Applied Statistics",
  "4"  = "Scientific Computing",
  "5"  = "Calculus",
  "6"  = "Software Engineering",
  "7"  = "Machine Learning",
  "8"  = "Algorithms & Data Structures",
  "9"  = "Linear Algebra",
  "10" = "Business Analytics"
)

topic_display_order_10 <- c(
  "Machine Learning",
  "Calculus",
  "Linear Algebra",
  "Algorithms & Data Structures",
  "Probability",
  "Applied Statistics",
  "Business Analytics",
  "Software Engineering",
  "Information Systems",
  "Scientific Computing"
)



uni_plot_10_vem <- make_uni_plot(
  lda_fit = lda.out.10.vem, k = 10,
  title = "University Topic Distributions (10-Topic VEM)",
  topic_names = topic_names_10,
  topic_display_order = topic_display_order_10
)

ggsave("clean/images/university_topics_10_vem.png", uni_plot_10_vem, width = 12, height = 6, dpi = 600)

# Done.
