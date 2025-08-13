#############################################################
## DGP for Curricla Topic Modeling
## dkrasnov@student.ubc.ca - April 2023
#############################################################
# set.seed(87460945)

##############################
# 0 - Load librairies
##############################
library(igraph)
library(Matrix)
library(sqldf)
library(dplyr)
library(stringr)
library(textclean)

##############################
# 1 - Berkeley DGP
##############################
Degree_pathway_total <- c()
std_sample <- 100
for (i in 1:std_sample) {
  ## Load in course description data
  course_descrips <-
    read.csv("./data/Berkeley/University of California Berkeley Required Courses.csv") %>%
    rename(Course_Code = Course.Code,
           Course_Description = Course.Description)
  
  ds_cal <-
    read.csv(
      "./data/Berkeley/University of California Berkeley Data Science Program Requirements.csv"
    )
  
  
  ## All of (L1):
  Degree_pathway <- ds_cal$Category.Description[c(2, 4, 7)]
  Degree_pathway[1] <- "DATA C8"
  Degree_pathway <-
    c(Degree_pathway, str_squish(sample(unlist(
      str_split(ds_cal$Category.Description[3], "or")
    ), 1)))
  
  
  L5 <- sample(unlist(str_split(ds_cal$Category.Description[5], "or")), 1)
  if(str_squish(L5)=="EECS 16A& EECS 16B"){
    L5 <- unlist(str_split(L5,"&"))
  } 
  Degree_pathway <- c(Degree_pathway,str_squish(L5))
  
  Degree_pathway <-
    c(Degree_pathway, str_squish(sample(unlist(
      str_split(ds_cal$Category.Description[6], "or")
    ), 1)))
  
  
  ## Choose 1 of the following (U2): note I made this DATA C100 because we lack the other course descriptions and the course calendar prefers Data C100
  Degree_pathway <- c(Degree_pathway, "DATA C100")
  
  # Choose two courses comprising 7+ units from the following (U3):
  done <- FALSE
  while (!done) {
    newcourses <- sample(ds_cal$Category.Description[11:44], 2)
    course1_cred <-
      subset(ds_cal, Category.Description == newcourses[1], select = Category.Minimum.Credit.Amount)
    
    course2_cred <-
      subset(ds_cal, Category.Description == newcourses[1], select = Category.Minimum.Credit.Amount)
    
    sprintf("Course 1 Slected: %s worth %d credits",
            newcourses[1],
            as.numeric(course1_cred))
    sprintf("Course 2 Slected: %s worth %d credits",
            newcourses[2],
            as.numeric(course2_cred))
    
    if (course1_cred + course2_cred >= 7) {
      done <- TRUE
    }
    
  }
  
  if("COMPSCI 186 or COMPSCI W186" %in% replace_non_ascii(newcourses)){
    idx <- which(replace_non_ascii(newcourses)== "COMPSCI 186 or COMPSCI W186")
    newcourses[idx] <- sample(str_squish(unlist(str_split(newcourses[idx],"or"))),1)
  }
  
  if("COMPSCI 169 or COMPSCI 169A or COMPSCI W169A" %in% replace_non_ascii(newcourses)){
    idx <- which(replace_non_ascii(newcourses)== "COMPSCI 169 or COMPSCI 169A or COMPSCI W169A")
    newcourses[idx] <- sample(str_squish(unlist(str_split(newcourses[idx],"or"))),1)
  }
  
  if("ECON 140 or ECON 141" %in% replace_non_ascii(newcourses)){
    idx <- which(replace_non_ascii(newcourses)== "ECON 140 or ECON 141")
    newcourses[idx] <- sample(str_squish(unlist(str_split(newcourses[idx],"or"))),1)
  }
  
  
  Degree_pathway <- c(Degree_pathway, newcourses)
  
  ## Choose one of the following (U4):
  newcourses <- sample(ds_cal$Category.Description[46:49], 1)
  if(replace_non_ascii(newcourses) == "DATA/STAT C140"){
    newcourses <- "DATA C140" # we don't have STAT C140
  }
  Degree_pathway <- c(Degree_pathway, str_squish(newcourses))
  
  ## Choose one of the following (U5):
  newcourses <- sample(ds_cal$Category.Description[51:55], 1)
  
  
  if(replace_non_ascii(newcourses)=="DATA/STAT C102"){
    newcourses <- "DATA C102" # We don't have STAT c102
  }
  
  Degree_pathway <- c(Degree_pathway, str_squish(newcourses))
  
  
  
  ## Choose one of the following (U6):
  newcourses <- sample(ds_cal$Category.Description[57:66], 1)
  
  if("AFRICAM 134 or AFRICAM/AMERSTD C134" == replace_non_ascii(newcourses)){
    afrc <- sample(str_squish(unlist(str_split(newcourses,"or"))),1)
    if(afrc == "AFRICAM/AMERSTD C134"){
      afrc <- "AFRICAM C134"
    }
    newcourses <- afrc
  }
  
  if("PB HLTH C160/ESPM C167" == replace_non_ascii(newcourses)){
    newcourses <- "PB HLTH C160" # Don't have ESPM C167
  }
  
  if("DATA C104/HISTORY C184D/STS C104D"== str_squish(replace_non_ascii(newcourses))){
    newcourses <- "DATA C104" # Don't have others
  }
  
  Degree_pathway <- c(Degree_pathway, str_squish(newcourses))
  
  Degree_pathway_total <- c(Degree_pathway_total,Degree_pathway)
 }

Degree_pathway <- data.frame(Course_Code = Degree_pathway)
Degree_pathway$Course_Code <-
  replace_non_ascii(Degree_pathway$Course_Code)
course_descrips$Course_Code <-
  replace_non_ascii(course_descrips$Course_Code)

Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)
Degree_pathway_total$Course_Code <-
  replace_non_ascii(Degree_pathway_total$Course_Code)

query <-
  "SELECT Course_Code,Course_Description FROM Degree_pathway LEFT JOIN course_descrips USING(Course_Code)"
df <- sqldf(query)

query_total <-
  "SELECT Course_Code,Course_Description FROM Degree_pathway_total LEFT JOIN course_descrips USING(Course_Code)"
df_total <- sqldf(query_total)

berkeley_by_course <- df


berkeley <- df
berkeley <-
  data.frame("doc_id" = "Berkeley",
             "text" = str_squish(paste(berkeley$Course_Description, collapse = " ")))

berkeley_total <- df_total
berkeley_total$Course_Code <- paste("Berkeley ",berkeley_total$Course_Code,sep=""," (",1:nrow(berkeley_total),")")

##############################
# 2 - Concordia DGP
##############################
ds_cal <-
  read.csv("./data/Concordia/Concordia University Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

course_descrips_compsci <-
  read.csv("./data/Concordia/Concordia University Computer Science Courses.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_eng <-
  read.csv("./data/Concordia/Concordia University Engineering Courses.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_eng$Course_Code <-
  replace_non_ascii(course_descrips_eng$Course_Code)

course_descrips_mathstat <-
  read.csv("./data/Concordia/Concordia University Mathematics and Statistics Courses.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_mathstat$Course_Code <-
  replace_non_ascii(course_descrips_mathstat$Course_Code)

Degree_pathway_total <- c()
dup_check <- c()
for (i in 1:std_sample) {
  # G1 - G19
  Degree_pathway <- ds_cal$Category_Description[1:19]
  
  # G20
  done <- FALSE
  while(!done){
    newcourses <-
      subset(
        course_descrips_mathstat,
        str_detect(Course_Code, "^[MAST|MATH|STAT]* [3|4]"),
        select = Course_Code
      )
    
    samp <- sample(newcourses$Course_Code, 2)
    
    if(sum(samp %in% Degree_pathway) == 0){
      Degree_pathway <- c(Degree_pathway, samp)
      done <- TRUE
    }
    
  }
  
  # G21
  done <- FALSE
  while (!done) {
    newcourses <-
      subset(course_descrips_compsci,
             str_detect(Course_Code, "^[COMP|SOEN]* [3|4]"),
             select = Course_Code)
    newcourses <- sample(newcourses$Course_Code, 2)
    course1 <-
      subset(course_descrips_compsci,
             Course_Code == newcourses[1],
             select = Credit.Amount)
    course2 <-
      subset(course_descrips_compsci,
             Course_Code == newcourses[2],
             select = Credit.Amount)
    if (course1 + course2 >= 6 & sum(newcourses %in% Degree_pathway)==0) {
      done <- TRUE
    }
  }
  
  Degree_pathway <- c(Degree_pathway, newcourses)
  Degree_pathway_total <- c(Degree_pathway_total, Degree_pathway)
  
}

Degree_pathway <- data.frame(Course_Code = Degree_pathway)
Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_eng USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_mathstat USING(Course_Code)
  "
result <- sqldf(query)
concordia <- result
concordia <-
  data.frame(
    "doc_id" = "Concordia",
    "text" = str_squish(paste(concordia$Course_Description, collapse = " "))
  )

concordia_by_course <- result

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_eng USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_mathstat USING(Course_Code)
  "
result_total <- sqldf(query_total)
concordia_total <- result_total
# concordia_total <-
#   data.frame(
#     "doc_id" = "Concordia",
#     "text" = paste(concordia_total$Course_Description, collapse = " ")
#   )

concordia_total <- result_total
concordia_total$Course_Code <- paste("Concordia ",concordia_total$Course_Code,sep=""," (",1:nrow(concordia_total),")")

##############################
# 3 - Laurier DGP
##############################
ds_cal <-
  read.csv("./data/Laurier/Wilfrid Laurier University Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

ds_spec_cal <-
  read.csv(
    "./data/Laurier/Wilfrid Laurier University Data Science Program Specialization Requirements.csv"
  ) %>%
  rename(Category_Description = Category.Description)
ds_spec_cal$Category_Description <-
  replace_non_ascii(ds_spec_cal$Category_Description)

#Course descrips
course_descrips_bus <-
  read.csv("./data/Laurier/Wilfrid Laurier University Business Course Calendar.csv") %>%
  rename(
    Course_Code = Course.Code,
    Course_Description = Course.Description,
    Credit_Amount = Credit.Amount
  )
course_descrips_bus$Course_Code <-
  replace_non_ascii(course_descrips_bus$Course_Code)

course_descrips_compsci <-
  read.csv("./data/Laurier/Wilfrid Laurier University Computer Science Course Calendar.csv") %>%
  rename(
    Course_Code = Course.Code,
    Course_Description = Course.Description,
    Credit_Amount = Credit.Amount
  )
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_ds <-
  read.csv("./data/Laurier/Wilfrid Laurier University Data Science Course Calendar.csv") %>%
  rename(
    Course_Code = Course.Code,
    Course_Description = Course.Description,
    Credit_Amount = Credit.Amount
  )
course_descrips_ds$Course_Code <-
  replace_non_ascii(course_descrips_ds$Course_Code)

course_descrips_math <-
  read.csv(
    "./data/Laurier/Wilfrid Laurier University Mathematics and Statistics Course Calendar.csv"
  ) %>%
  rename(
    Course_Code = Course.Code,
    Course_Description = Course.Description,
    Credit_Amount = Credit.Amount
  )
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)

Degree_pathway_total <- c()
for (i in 1:std_sample) {
  # G1
  Degree_pathway <-
    str_trim(sample(unlist(
      str_split(ds_cal$Category_Description[1], "or")
    ), 1))
  # G2 - G14, G16 - G18, G20 - G 23
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[c(2:14, 16:18, 20:23)])
  
  # G15
  Degree_pathway <-
    c(Degree_pathway, str_trim(sample(unlist(
      str_split(ds_cal$Category_Description[15], "or")
    ), 1)))
  
  # G19
  Degree_pathway <-
    c(Degree_pathway, str_trim(sample(unlist(
      str_split(ds_cal$Category_Description[19], "or")
    ), 1)))
  
  # G24
  newcourses <-
    subset(course_descrips_bus,
           str_detect(Course_Code, "(^BU3)|(^BU4)"),
           select = Course_Code)
  newcourses <- sample(newcourses$Course_Code, 1)
  
  Degree_pathway <- c(Degree_pathway, newcourses)
  
  # G25 TODO what is meant by senior credit?
  newcourses <-
    subset(course_descrips_compsci,
           str_detect(Course_Code, "(^CP3)|(^CP4)"))$Course_Code
  
  newcourses <-
    c(newcourses,
      subset(
        course_descrips_math,
        str_detect(Course_Code, "(^MA3)|(^MA4)|(^ST3)|(^ST4)")
      )$Course_Code)
  
  newcourses <-
    c(newcourses,
      subset(
        course_descrips_ds,
        str_detect(Course_Code, "(^DATA3)|(^DATA4)")
      )$Course_Code)
  
  newcourses <- data.frame(Course_Code = newcourses)
  
  query <-
    "SELECT Course_Code, Course_Description,Credit_Amount FROM newcourses JOIN course_descrips_compsci USING(Course_Code)
  union
  SELECT Course_Code, Course_Description,Credit_Amount FROM newcourses  JOIN course_descrips_math USING(Course_Code)
  union
  SELECT Course_Code, Course_Description,Credit_Amount FROM newcourses  JOIN course_descrips_ds USING(Course_Code)"
  result <- sqldf(query)
  
  done <- FALSE
  while (!done) {
    courses <- sample(result$Course_Code, 5)
    credits <-
      subset(result, Course_Code %in% courses, select = Credit_Amount)
    if (sum(credits) >= 15 & sum(courses %in% Degree_pathway) == 0) {
      done <- TRUE
      Degree_pathway <- c(Degree_pathway, courses)
    }
  }
  
  # Choose specialization
  spec <-"[a-zA-Z]* S3"
  sample(c("[a-zA-Z]* S1", "[a-zA-Z]* S2", "[a-zA-Z]* S3"), 1)
  newcourses <-
    subset(ds_spec_cal,
           str_detect(Requirement.Category, spec),
           select = Category_Description)
  
  # S1
  if (str_detect(newcourses[1, 1], " Financial Risk Analysis")) {
    
    courses <- newcourses$Category_Description[c(2:5, 7)]
    courses <- courses[!(courses %in% Degree_pathway)]
    
    Degree_pathway <-
      c(Degree_pathway, courses)
    
    samp <- sample(str_trim(unlist(
      str_split(str_sub(
        newcourses$Category_Description[6], 17, 43
      ), ", ")
    )), 1)
    
    while (sum(samp %in% Degree_pathway) != 0) {
      samp <- sample(str_trim(unlist(
        str_split(str_sub(
          newcourses$Category_Description[6], 17, 43
        ), ", ")
      )), 1)
    }
    
    Degree_pathway <-
      c(Degree_pathway, samp)
  }
  
  
  # S2
  if (str_detect(newcourses[1, 1], "Big Data")) {
    courses <- newcourses$Category_Description[2:6]
    courses <- courses[!(courses %in% Degree_pathway)]
    Degree_pathway <-
      c(Degree_pathway, courses)
  }
  
  
  # S3
  if (str_detect(newcourses[1, 1], "Statistical Analysis Concentration")) {
    
    courses <- c(newcourses$Category_Description[2],
                 newcourses$Category_Description[4],
                 newcourses$Category_Description[5])
    courses <- courses[!(courses %in% Degree_pathway)]
    
    Degree_pathway <-
      c(
        Degree_pathway,
        courses
      )
    
    samp <- sample(unlist(str_split(
      str_sub(newcourses$Category_Description[3], 10, 25), " and "
    )), 1)
    
    while(sum(samp %in% Degree_pathway) != 0){
      samp <- sample(unlist(str_split(
        str_sub(newcourses$Category_Description[3], 10, 25), " and "
      )), 1)
    }
    
    Degree_pathway <-
      c(Degree_pathway, samp)
    
    samp <- sample(unlist(
      subset(
        course_descrips_math,
        str_detect(Course_Code, "(^ST3)|(^ST4)")
      )$Course_Code
    ), 1)
    
    
    while(sum(samp %in% Degree_pathway) != 0){
      samp <- sample(unlist(
        subset(
          course_descrips_math,
          str_detect(Course_Code, "(^ST3)|(^ST4)")
        )$Course_Code
      ), 1)
    }
    
    Degree_pathway <-
      c(Degree_pathway, samp)
  }
  
  Degree_pathway_total <- c(Degree_pathway_total,Degree_pathway)
}


Degree_pathway <- data.frame(Course_Code = Degree_pathway)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_bus USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_ds USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  "
result <- sqldf(query)
laurier <- result
laurier <-
  data.frame("doc_id" = "Laurier",
             "text" = str_squish(paste(laurier$Course_Description, collapse = " ")))

laurier_by_course <- result

Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_bus USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_ds USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  "
result_total <- sqldf(query_total)
laurier_total <- result_total
# laurier_total <-
#   data.frame("doc_id" = "Laurier",
#              "text" = paste(laurier_total$Course_Description, collapse = " "))
laurier_total$Course_Code <- paste("Laurier ",laurier_total$Course_Code,sep=""," (",1:nrow(laurier_total),")")



##############################
# 4 - Manitoba DGP
##############################
ds_cal <-
  read.csv("./data/Manitoba/University of Manitoba Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)

ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

#Course descrips
course_descrips_compsci <-
  read.csv("./data/Manitoba/University of Manitoba Computer Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_ds <-
  read.csv("./data/Manitoba/University of Manitoba Data Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_ds$Course_Code <-
  replace_non_ascii(course_descrips_ds$Course_Code)

course_descrips_inter <-
  read.csv(
    "./data/Manitoba/University of Manitoba Interdisciplinary Science Course Calendar.csv"
  ) %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_inter$Course_Code <-
  replace_non_ascii(course_descrips_inter$Course_Code)

course_descrips_math <-
  read.csv("./data/Manitoba/University of Manitoba Mathematics Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)


course_descrips_stat <-
  read.csv("./data/Manitoba/University of Manitoba Statistics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_stat$Course_Code <-
  replace_non_ascii(course_descrips_stat$Course_Code)

Degree_pathway_total <- c()
for (i in 1:std_sample) {
  # F1 - F7
  Degree_pathway <- ds_cal$Category_Description[1:7]
  # NOTE: There are no arts classes scrapped so cant do G1
  
  # S1 - S6
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[10:15])
  
  # U1 - U8
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[17:24])
  
  # C1 - C4 if opted in
  if (sample(c(TRUE, FALSE), 1)) {
    Degree_pathway <-
      c(Degree_pathway, ds_cal$Category_Description[31:34])
  }
  
  # G2
  newcourses <-
    c(
      course_descrips_stat$Course_Code,
      course_descrips_math$Course_Code,
      course_descrips_inter$Course_Code,
      course_descrips_ds$Course_Code
    )
  
  samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                              !(newcourses %in% Degree_pathway)], 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                                !(newcourses %in% Degree_pathway)], 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  
  
  # G3
  
  samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                              !(newcourses %in% Degree_pathway)], 4)
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                                !(newcourses %in% Degree_pathway)], 4)
  }
  
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  
  
  # U9
  samp <- sample(
    subset(
      course_descrips_compsci,
      Course_Code %in% c("COMP 2080", "COMP 2150", "COMP 4510", "COMP 4710")
    )$Course_Code,
    1
  )
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(
      subset(
        course_descrips_compsci,
        Course_Code %in% c("COMP 2080", "COMP 2150", "COMP 4510", "COMP 4710")
      )$Course_Code,
      1
    )
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # U10
  samp <- sample(
    subset(
      course_descrips_math,
      Course_Code %in% c("MATH 2070", "MATH 2080", "MATH 2090", "MATH 2180", "MATH 4370")
    )$Course_Code,
    1
  )
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(
      subset(
        course_descrips_math,
        Course_Code %in% c("MATH 2070", "MATH 2080", "MATH 2090", "MATH 2180", "MATH 4370")
      )$Course_Code,
      1
    )
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # U11
  samp <- sample(
    subset(
      course_descrips_stat,
      Course_Code %in% c(
        "STAT 2300",
        "STAT 2800",
        "STAT 3030",
        "STAT 3550",
        "STAT 3690",
        "STAT 4100",
        "STAT 4150",
        "STAT 4250"
      )
    )$Course_Code,
    1
  )
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(
      subset(
        course_descrips_stat,
        Course_Code %in% c(
          "STAT 2300",
          "STAT 2800",
          "STAT 3030",
          "STAT 3550",
          "STAT 3690",
          "STAT 4100",
          "STAT 4150",
          "STAT 4250"
        )
      )$Course_Code,
      1
    )
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G4 - G5
  samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                              !(newcourses %in% Degree_pathway)], 8)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(newcourses[!(newcourses %in% ds_cal$Category_Description) |
                                !(newcourses %in% Degree_pathway)], 8)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  
  
  Degree_pathway_total <- c(Degree_pathway_total, Degree_pathway)
  
}


Degree_pathway <- data.frame(Course_Code = Degree_pathway)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_ds USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_inter USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_stat USING(Course_Code)"

result <- sqldf(query)
manitoba <- result
manitoba <-
  data.frame("doc_id" = "Manitoba",
             "text" = str_squish(paste(manitoba$Course_Description, collapse = " ")))

manitoba_by_course <- result

Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_ds USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_inter USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_stat USING(Course_Code)"

result_total <- sqldf(query_total)
manitoba_total <- result_total
# manitoba_total <-
#   data.frame("doc_id" = "Manitoba",
#              "text" = paste(manitoba_total$Course_Description, collapse = " "))
manitoba_total$Course_Code <- paste("Manitoba ",manitoba_total$Course_Code,sep=""," (",1:nrow(manitoba_total),")")


##############################
# 5 - SFU DGP
##############################
ds_cal <-
  read.csv("./data/Simon Fraser/Simon Fraser University Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

#Course descrips
course_descrips_bus <-
  read.csv("./data/Simon Fraser/Simon Fraser University Business Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_bus$Course_Code <-
  replace_non_ascii(course_descrips_bus$Course_Code)

course_descrips_compsci <-
  read.csv("./data/Simon Fraser/Simon Fraser University Computer Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_ds <-
  read.csv("./data/Simon Fraser/Simon Fraser University Data Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_ds$Course_Code <-
  replace_non_ascii(course_descrips_ds$Course_Code)

course_descrips_econ <-
  read.csv("./data/Simon Fraser/Simon Fraser University Economics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_econ$Course_Code <-
  replace_non_ascii(course_descrips_econ$Course_Code)

course_descrips_math_and_compsci <-
  read.csv(
    "./data/Simon Fraser/Simon Fraser University Math and Computer Science Course Calendar.csv"
  ) %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math_and_compsci$Course_Code <-
  replace_non_ascii(course_descrips_math_and_compsci$Course_Code)

course_descrips_math <-
  read.csv("./data/Simon Fraser/Simon Fraser University Math Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)

course_descrips_stat <-
  read.csv("./data/Simon Fraser/Simon Fraser University Statistics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_stat$Course_Code <-
  replace_non_ascii(course_descrips_stat$Course_Code)


Degree_pathway_total <- c()
for (i in 1:std_sample) {
  # L1
  Degree_pathway <-
    unlist(strsplit(substr(ds_cal$Category_Description[1], 27, 61), ", "))
  
  # L2
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[2], 27, 74), ", "
    )))
  
  # L3
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[3], 28, 45), ", "
    )))
  
  # L4
  Degree_pathway <-
    c(Degree_pathway,
      substr(ds_cal$Category_Description[4], 20, 27))
  
  # L5
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[5], 27, 64), ", "
    )), 1))
  
  # L6
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[6], 10, 28), ", "
    )))
  
  # L7
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[7], 9, 26), ", "
    )), 1))
  
  # L8
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      str_sub(ds_cal$Category_Description[8], 27, 54), ", "
    )))
  
  # U1
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[9], 9, 55), ", "
    )), 1))
  
  # U2
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[10], 27, 79), ", "
    )))
  
  # U3
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[11], 27, 74), ", "
    )))
  
  # U4
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[12], 27, 44), ", "
    )), 1))
  
  # U5
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[13], 27, 64), ", "
    )), 1))
  
  # U6
  Degree_pathway <-
    c(Degree_pathway, unlist(strsplit(
      substr(ds_cal$Category_Description[14], 10, 27), ", "
    )))
  
  # U7
  Degree_pathway <-
    c(Degree_pathway, sample(unlist(strsplit(
      substr(ds_cal$Category_Description[15], 9, 36), ", "
    )), 1))
  
  Degree_pathway_total <- c(Degree_pathway_total,Degree_pathway)
}


Degree_pathway <- data.frame(Course_Code = Degree_pathway)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_bus USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_ds USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_econ USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math_and_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_stat USING(Course_Code)"

result <- sqldf(query)
sfu <- result
sfu <-
  data.frame("doc_id" = "SFU",
             "text" = str_squish(paste(sfu$Course_Description, collapse = " ")))

sfu_by_course <- result


Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_bus USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_ds USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_econ USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math_and_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_stat USING(Course_Code)"

result_total <- sqldf(query_total)
sfu_total <- result_total
# sfu_total <-
#   data.frame("doc_id" = "SFU",
#              "text" = paste(sfu_total$Course_Description, collapse = " "))
sfu_total$Course_Code <- paste("SFU ",sfu_total$Course_Code,sep=""," (",1:nrow(sfu_total),")")


##############################
# 6 - University of Toronto DGP
##############################
# DS required courses
ds_cal <-
  read.csv("./data/Toronto/University of Toronto Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

#Course descrips
course_descrips_compsci <-
  read.csv("./data/Toronto/University of Toronto Computer Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_ds <-
  read.csv("./data/Toronto/University of Toronto Data Science Required Courses.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_ds$Course_Code <-
  replace_non_ascii(course_descrips_ds$Course_Code)

course_descrips_math <-
  read.csv("./data/Toronto/University of Toronto Mathematics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)

course_descrips_stat <-
  read.csv("./data/Toronto/University of Toronto Statistics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_stat$Course_Code <-
  replace_non_ascii(course_descrips_stat$Course_Code)

Degree_pathway_total <- c()
for (i in 1:std_sample) {# F1
  Degree_pathway <-
    sample(unlist(str_split(ds_cal$Category_Description[1], " or ")), 1)
  
  # F2
  samp <- sample(unlist(
    str_split(ds_cal$Category_Description[2], " or ")
  ), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(
      str_split(ds_cal$Category_Description[2], " or ")
    ), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # F3
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[3])
  
  # F4
  samp <- substr(unlist(str_split(
    sample(unlist(
      str_split(ds_cal$Category_Description[4], " or ")
    ), 1), " and "
  )), 2, 8)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- substr(unlist(str_split(
      sample(unlist(
        str_split(ds_cal$Category_Description[4], " or ")
      ), 1), " and "
    )), 2, 8)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # S1
  samp <-  sample(unlist(
    str_split(ds_cal$Category_Description[7], " or ")
  ), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <-  sample(unlist(
      str_split(ds_cal$Category_Description[7], " or ")
    ), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # S2-S4
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[8:10])
  
  # S5
  course <-
    sample(unlist(str_split(ds_cal$Category_Description[11], " or ")), 1)
  if (str_detect(course, "and")) {
    course <- unlist(str_split(str_sub(course, 3, 23), " and "))
  }
  
  while(sum(course %in% Degree_pathway) != 0){
    course <-
      sample(unlist(str_split(ds_cal$Category_Description[11], " or ")), 1)
    if (str_detect(course, "and")) {
      course <- unlist(str_split(str_sub(course, 3, 23), " and "))
    }
  }
  Degree_pathway <- c(Degree_pathway, course)
  
  # S5A
  courses <- unlist(str_split(
    str_sub(ds_cal$Category_Description[12], 3, 23), " and "
  ))
  
  for (cor in courses) {
    if(!(cor %in% Degree_pathway)){
      Degree_pathway <-
        c(Degree_pathway, cor)
    }
  }
  
  # S5B - U1
  
  if(!(ds_cal$Category_Description[13] %in% Degree_pathway)){
    Degree_pathway <-
      c(Degree_pathway, ds_cal$Category_Description[13])
  }
  if(!(ds_cal$Category_Description[14] %in% Degree_pathway)){
    Degree_pathway <-
      c(Degree_pathway, ds_cal$Category_Description[14])
  }
  if(!(ds_cal$Category_Description[16] %in% Degree_pathway)){
    Degree_pathway <- c(Degree_pathway, ds_cal$Category_Description[16])
  }
  if(!("JSC270H1" %in% Degree_pathway)){
    Degree_pathway <-
      c(Degree_pathway, "JSC270H1")
  }
  
  # U2
  samp <- sample(unlist(str_split(
    str_sub(ds_cal$Category_Description[17], 8, 27), " or "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[17], 8, 27), " or "
    )), 1)
  }
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # U3-U4
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[18:19])
  
  # U5
  samp <- sample(unlist(
    str_split(ds_cal$Category_Description[20], " or ")
  ), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(
      str_split(ds_cal$Category_Description[20], " or ")
    ), 1)
  }
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # U6 - U8
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[21:22], "JSC370H1")
  
  # U9
  samp <- sample(unlist(
    str_split(ds_cal$Category_Description[24], " or ")
  ), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(
      str_split(ds_cal$Category_Description[24], " or ")
    ), 1)
  }
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # General Category 1 (4 courses at least 2 courses at 400-level)
  courses <-
    c(unlist(
      str_extract_all(
        ds_cal$Category_Description[25],
        "\\b[A-Z]{3}[0-9]{3}[A-Z]?[0-9]?[HY]?1?\\b"
      )
    ),
    subset(course_descrips_stat, str_detect(Course_Code, "^STA4"))$Course_Code)
  courses <- unique(courses)
  
  course_samp <- sample(courses, 4)
  
  while(sum(str_detect(course_samp, "[A-Z]*4")) < 2 & sum(course_samp %in% Degree_pathway)!=0){
    course_samp <- sample(courses, 4)
  }
  
  Degree_pathway <- c(Degree_pathway, course_samp)
  Degree_pathway <- unique(Degree_pathway)
  
  # General Category 2
  if (!("STA130H1" %in% Degree_pathway)) {
    Degree_pathway <-
      c(Degree_pathway, sample(subset(
        course_descrips_stat,
        str_detect(Course_Code, "(^STA4)|(^STA3)")
      )$Course_Code, 1))
  }
  Degree_pathway <- unique(Degree_pathway)
  Degree_pathway_total <- c(Degree_pathway_total, Degree_pathway)
}


Degree_pathway <- data.frame(Course_Code = Degree_pathway)

Degree_pathway$Course_Code <- replace_non_ascii(Degree_pathway$Course_Code)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway  JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway  JOIN course_descrips_ds USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_stat USING(Course_Code)"

result <- sqldf(query)
toronto <- result[!duplicated(result$Course_Code), ]

toronto <-
  data.frame("doc_id" = "Toronto",
             "text" = str_squish(paste(toronto$Course_Description, collapse = " ")))

toronto_by_course <- result


Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total  JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total  JOIN course_descrips_ds USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_stat USING(Course_Code)"

result_total <- sqldf(query_total)
toronto_total <- result_total[which(!duplicated(result_total$Course_Code)), ]
# toronto_total <-
#   data.frame("doc_id" = "Toronto",
#              "text" = paste(toronto_total$Course_Description, collapse = " "))
toronto_total$Course_Code <- paste("Toronto ",toronto_total$Course_Code,sep=""," (",1:nrow(toronto_total),")")

##############################
# 7 - Waterloo DGP
##############################
# DS required courses
ds_cal <-
  read.csv("./data/Waterloo/University of Waterloo Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

# STAT required courses
stat_cal <-
  read.csv("./data/Waterloo/University of Waterloo Statistics Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
stat_cal$Category_Description <-
  replace_non_ascii(stat_cal$Category_Description)

#Course descrips
course_descrips_app_math <-
  read.csv("./data/Waterloo/University of Waterloo Applied Mathematics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_app_math$Course_Code <-
  replace_non_ascii(course_descrips_app_math$Course_Code)

course_descrips_compsci <-
  read.csv("./data/Waterloo/University of Waterloo Computer Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)


course_descrips_english <-
  read.csv("./data/Waterloo/University of Waterloo English Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_english$Course_Code <-
  replace_non_ascii(course_descrips_english$Course_Code)

course_descrips_math <-
  read.csv("./data/Waterloo/University of Waterloo Mathematics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)

course_descrips_math_elec <-
  read.csv("./data/Waterloo/University of Waterloo Mathematics Electives Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math_elec$Course_Code <-
  replace_non_ascii(course_descrips_math_elec$Course_Code)


course_descrips_stat <-
  read.csv("./data/Waterloo/University of Waterloo Statistics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_stat$Course_Code <-
  replace_non_ascii(course_descrips_stat$Course_Code)

Degree_pathway_total <- c()
for (i in 1:std_sample) {# DS Courses
  # G1
  Degree_pathway <-
    sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[1], 9, 22), ", "
    )), 1)
  
  # G2
  samp <- sample(unlist(str_split(
    str_sub(ds_cal$Category_Description[2], 9, 26), ", "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[2], 9, 26), ", "
    )), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G3
  Degree_pathway <-
    c(Degree_pathway, unlist(str_split(
      str_sub(ds_cal$Category_Description[3], 9, 81), ", "
    )))
  
  # G4
  samp <- sample(unlist(str_split(
    str_sub(ds_cal$Category_Description[4], 9, 22), ", "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[4], 9, 22), ", "
    )), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G5
  samp <- sample(unlist(str_split(
    str_sub(ds_cal$Category_Description[5], 9, 40), ", "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[5], 9, 40), ", "
    )), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G6
  samp <- sample(unlist(str_split(
    str_sub(ds_cal$Category_Description[6], 49, 90), ", "
  )), 2)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(ds_cal$Category_Description[6], 49, 90), ", "
    )), 2)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # Stat courses
  
  # G1
  samp <- sample(unlist(str_split(
    str_sub(stat_cal$Category_Description[1], 9, 26), ", "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(stat_cal$Category_Description[1], 9, 26), ", "
    )), 1)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G2
  Degree_pathway <-
    c(Degree_pathway, unlist(str_split(
      str_sub(stat_cal$Category_Description[2], 9, 56), ", "
    )))
  
  # G3
  samp <- sample(unlist(str_split(
    str_sub(stat_cal$Category_Description[3], 9, 89), ", "
  )), 1)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(unlist(str_split(
      str_sub(stat_cal$Category_Description[3], 9, 89), ", "
    )), 1)
  }
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G4
  samp <- sample(subset(
    course_descrips_stat, str_detect(Course_Code, "(^STAT) 4")
  )$Course_Code, 2)
  
  while(sum(samp %in% Degree_pathway) != 0){
    samp <- sample(subset(
      course_descrips_stat, str_detect(Course_Code, "(^STAT) 4")
    )$Course_Code, 2)
  }
  
  Degree_pathway <-
    c(Degree_pathway, samp)
  
  # G5
  done <- FALSE
  while (!done) {
    course <-
      sample(subset(
        course_descrips_stat,
        str_detect(Course_Code, "((^STAT) 4)|((^STAT) 3)")
      )$Course_Code, 1)
    if (!(course %in% Degree_pathway)) {
      done <- TRUE
    }
  }
  Degree_pathway <- c(Degree_pathway, course)
  
  # G6
  course <- sample(c("CS 457", "CS 485", "CS 486", "STAT"), 1)
  if (course == "STAT") {
    done <- FALSE
    while (!done) {
      course <-
        sample(subset(
          course_descrips_stat,
          str_detect(Course_Code, "(^STAT) 4")
        )$Course_Code,
        1)
      if (!(course %in% Degree_pathway)) {
        done <- TRUE
      }
    }
  }
  
  while(sum(course %in% Degree_pathway) != 0){
    course <- sample(c("CS 457", "CS 485", "CS 486", "STAT"), 1)
    if (course == "STAT") {
      done <- FALSE
      while (!done) {
        course <-
          sample(subset(
            course_descrips_stat,
            str_detect(Course_Code, "(^STAT) 4")
          )$Course_Code,
          1)
        if (!(course %in% Degree_pathway)) {
          done <- TRUE
        }
      }
    }
  }
  
  Degree_pathway <- c(Degree_pathway, course)
  
  # G7/G8
  
  done <- FALSE
  while (!done) {
    courses <-
      subset(
        rbind(
          course_descrips_app_math,
          course_descrips_math,
          course_descrips_stat
        ),
        str_detect(
          Course_Code,
          "((^MATH) 4)|((^MATH) 3)|((^AMATH) 3)|((^AMATH) 4)|((^STAT) 4)|((^STAT) 3)"
        )
      )$Course_Code
    
    courses <- sample(courses, 7)
    
    if (sum(courses %in% Degree_pathway) == 0) {
      done <- TRUE
    }
  }
  
  Degree_pathway <- c(Degree_pathway, courses)
  Degree_pathway_total <- c(Degree_pathway_total,Degree_pathway)
  
}


Degree_pathway <- data.frame("Course_Code" = Degree_pathway)

query <- "
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_app_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_english USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math_elec USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_stat USING(Course_Code)"

result <- sqldf(query)
waterloo <- result
waterloo <-
  data.frame("doc_id" = "Waterloo",
             "text" = str_squish(paste(waterloo$Course_Description, collapse = " ")))

waterloo_by_course <- result

Degree_pathway_total <- data.frame("Course_Code" = Degree_pathway_total)

query_total <- "
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_app_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_english USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math_elec USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_stat USING(Course_Code)"

result_total <- sqldf(query_total)
waterloo_total <- result_total
# waterloo_total <-
#   data.frame("doc_id" = "Waterloo",
#              "text" = paste(waterloo_total$Course_Description, collapse = " "))
waterloo_total$Course_Code <- paste("Waterloo ",waterloo_total$Course_Code,sep=""," (",1:nrow(waterloo_total),")")

##############################
# 8 - Western DGP
##############################
# DS required courses
ds_cal <-
  read.csv("./data/Western/University of Western Ontario Data Science Program Requirements.csv") %>%
  rename(Category_Description = Category.Description)
ds_cal$Category_Description <-
  replace_non_ascii(ds_cal$Category_Description)

#Course descrips
course_descrips_compsci <-
  read.csv("./data/Western/University of Western Ontario Computer Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_compsci$Course_Code <-
  replace_non_ascii(course_descrips_compsci$Course_Code)

course_descrips_ds <-
  read.csv("./data/Western/University of Western Ontario Data Science Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_ds$Course_Code <-
  replace_non_ascii(course_descrips_ds$Course_Code)

course_descrips_math <-
  read.csv("./data/Western/University of Western Ontario Mathematics Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_math$Course_Code <-
  replace_non_ascii(course_descrips_math$Course_Code)

course_descrips_soft_eng <-
  read.csv(
    "./data/Western/University of Western Ontario Software Engineering Course Calendar.csv"
  ) %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_soft_eng$Course_Code <-
  replace_non_ascii(course_descrips_soft_eng$Course_Code)

course_descrips_stat <-
  read.csv(
    "./data/Western/University of Western Ontario Statistical Sciences Course Calendar.csv"
  ) %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)
course_descrips_stat$Course_Code <-
  replace_non_ascii(course_descrips_stat$Course_Code)

Degree_pathway_total <- c()
for (i in 1:std_sample) {
  # G1
  Degree_pathway <-
    unlist(str_split(ds_cal$Category_Description[1], ", "))
  Degree_pathway[1] <- "Data Science 2000A/B"
  
  # G2
  Degree_pathway <-
    c(Degree_pathway,
      substr(ds_cal$Category_Description[2], 18, 37),
      unlist(str_split(
        substr(ds_cal$Category_Description[2], 152, 1000), ", "
      )))
  Degree_pathway[13] <- "Computer Science 3340A/B"
  
  # G2F (G2A - G2E contain duplicates)
  Degree_pathway <-
    c(Degree_pathway, ds_cal$Category_Description[8])
  
  # G3
  Degree_pathway <-
    c(Degree_pathway, unlist(str_split(
      substr(ds_cal$Category_Description[9], 17, 82), ", "
    )))
  
  Degree_pathway_total <- c(Degree_pathway_total,Degree_pathway)  
  
}


Degree_pathway <- data.frame(Course_Code = Degree_pathway)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_compsci USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_ds USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_math USING(Course_Code)
  UNION
  SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_stat USING(Course_Code)"

result <- sqldf(query)
western <- result
western <-
  data.frame("doc_id" = "Western",
             "text" = str_squish(paste(western$Course_Description, collapse = " ")))

western_by_course <- result

Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_compsci USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_ds USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_math USING(Course_Code)
  UNION ALL
  SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_stat USING(Course_Code)"

result_total <- sqldf(query_total)
western_total <- result_total
# western_total <-
#   data.frame("doc_id" = "Western",
#              "text" = paste(western_total$Course_Description, collapse = " "))
western_total$Course_Code <- paste("Western ",western_total$Course_Code,sep=""," (",1:nrow(western_total),")")

##############################
# 9 - UBCO DGP
##############################
course_descrips_UBCO <-
  read.csv("./data/UBCO/UBCO Course Calendar.csv") %>%
  rename(Course_Code = Course.Code,
         Course_Description = Course.Description)

Degree_pathway_total <- c()
for (i in 1:std_sample) {
  
  
  # First Year
  Degree_pathway <- c("DATA 101")
  Degree_pathway <-
    c(Degree_pathway, sample(c("CHEM 111", "CHEM 121"), 1))
  Degree_pathway <- c(Degree_pathway, "MATH 100", "MATH 101")
  
  x <- sample(c(TRUE, FALSE), 1)
  if (x) {
    Degree_pathway <- c(Degree_pathway, "ENGL 109")
  } else{
    Degree_pathway <-
      c(Degree_pathway, sample(
        c(
          "ENGL 112",
          "ENGL 113",
          "ENGL 114",
          "ENGL 150",
          "ENGL 151",
          "ENGL 153",
          "ENGL 154",
          "ENGL 155",
          "ENGL 156"
        ),
        2
      ))
  }
  
  Degree_pathway <-
    c(Degree_pathway, sample(c("PHYS 111", "PHYS 112"), 1))
  Degree_pathway <-
    c(Degree_pathway, sample(c("PHYS 121", "PHYS 122"), 1))
  Degree_pathway <- c(Degree_pathway, "COSC 111", "COSC 121")
  
  # Second Year
  Degree_pathway <-
    c(Degree_pathway,
      "MATH 200",
      "MATH 221",
      "STAT 230",
      "COSC 221",
      "COSC 222")
  
  # Third and Fourth Year
  Degree_pathway <-
    c(Degree_pathway,
      "DATA 301",
      "DATA 311",
      "COSC 304",
      "STAT 303",
      "PHIL 331")
  
  upper_year_data <-
    c("DATA 310", "DATA 315", "DATA 405", "DATA 407", "DATA 410")
  max_2_stat <- c("STAT 400", "STAT 401", "STAT 403", "STAT 406")
  max_2_cosc_math_phys <-
    c(
      "COSC 303",
      "COSC 322",
      "COSC 329",
      "COSC 344",
      "COSC 407",
      "COSC 421",
      "MATH 303",
      "MATH 307",
      "MATH 409",
      "PHYS 420"
    )
  course <- c()
  while(sum(unique(course) %in% Degree_pathway)!=0){
    while (length(unique(course)) < 9) {
      var <- sample(c("1", "2", "3"), 1)
      
      if (var == 1) {
        course <- c(course, sample(max_2_stat, 1))
        
      } else if (var == 2) {
        course <- c(course, sample(max_2_cosc_math_phys, 1))
        
      } else if (var == 3) {
        course <- c(course, sample(upper_year_data, 1))
      }
    }
  }
  Degree_pathway <- c(Degree_pathway, unique(course))
  Degree_pathway_total <- c(Degree_pathway_total, Degree_pathway)
}

Degree_pathway <- data.frame(Course_Code = Degree_pathway)

query <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway JOIN course_descrips_UBCO USING(Course_Code)"
result <- sqldf(query)
ubco <- result
ubco <-
  data.frame("doc_id" = "UBCO",
             "text" = str_squish(paste(ubco$Course_Description, collapse = " ")))

ubco_by_course <- result

Degree_pathway_total <- data.frame(Course_Code = Degree_pathway_total)

query_total <-
  "SELECT Course_Code, Course_Description FROM Degree_pathway_total JOIN course_descrips_UBCO USING(Course_Code)"
result_total <- sqldf(query_total)
ubco_total <- result_total
# ubco_total <-
#   data.frame("doc_id" = "UBCO",
#              "text" = paste(ubco_total$Course_Description, collapse = " "))
ubco_total$Course_Code <- paste("UBCO ",ubco_total$Course_Code,sep=""," (",1:nrow(ubco_total),")")


##############################
# 10 - Save Corpus Objects
##############################


degree_corpus <-
  rbind(western,
        waterloo,
        toronto,
        sfu,
        manitoba,
        laurier,
        concordia,
        berkeley,
        ubco)

# save(degree_corpus,file="./data/RObjects/degree_corpus.RData")

degree_corpus_by_course <-
  rbind(
    western_by_course,
    waterloo_by_course,
    toronto_by_course,
    sfu_by_course,
    manitoba_by_course,
    laurier_by_course,
    concordia_by_course,
    berkeley_by_course,
    ubco_by_course
  )

# save(degree_corpus_by_course, file = "./data/RObjects/degree_corpus_by_course.RData")


degree_corpus_total <- rbind(
  western_total,
  waterloo_total,
  toronto_total,
  sfu_total,
  manitoba_total,
  laurier_total,
  concordia_total,
  berkeley_total,
  ubco_total
)

# save(degree_corpus_total,file="./data/RObjects/degree_corpus_total.RData")
