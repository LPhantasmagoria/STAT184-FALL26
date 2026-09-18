# Setup Code for Problem 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# Problem 1: Part A
# Convert section to a factor with levels: A, B
section <- factor(section, levels = c("A", "B"))

# Create a data frame named students with all five vectors
students <- data.frame(student_id, section, quiz1, quiz2, passed)

# Create score_matrix with students as rows, quizzes as columns,
# Student IDs as row names, and quiz 1&2 as column names
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id

# Create course_record, a list, with
# course = R Programming; scores = students; and
# a vector cutoff = c(pass = 70, excellent = 90)
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoff = c(pass = 70, excellent = 90)
)

# Problem 1: Part B
# Extract S02's second quiz score from score_matrix
score_matrix["S02", "quiz2"]

# Extract the first two matrix rows while preserving matrix dimensions
score_matrix[1:2, , drop = FALSE]

# Extract course from course record using [,[[, and $
course_record["course"]
course_record[["course"]]
course_record$course
# [ returns a list
# [[ returns the element
# $ returns the named element

# Problem 1: Part C; NO LOOPING
# Add average, the row-wise mean of the two quizzes to students
students$average <- (students$quiz1 + students$quiz2) / 2

# Add excellent, equal to True when average >=90
students$excellent <- students$average >= 90

# Select section A students whose average is at least 80
sub = subset(students, section == "A" & average >= 80)

# Return student_id, section, and average for that subset
sub[, c("student_id", "section", "average")]

# Create a named numeric vector of all student averages
# Using student_id as names
num_vector <- c(students$average)
names(num_vector) <- c(students$student_id)

# Briefly explain why your calculations are vectorized
# Because we are manipulating vectors or whole entire columns/rows.

# PROBLEM 2
# PROBLEM 2 SETUP
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# Problem 2 Part A
# Use read.csv() to create measurements.
# Treat blank fields and the text NA as missing
measurements <- read.csv(text = csv_text, na.strings = c(" ", "NA"))

# inspect with head(), str(), dim(), and names()
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Count missing values in each column
sum(is.na(measurements))

# Create measurements_complete using complete.case()
measurements_complete <- measurements[complete.cases(measurements), ]

# Report the sample IDs removed by the complete-case filter()
measurements$sample_id[!complete.cases(measurements)]

# Explain why x == NA is not a valid missing-value test
# You can't use x == NA, you have to use is.na(x)
# because na has no value.

# PROBLEM 2: PART B (NO LOOPIING)

# Convert site and status to factors and report their level
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

# Create temp_f
# temp_f = temp_c * 9 / 5 + 32
temp_f = measurements$temp_c * 9 / 5 + 32

# Create ph_below_7, preserving missing pH values
ph_below_7 <- measurements$ph < 7

# Select complete observations from North to South
# whose status is "ok"
ok <- measurements_complete[
  (measurements_complete$site == "North" |
  measurements_complete$site == "South") &
  measurements_complete$status == "ok",]

# return only sample_id, site, temp_c, temp_f, and ph
ok$temp_f <- ok$temp_c * 9 / 5 + 32
ok <- ok[, c("sample_id", "site", "temp_c",
          "temp_f", "ph")]

# Calculate the overall mean Celsius temp 
# and the South-site mean. Ignore missing values.
mean(measurements$temp_c, na.rm = TRUE)

# PROBLEM 2: PART C
# SETUP
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

# Compute A*B and A%*%B
# Report the dimensions of both results.
# Explain how element-wise differs from matrix multiplication.
dim(A*B)
dim(A%*%B)
# Element wise, the two matrix only multiply the two numbers at the index.
# Matrix Multiplication multiplies rows by columns

# PROBLEM 3
# SETUP
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# PROBLEM 3: PART A
# SETUP
# Use if, else if and else to return grades A-F
# Return NA_character_ when score is missing
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60){
  if (is.na(score)) {print("NA_character_")}
  else if (score >= 90) {print("A")}
  else if (score >= 80) {print("B")}
  else if (score >= 70) {print("C")}
  else if (score >= 60) {print("D")}
  else {print("F")} }

# Testing
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# PROBLEM 3: PART B
# Create a vector grades of NA_character with the same length as scores.
# Use for loop with seq_along(scores) to call grade_one(scores[i])
# Store result in grades[i]
# After loop, assign student_id as the names of grades.
# Display vector and explain i in loop.

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades
# i in the loop is the index that allows you
# to call the scores in grades 

# PROBLEM 3: PART C-1
# SETUP
summarize_scores <- function(x, na.rm = TRUE, digits = 1)
  {summ_scores <- c(
    total_count = length(x),
    missing_count = sum(is.na(x)),
    mean = round((mean(x, na.rm = na.rm)), digits),
    sd = round((sd(x, na.rm = na.rm)), digits),
    minimum = round((min(x, na.rm = na.rm)), digits),
    maximum = round((max(x, na.rm = na.rm)), digits))
  return (summ_scores)}

# Call once with default and once with named argument.
summarize_scores(scores)
summarize_scores(x = scores, na.rm = TRUE, digits = 1)

# Testing
summarize_scores( x = scores, na.rm = TRUE, digits = 2 )

# PROBLEM 3: PART C-2
# Write plot_scores <- functions(x,...)
plot_scores <- function(x, ...)
  {plot(x, ...)}

# Testing
plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )

# Predictions and explanations
# Created a plot from scores.
# type = "b" refers to both points and lines are shown
# pch stands for plotting character; 19 specifies it to be a closed circle.
# xlab labels the x axis
# ylab labels the y axis
# main labels the title
# The ellipsis argument is used for alluding many arguments.