student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
# Part A
section <- factor(section, levels = c("A", "B"))

students <- data.frame(student_id, section, quiz1, quiz2, passed)

score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

typeof(section)
class(section)
length(section)
str(section)

typeof(students)
class(students)
length(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
length(score_matrix)
str(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

# A vector stores values of one atomic type 
#while a list can store objects of different types 
#A matrix is a two-dimensional atomic vector
#  so all entries share one type. A factor stores categorical data using integer codes and
# levels. A data frame is a list of equal-length columns that may have different
# types.


# Part B
score_matrix["S04", "quiz2"]
score_matrix[1:2, , drop = FALSE]
course_record["course"]
course_record[["course"]]
course_record$course

# [ returns a subset and keeps the surrounding structure, so the first result
# is still a list. [[ extracts one individual element from a list,
#  while $ is a  way to extract a named element using its literal name.

# Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])
students$excellent <- students$average >= 90

section_a_80 <- students[
  students$section == "A" & students$average >= 80,
  c("student_id", "section", "average")
]
section_a_80
student_averages <- students$average
names(student_averages) <- students$student_id
student_averages

# These calculations are vectorized because rowMeans(), >=, &, and data-frame
# subsetting operate on entire vectors or rows at once 

# Problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# Part A
measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)
colSums(is.na(measurements))
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete
removed_sample_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_sample_ids

# x == NA is invalid because comparisons with NA return NA rather than true or
# false The correct test for a missing value is is.na(x).

# Part B
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]
selected_measurements

overall_mean_c <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean_c

south_mean_c <- mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)
south_mean_c
# The transformations and filtering are vectorized because arithmetic,
# comparisons, %in%, complete.cases(), and mean() process whole vectors.

# Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise_result <- A * B
elementwise_result

matrix_result <- A %*% B
matrix_result

dim(elementwise_result)
dim(matrix_result)
# Both results have dimensions 2 x 2.

# A * B multiplies entries in matching positions. A %*% B performs matrix
# multiplication by taking row-by-column dot products.


# Problem 3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# Part A:
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA) # NA
grade_one(90) # "A"
grade_one(80) # "B"
grade_one(85) # "B"
grade_one(74) # "C"

# Part B
grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
grades

# During each loop iteration, i is the position of the current score and the
# matching position where its grade is stored.

# Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  result <- c(
    total = length(x),
    missing = sum(is.na(x)),
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm)
  )
  
  result[c("mean", "sd", "min", "max")] <-
    round(result[c("mean", "sd", "min", "max")], digits)
  
  result
}

summarize_scores(scores)
summarize_scores(x = scores, na.rm = TRUE, digits = 2)
# Part C-2
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# The function plots each nonmissing score against its position in scores.
# Position 3 is missing, so no point is drawn there and the connected line has
# a gap at that position. 
# The ... passes options such as type, pch, labels, and the title from plot_scores() to the base R plot() function.
