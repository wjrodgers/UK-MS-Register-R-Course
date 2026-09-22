######### Introduction to R and RStudio #########

# What is R and RStudio?
# R does the calculations. RStudio helps us write, run and organise our code.
# Open Introduction to R.Rproj before starting this course.
# Let's find the Script, Console, Environment, Files, Plots and Help panes.

# Write code in the script and save it. Run a line with Ctrl + Enter on Windows
# or Cmd + Enter on a Mac. The answer appears in the Console.
# A comment starts with #. R does not run comments.
# Run one section at a time today, rather than the whole script at once.

# Reference book: Chapter 1, Getting started with R and RStudio
# https://intro2r.com/chap1.html
# Section 2.3, Using functions in R
# https://intro2r.com/using-functions-in-r.html

######### R as a calculator #########

3 + 4
8 - 2
5 * 2
3/4
2^4
sqrt(9)

# Order of operations - brackets change what gets calculated first

2 + 5 * 10
(2 + 5) * 10

######### Assigning variables #########

# To store a value in an object, use <-
# Let's calculate someone's height in metres

height <- 176
height_in_m <- height / 100
height_in_m

# R is case sensitive: height and Height would be different objects.
# Use names that will make sense when you come back to your work.

# Logical comparisons (TRUE or FALSE)
# <- stores a result. == asks whether two things are equal.

height == 176
height > 180
height != 176

######### Vectors #########

# Create a vector with the c function to combine elements
# A vector holds values of the same type

age <- c(30, 38, 35, 26, 40, 36)
age
length(age)

participant_id <- 1:6
participant_id

# We can also store text and logical values
# Text needs quotation marks. TRUE and FALSE do not.

group <- c("Group A", "Group B", "Group A", "Group B", "Group A", "Group B")
age >= 35

# R works on each value in the vector

age + 1

######### Basic functions #########

# A function does a job. Its arguments go inside the brackets.

mean(age)
sd(age)
median(age)
range(age)
sum(age)

######### Getting help in R (5 minutes, including practice) #########

# You do not need to memorise every function or its arguments.
# Use ? followed by a function name, or help() with its name in quotes.
# Use the name without calling it: ?mean rather than mean(age).
# Run either line below to open the documentation in RStudio's Help pane.

?mean
help("mean")

# How to read a help page:
# Description: what does the function do?
# Usage: how do you call it, and what are the default arguments?
# Arguments: what can you supply or change? Find na.rm here.
# Value: what does the function return?
# Examples: small pieces of code you can copy, run and adapt.
# Some pages describe several related methods. For mean, look at the default
# method to find na.rm. You do not need to understand the whole page today.

# Let's have a go (2 minutes within this section):
# Open help("sd"). Find the default value of na.rm.
# Predict sd(c(2, NA, 4)), then use the help page to calculate the SD of the
# observed values. Explain to a partner which argument you changed.

# If you don't know the exact function name, search installed documentation.
# Try this in the Console: ??"standard deviation"
# Later, for a package function, you can be specific:
# help("filter", package = "dplyr")
# The package must be installed. This avoids confusing dplyr's filter with
# another function of the same name.

######### Sub-setting/indexing #########

# R starts counting at 1

age[1]
age[c(2, 4)]

# A negative size removed the position
age[-1]

# Logical subsetting: TRUE = keep, FALSE = exclude

age > 35
age[age > 35]

# & = and, | = or

age[(age >= 30) & (age < 40)]
age[(age < 30) | (age >= 40)]

######### Missing values #########

# NA = a missing value. It is not the same as zero.

score <- c(2, 6, NA, 10)
mean(score)
is.na(score)
sum(is.na(score))

# Calculate the mean of the observed values
# na.rm = TRUE does not change the original vector or fill in its missing value

mean(score, na.rm = TRUE)
score

# Don't use score == NA to find missing values. Use is.na().
# ! means not

score[!is.na(score)]

######### Let's have a go - Exercise 1 (10 minutes) #########

# Create a vector called waiting_days with the values 7, 14, NA, 21 and 28.
# How many values are missing?
# What is the mean of the observed waiting times?
# Select the observed waiting times that are greater than 14 days.
# Hint: combine !is.na(waiting_days) with your condition using &.
# Add a comment explaining what na.rm = TRUE does.

######### When something goes wrong #########

# Try mean(Age) in the Console. Why does R say object 'Age' not found?
# Check the spelling and capitals, then check that you ran the assignment.
# If the Console shows + instead of >, R is waiting for more code.
# Check for an unclosed bracket or quote. Esc cancels an unfinished command.
# Save your script often if you don't want to lose your work

######### End-of-module checkpoint (5 minutes) #########
# Work alone for 2 minutes, compare with a partner for 2, then share for 1.
# Without copying the example, make a vector c(10, NA, 20, 30).
# Predict mean() with and without na.rm = TRUE, then run both.
# Can you explain to your partner the difference between removing NA from 
# a calculation and imputation.

