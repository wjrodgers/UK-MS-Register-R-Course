# Load any library that you will be using in this script at the top
library(tidyverse)

######### Working with data #########

# This script runs from the course project without running script 1 first.
# Today we're using fictional research data that looks similar to data you might
# encounter on the UK MS Register.
# Our question: what do the participants and their recorded EDSS scores look like?

# Reference book: Sections 3.2 to 3.4, data structures, import and wrangling
# https://intro2r.com/data-structures.html
# https://intro2r.com/importing-data.html
# https://intro2r.com/wrangling-data-frames.html

######### Dataframes and tibbles #########

# Each row is one observation; each column is a variable.
# Always ask what a row represents (what is it unique by). 
# Here it is one participant.
# A tibble is a type of dataframe with slightly different printing/subsetting.

participants <- tibble(
  UserId = 1:6,
  Age = c(30, 38, 35, 26, 40, 36),
  Height = c(176, 181, 184, 182, 186, 161)
)
participants
participants$Age
participants[1, 2]
participants[1, ]
participants[, 2]

# $ gives a vector; selecting a tibble column with [ keeps it as a tibble

mean(participants$Age)

######### Importing data #########

# Open the course .Rproj file so R knows where to look.
# file.path() joins folder and file names on Windows, Mac and Linux.
# No need to put your own computer's full path into the script.

getwd()
file.exists(file.path("DataIn", "Participants.csv"))

Participants_raw <- read.csv(file.path("DataIn", "Participants.csv"),
                             na.strings = c("", "NA"),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE)

# Different ways of interrogating an object (your data) in the R environment

dim(Participants_raw)
names(Participants_raw)
head(Participants_raw)
str(Participants_raw)
summary(Participants_raw)
# View(Participants_raw)  # capital V; optional in RStudio

######### Selecting, filtering and mutating #########

# |>is a pipe operator and means "then do"
# Take this data |>
#   do something with it

Participants_raw |>
  select(UserId, Gender, age) |>
  head()

# head() previews six rows so we do not print the whole dataset.
# Select chooses columns; filter chooses rows

Participants_raw |>
  filter(Gender == "Female") |>
  head()

# Missing ages would not pass this filter

Participants_raw |>
  filter(!is.na(age) & age > 45) |>
  head()

# mutate creates or changes a column
# This only prints a result until we assign it to an object.
# The synthetic age columns are in years at the fixed study reference date.

Participants <- Participants_raw |>
  mutate(YearsSinceDiagnosis = age - age_at_diagnosis)

Participants |>
  arrange(age) |>
  head()

######### Working with Factors #########

# Factors store categories. We can choose the order in which they appear.
# Let's check the values before converting them.

Participants |>count(Gender)

Participants <- Participants |>
  mutate(Gender = factor(Gender, levels = c("Female", "Male", "PNTS")))

levels(Participants$Gender)
str(Participants$Gender)

# Careful: as.numeric(a_factor) returns internal category codes.
# It does not recover the numbers printed as category labels.
# Unexpected labels become NA when they are not in the specified levels.

######### Summarising data #########

Participants |>
  count(Gender)

# n() counts rows; sum(!is.na(age)) counts observed ages

Participants |>
  group_by(Gender) |>
  summarise(N = n(),
            N_Age = sum(!is.na(age)),
            M_Age = mean(age, na.rm = TRUE),
            SD_Age = sd(age, na.rm = TRUE),
            .groups = "drop")

# .groups = "drop" leaves an ungrouped result.
# This stops later calculations accidentally being done within groups.

######### Let's have a go - Exercise 2 (10 minutes) #########

# Count the participants at each Region in Participants_raw.
# Select UserId and age for people with a known age greater than 45.
# Calculate the median age and the number of missing ages in Participants.
# Check the 12 Region labels. Why should we inspect them before grouping?

######### Exporting data #########

# Keep the imported file unchanged. Write results to a separate folder.
# Rerunning this code replaces this particular output file.

dir.create("Output", showWarnings = FALSE)
write.csv(Participants, file.path("Output", "Participants_practice.csv"),
          row.names = FALSE, na = "")

# CSV stores values, but not factor levels or other R-specific information.
# Reference book: Section 3.6, Exporting data
# https://intro2r.com/exporting-data.html


######### End-of-module checkpoint (5 minutes) #########
# Work alone for 2 minutes, compare with a partner for 2, then share for 1.
# Count ms_at_diagnosis, then add Percentage = 100 * n / sum(n).
# Check RRMS 56%, SPMS 24%, PPMS 12%, Unknown 8% and a total of 12,000.
# Explain why count() answers this question but head() cannot.
# Exit question: how does select() differ from filter()?
