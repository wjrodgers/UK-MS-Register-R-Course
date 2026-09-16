# Load any library that you will be using in this script at the top
library(tidyverse)

################################################################################
# file: 3 DataPrep.R
# Revised teaching version based on Jeff Rodgers' original course scripts
#
# This file loads fictional research data, checks it and prepares one row per
# participant for descriptive analysis. These are not UK MS Register records.
# Open Introduction to R.Rproj. This script does not depend on scripts 1 or 2.
################################################################################

# Reference book: Sections 3.3 and 3.4, importing and wrangling data
# https://intro2r.com/importing-data.html
# https://intro2r.com/wrangling-data-frames.html
# Joins and string functions here are course extensions to that reading.

######### Load raw data #########

Participants_raw <- read.csv(file.path("DataIn", "Participants.csv"),
                             na.strings = c("", "NA"),
                             strip.white = TRUE, stringsAsFactors = FALSE)
Scores_raw <- read.csv(file.path("DataIn", "Scores.csv"),
                       na.strings = c("", "NA"),
                       strip.white = TRUE, stringsAsFactors = FALSE)

# Let's look at what we've just loaded into memory
# Participants: one row per person. Scores: one row per assessment record.

dim(Participants_raw)
dim(Scores_raw)
head(Scores_raw)
str(Scores_raw)

######### Check IDs before doing anything else #########

Participants_raw %>% count(UserId) %>% filter(n > 1)
Scores_raw %>% count(UserId) %>% filter(n > 1) %>% head()

# Repeated people in Scores are expected; repeated participant IDs are not.
# stopifnot stops the script if a required check is FALSE.

stopifnot(!anyNA(Participants_raw$UserId),
          anyDuplicated(Participants_raw$UserId) == 0,
          !anyNA(Scores_raw$UserId),
          !anyNA(Scores_raw$RecordId),
          anyDuplicated(Scores_raw$RecordId) == 0)

######### Basic string manipulation #########

Participants_raw %>% count(Site)

# Trim spaces and use consistent case. Check that labels now make sense.

Participants <- Participants_raw %>%
  mutate(Site = str_to_lower(str_trim(Site)),
         Gender = factor(Gender, levels = c("Female", "Male", "PNTS")),
         ms_at_diagnosis = factor(ms_at_diagnosis),
         ms_type_now = factor(ms_type_now),
         YearsSinceDiagnosis = age - age_at_diagnosis,
         AgeOrderReview = age_at_msnow < age_at_diagnosis)

Participants %>% count(Site)

# The study reference date is fixed so results don't change tomorrow.
# Ages are simulated in years at the reference date. Inspect ordering flags.
# The source summary can have age_at_msnow earlier than age_at_diagnosis.
# Keep these values for discussion, rather than inventing corrected ages.
Participants %>% count(AgeOrderReview)

ReferenceDate <- as.Date("2025-12-31")

######### Working with dates and scores #########

# Dates are YYYY-MM-DD. EDSS is an ordinal disability scale from 0 to 10.
# Its valid categories are 0, then 1 to 10 in half steps (there is no 0.5).
# Higher scores indicate greater disability; steps are not equal units.
# These are synthetic EDSS values, not assessments of real people.
# https://mstrust.org.uk/a-z/expanded-disability-status-scale-edss
ValidEDSS <- c(0, seq(1, 10, by = 0.5))

Scores <- Scores_raw %>%
  mutate(CompletedDate = as.Date(CompletedDate, format = "%Y-%m-%d"),
         BadDate = is.na(CompletedDate) | CompletedDate > ReferenceDate,
         BadScore = !is.na(EDSS) & !EDSS %in% ValidEDSS)

Scores %>% count(BadDate, BadScore)
Scores %>% filter(BadDate | BadScore)

# This is a teaching decision: set an invalid EDSS score to NA, keep the row.
# Records without an eligible date cannot be used to identify a latest visit.
# Save flagged records for review rather than silently removing the evidence.

ReviewRecords <- Scores %>% filter(BadDate | BadScore)

Scores <- Scores %>%
  mutate(EDSS = if_else(BadScore, NA_real_, as.numeric(EDSS)))

######### One row per participant #########

# Our question uses the latest dated record on or before ReferenceDate.
# If two records have the same date, take the larger RecordId.
# This tie rule is for this exercise; check the source system in real work.
# Keep a missing latest score. Don't silently substitute an earlier score.

LatestScores <- Scores %>%
  filter(!BadDate) %>%
  group_by(UserId) %>%
  arrange(CompletedDate, RecordId, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(UserId, RecordId, CompletedDate, EDSS)

######### Joining data #########

# First check for score IDs with no participant record
# anti_join shows rows that have no match in the other table.

UnmatchedScores <- Scores %>%
  anti_join(Participants, by = "UserId")
UnmatchedScores

# left_join keeps every participant, including those without an eligible record.
# Using all score records here would repeat participants in the result.

stopifnot(anyDuplicated(LatestScores$UserId) == 0)

AnalysisData <- Participants %>%
  left_join(LatestScores, by = "UserId") %>%
  mutate(HasRecord = !is.na(RecordId))

stopifnot(nrow(AnalysisData) == nrow(Participants),
          anyDuplicated(AnalysisData$UserId) == 0)

AnalysisData %>% count(HasRecord)
AnalysisData %>% summarise(N = n(), MissingScore = sum(is.na(EDSS)))

# An unmatched score is not a reason to add a new person to our cohort.
# Missing score and no eligible assessment record are different things.
# We have not replaced missing values with zero or with a group mean.

######### Let's have a go - Exercise 3 (10 minutes) #########

# How many people are in AnalysisData?
# Which people have no eligible assessment record?
# Which have an eligible record but a missing score?
# Why might dropping every row with any NA remove more people than intended?
# Hint: use filter(), HasRecord and is.na(EDSS).
# Look at AgeOrderReview too. What would you ask the data provider?

######### Exporting the prepared data #########

dir.create("Output", showWarnings = FALSE)
write.csv(AnalysisData, file.path("Output", "AnalysisData.csv"),
          row.names = FALSE, na = "")
write.csv(Scores, file.path("Output", "Scores_clean.csv"),
          row.names = FALSE, na = "")
write.csv(ReviewRecords, file.path("Output", "ReviewRecords.csv"),
          row.names = FALSE, na = "")
write.csv(UnmatchedScores, file.path("Output", "UnmatchedScores.csv"),
          row.names = FALSE, na = "")

write.csv(Participants %>% filter(AgeOrderReview),
          file.path("Output", "AgeOrderReview.csv"), row.names = FALSE, na = "")

# RDS keeps dates and factor levels when we read the data back into R.

saveRDS(AnalysisData, file.path("Output", "AnalysisData.rds"))

######### Optional - Reshaping data (after the core day) #########

# Let's take two fictional measurements per person.
# Wide: one row per person. Long: one row per person per occasion.

Scores_wide <- tibble(UserId = 1:3,
                     Baseline = c(2, 4, 6),
                     FollowUp = c(2.5, NA, 6))

Scores_long <- Scores_wide %>%
  pivot_longer(cols = c(Baseline, FollowUp),
               names_to = "Occasion", values_to = "EDSS")
Scores_long

# Missing follow-up values still represent measurement occasions.

Scores_long %>%
  pivot_wider(names_from = Occasion, values_from = EDSS)
