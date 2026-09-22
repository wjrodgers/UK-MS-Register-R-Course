# Load libraries at the top. Open Introduction to R.Rproj before running.
library(tidyverse)

################################################################################
# 3 DataPrep.R - Cleaning sparse registry-style data (75 minutes)
# Fictional teaching data, NOT a UKMSR extract.
# Aim: inspect missingness, validate dates/ages, document corrections/exclusions,
# then create one row per eligible participant without discarding all sparse rows.
# Run short sections and inspect results. No dependency on scripts 1 or 2.
# Reading: https://intro2r.com/wrangling-data-frames.html
################################################################################

######### 1. Import and inspect: what does a row represent? #########
Participants_raw <- read.csv("DataIn/Participants.csv", na.strings = c("", "NA"),
                             stringsAsFactors = FALSE)
Scores_raw <- read.csv("DataIn/Scores.csv", na.strings = c("", "NA"),
                       colClasses = c(CompletedDate = "character"),
                       stringsAsFactors = FALSE)
dim(Participants_raw)
dim(Scores_raw)
head(Participants_raw)
head(Scores_raw)
# Participant row = one person; score row = one assessment. Repeated UserIds
# in Scores are expected. Duplicate participant or record IDs need resolution.
stopifnot(!anyNA(Participants_raw$UserId), anyDuplicated(Participants_raw$UserId) == 0,
          !anyNA(Scores_raw$UserId), !anyNA(Scores_raw$RecordId),
          anyDuplicated(Scores_raw$RecordId) == 0)

######### 2. Sparse data: describe before deciding #########
MissingParticipants <- Participants_raw %>%
  summarise(across(everything(), ~ sum(is.na(.x))))
MissingScores <- Scores_raw %>%
  summarise(across(everything(), ~ sum(is.na(.x))))
MissingParticipants
MissingScores
Scores_raw %>% count(MissingDate = is.na(CompletedDate), MissingEDSS = is.na(EDSS))
# Missing is not zero. A person without an onset age can still contribute to
# EDSS summaries. Do not use na.omit() or drop_na() on the entire dataset.
# Ask: what information is actually required for THIS analysis?

######### 3. Strings and ages #########
Participants_raw %>% count(Region)
Participants_checked <- Participants_raw %>%
  mutate(Region = str_squish(Region),
         Gender = factor(tolower(Gender), levels = c("female", "male", "pnts")),
         ms_at_diagnosis = factor(ms_at_diagnosis),
         ms_type_now = factor(ms_type_now),
         YearsSinceDiagnosis = age - age_at_diagnosis,
         MissingOnset = is.na(age_at_onset),
         OnsetAfterDiagnosis = !is.na(age_at_onset) & !is.na(age_at_diagnosis) &
                               age_at_onset > age_at_diagnosis,
         BadAge = (!is.na(age) & (age < 0 | age > 115)) |
                  (!is.na(age_at_onset) & age_at_onset < 0) |
                  (!is.na(age_at_diagnosis) & age_at_diagnosis < 0) |
                  (!is.na(age_at_onset) & !is.na(age) & age_at_onset > age) |
                  (!is.na(age_at_diagnosis) & !is.na(age) & age_at_diagnosis > age),
         AgeOrderReview = !is.na(age_at_msnow) & !is.na(age_at_diagnosis) &
                          age_at_msnow < age_at_diagnosis,
         ExclusionReason = case_when(
           OnsetAfterDiagnosis & BadAge ~ "Onset after diagnosis; implausible age",
           OnsetAfterDiagnosis ~ "Onset after diagnosis",
           BadAge ~ "Implausible age", TRUE ~ NA_character_))
Participants_checked %>% count(Region)
Participants_checked %>% count(MissingOnset, OnsetAfterDiagnosis)
# age_at_onset means symptom onset; age_at_msnow is a separate source field.
# Never relabel one as the other. The latter's ordering remains a review issue.
# Missing onset cannot establish whether ordering is valid; retain and flag it.
# Our exercise excludes illogical age records from this analysis, without
# deleting source rows. In a real project query the source and agree rules first.
ExcludedParticipants <- Participants_checked %>% filter(!is.na(ExclusionReason))
Participants <- Participants_checked %>% filter(is.na(ExclusionReason))
ExcludedParticipants %>% select(UserId, age_at_onset, age_at_diagnosis, ExclusionReason)

######### 4. Date text, calendar validity and plausible range #########
# A parseable date can still be implausible: 1900 and 1025 are real years!
# This fictional extract covers assessments in 2025 ONLY. These limits belong
# to this exercise; do not apply them indiscriminately to UKMSR historical data.
EarliestDate <- as.Date("2025-01-01")
ReferenceDate <- as.Date("2025-12-31")

# Look at the range of Dates
Scores_raw |> reframe(range_CompletedDate = range(CompletedDate,na.rm = TRUE))

Scores <- Scores_raw %>%
  rename(CompletedDate = CompletedDate, EDSS_raw = EDSS) %>%
  mutate(DateCorrected = if_else(CompletedDate == '1025-01-15','2025-01-15',CompletedDate),
         MissingDate = is.na(CompletedDate),
         DateBeforeWindow = !is.na(CompletedDate) & CompletedDate < EarliestDate,
         DateAfterWindow = !is.na(CompletedDate) & CompletedDate > ReferenceDate,
         DateIssue = case_when(MissingDate ~ "Missing date",
                               DateBeforeWindow ~ "Before extract window",
                               DateAfterWindow ~ "After extract window",
                               TRUE ~ "Eligible date"),
         BadDate = DateIssue != "Eligible date",
         BadScore = !is.na(EDSS_raw) & !EDSS_raw %in% c(0, seq(1, 10, 0.5)),
         EDSS = if_else(BadScore, NA_real_, as.numeric(EDSS_raw)))
Scores %>% count(DateIssue)
Scores %>% filter(UserId %in% c(11, 14, 16, 18, 20)) %>%
  select(UserId, CompletedDate, DateCorrected, DateIssue)
# Invalid EDSS becomes NA; retain its original value and flag. A missing date
# prevents chronological selection, not all possible uses of that assessment.
ReviewRecords <- Scores %>% filter(BadDate | BadScore)
ExcludedAssessments <- Scores %>% filter(BadDate)

######### 5. Keep separate exclusion reasons and select a latest record #########
# An unmatched ID differs from a known participant excluded by an age rule.
UnmatchedScores <- Scores %>% anti_join(Participants_checked, by = "UserId")
ScoresForExcludedParticipants <- Scores %>% semi_join(ExcludedParticipants, by = "UserId")
LatestScores <- Scores %>%
  semi_join(Participants, by = "UserId") %>%
  filter(!BadDate) %>%
  group_by(UserId) %>%
  arrange(CompletedDate, RecordId, .by_group = TRUE) %>%
  slice_tail(n = 1) %>% ungroup() %>%
  select(UserId, RecordId, CompletedDate, EDSS)
# The later RecordId breaks a date tie for this exercise. Do not replace a
# missing latest EDSS with an earlier nonmissing score: that changes the question.
stopifnot(anyDuplicated(LatestScores$UserId) == 0)
AnalysisData <- Participants %>%
  left_join(LatestScores, by = "UserId") %>%
  mutate(HasRecord = !is.na(RecordId))
AnalysisData %>% count(HasRecord, MissingEDSS = is.na(EDSS))
stopifnot(nrow(AnalysisData) == nrow(Participants),
          nrow(Participants) + nrow(ExcludedParticipants) == nrow(Participants_raw))

######### 6. Audit the effect of cleaning #########
CleaningFlow <- tibble(
  Stage = c("Raw participants", "Excluded by age rules", "Eligible participants",
            "Eligible participants with a dated assessment", "With observed latest EDSS"),
  N = c(nrow(Participants_raw), nrow(ExcludedParticipants), nrow(AnalysisData),
        sum(AnalysisData$HasRecord), sum(!is.na(AnalysisData$EDSS))))
CleaningFlow
# Diagnosis percentages and region counts refer to the original 12000 people;
# exclusions can change their distribution in the analysis cohort.
# Report exclusions and available denominators, not just the final complete rows.

######### Let's have a go - Exercise 3 (10 minutes) #########
# 1. Count missing onset ages, dates and scores. Are these the same denominator?
# 2. Trace IDs 14, 16, 18 and 20. Which can be corrected, and what is the evidence?
# 3. Find onset-after-diagnosis records. Show their IDs and exclusion reasons.
# 4. Explain why ID 23 stays despite a missing onset age.
# 5. Compare nrow(AnalysisData) with sum(complete.cases(AnalysisData)). Why can
#    complete.cases remove far too many rows, especially with audit columns?

######### Export clean data AND an audit trail #########
dir.create("Output", showWarnings = FALSE)
write.csv(AnalysisData, "Output/AnalysisData.csv", row.names = FALSE, na = "")
saveRDS(AnalysisData, "Output/AnalysisData.rds")
write.csv(Scores, "Output/Scores_clean.csv", row.names = FALSE, na = "")
write.csv(ReviewRecords, "Output/ReviewRecords.csv", row.names = FALSE, na = "")
write.csv(UnmatchedScores, "Output/UnmatchedScores.csv", row.names = FALSE, na = "")
write.csv(ExcludedParticipants, "Output/ExcludedParticipants.csv", row.names = FALSE, na = "")
write.csv(ExcludedAssessments, "Output/ExcludedAssessments.csv", row.names = FALSE, na = "")
write.csv(ScoresForExcludedParticipants, "Output/ScoresForExcludedParticipants.csv", row.names = FALSE, na = "")
write.csv(CleaningFlow, "Output/CleaningFlow.csv", row.names = FALSE)
write.csv(Participants_checked %>% filter(AgeOrderReview),
          "Output/AgeOrderReview.csv", row.names = FALSE, na = "")

######### End-of-module checkpoint (5 minutes) #########
# Alone for 2 minutes, pairs for 2, then share for 1:
# Classify a missing onset age, a 1900 assessment date, a confirmed year typo
# and onset after diagnosis as retain, correct or exclude for this analysis.
# Explain which record level you exclude: participant or assessment.
# Exit question: what evidence and denominator changes must you report?
