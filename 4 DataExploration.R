# Load any library that you will be using in this script at the top
library(tidyverse)

################################################################################
# file: 4 DataExploration.R
# Revised teaching version based on the original course work by Jeff Rodgers,
# Sarah Knowles, Elaine Craig and Rod Middleton
#
# Let's describe the fictional cohort, then look at its latest recorded EDSS scores.
# The unit of analysis is now a participant, not a assessment record.
################################################################################

# Reference book: Section 3.5, summarising; Section 4.2, base R plots
# https://intro2r.com/summarising-data-frames.html
# https://intro2r.com/simple-base-r-plots.html
# Section 5.2 introduces ggplot's data, mapping and geometry
# https://intro2r.com/the-start-of-the-end.html

# Rebuild the prepared data from the raw files every time.
# source runs another script. This makes script 4 work in a fresh R session
# and avoids accidentally using an old output after the cleaning code changes.
# During teaching, explain this line rather than repeating the whole lesson.

source("3 DataPrep.R")
AnalysisData <- readRDS(file.path("Output", "AnalysisData.rds"))

######### First check what we have #########

dim(AnalysisData)
str(AnalysisData)
head(AnalysisData)

######### Categorical data #########

# Different ways of looking at frequency data

table(AnalysisData$ms_type_now, useNA = "ifany")

GroupCounts <- AnalysisData %>%
  count(ms_type_now) %>%
  mutate(Percentage = round(100 * n/sum(n), 1))
GroupCounts

# The denominator above is all participants, not just those with a score.

AnalysisData %>% count(ms_type_now, HasRecord)

######### Numeric data and missing values #########

ScoreSummary <- AnalysisData %>%
  summarise(N = n(),
            N_Score = sum(!is.na(EDSS)),
            MissingScore = sum(is.na(EDSS)),
            M_Score = mean(EDSS, na.rm = TRUE),
            SD_Score = sd(EDSS, na.rm = TRUE),
            Median_Score = median(EDSS, na.rm = TRUE),
            Q1 = quantile(EDSS, 0.25, na.rm = TRUE),
            Q3 = quantile(EDSS, 0.75, na.rm = TRUE),
            IQR_Score = IQR(EDSS, na.rm = TRUE))
ScoreSummary

# Q1 and Q3 are quartile endpoints; IQR is Q3 minus Q1.
# na.rm = TRUE describes the available values; it does not solve missing data.
# With no observed values a mean is NaN; with fewer than two an SD is NA.

######### Visualise the data #########

# Let's start with a histogram. What does it show that the mean doesn't?

hist(AnalysisData$EDSS,
     main = "Latest recorded EDSS",
     xlab = "Synthetic EDSS (0-10; higher = greater disability)",
     col = "lightblue")

# EDSS is ordinal; half-point steps are not equally spaced disability units.
# Report counts and median/quartiles. The mean is included to check the
# requested simulation pattern, not to turn EDSS into a continuous measure.
# A normality test is not a decision rule for interpreting this scale.

######### Comparing groups #########

GroupSummary <- AnalysisData %>%
  group_by(ms_type_now) %>%
  summarise(N = n(),
            N_Score = sum(!is.na(EDSS)),
            MissingScore = sum(is.na(EDSS)),
            Mean_EDSS = mean(EDSS, na.rm = TRUE),
            Median_Score = median(EDSS, na.rm = TRUE),
            Q1 = quantile(EDSS, 0.25, na.rm = TRUE),
            Q3 = quantile(EDSS, 0.75, na.rm = TRUE),
            .groups = "drop")
GroupSummary

# Explicitly select observed scores for this plot.
# The table above tells us how many are missing in each group.

PlotData <- AnalysisData %>% filter(!is.na(EDSS))

# ggplot: data, mapping with aes(), then a geometry (what to draw).
# + adds a plot layer. It is different from the data pipe %>%.

ScorePlot <- ggplot(PlotData, aes(x = ms_type_now, y = EDSS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.15, size = 0.8,
             position = position_jitter(width = 0.15, height = 0, seed = 1)) +
  labs(title = "Latest recorded EDSS by current MS type",
       subtitle = "Fictional teaching data; each point is one participant",
       x = "Current MS type", y = "EDSS (0-10)") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_minimal()
print(ScorePlot)

# We hide the boxplot's separate outlier symbols because all points are drawn.
# We have not removed unusual values from the data.
# PPMS and RRMS were deliberately assigned higher mean EDSS in this simulation.
# This is a requested teaching contrast, not evidence about real MS populations.
# These groups are MS categories, not randomly assigned treatment groups.

######### Saving results #########

write.csv(GroupSummary, file.path("Output", "GroupSummary.csv"),
          row.names = FALSE, na = "")
ggsave(file.path("Output", "Score_by_group.png"), plot = ScorePlot,
       width = 7, height = 4.5, units = "in", dpi = 150)

######### Let's have a go - Exercise 4 (15 minutes) #########

# Describe the latest scores by Site rather than ms_type_now.
# Include the total number of people, the number with a score and the median.
# Change the plot to show Site and give it a useful title.
# Save your plot to Output with a different filename.
# Write two sentences describing the result, including the missing values.
# What would you need to know before explaining a difference between sites?
# Optional: recreate the supplied age summary using group_by(Gender,
# ms_at_diagnosis, ms_type_now) and mean()/sd() for each of the three ages.

######### Can someone else rerun your analysis? #########

# Save the script. Restart R (Session > Restart R), then run this script again.
# The tables and saved plot should be the same.
# The raw data, cleaning choices and scripts are all part of the analysis.

writeLines(capture.output(sessionInfo()), file.path("Output", "sessionInfo.txt"))

######### Optional - Exploring two numeric variables #########

AgeScoreData <- AnalysisData %>% filter(!is.na(age) & !is.na(EDSS))

ggplot(AgeScoreData, aes(x = age, y = EDSS)) +
  geom_point() +
  labs(x = "Synthetic age in 2025 (years)",
       y = "EDSS (0-10)",
       title = "Age and latest recorded EDSS") +
  theme_minimal()

# Describe what you see. Association alone does not establish cause.
