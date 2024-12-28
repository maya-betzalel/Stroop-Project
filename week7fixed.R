# R course for beginners
# Week 7
# assignment by Maya Betzalel, 211823836

#### Load Libraries ----
library(dplyr)
library(ggplot2)

#### Read Files ----

# Directory of extracted files
unzip_dir <- "C:/Users/maya3/OneDrive/Desktop/R/unzipped_data"

# List all files in the directory
files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)  # Get all files in the directory

# Read all files and merge into a single DataFrame
df <- do.call(rbind, lapply(files, read.csv))

# Initial check of the data
print(head(df))  # Display the first rows

#### Data Processing ----

# This script preforms data processing for Stroop Analysis
# Written by: Maya Betzalel
# Last update: 28-12-2024

df <- df |>
  mutate(
    congruency = as.factor(condition),  # Use the 'condition' column as 'congruency'
    acc = ifelse(participant_response == correct_response, 1, 0),  # Compute accuracy
    subject = as.factor(subject),
    task = as.factor(condition),  # Use 'condition' to create 'task'
    block = as.numeric(block),
    trial = as.numeric(trial),
    acc = as.numeric(acc),
    rt = as.numeric(rt)
  ) |>
  select(subject, block, trial, task, congruency, participant_response, correct_response, acc, rt)  # Keep relevant columns only

# Save the raw data
save(df, file = "C:/Users/maya3/OneDrive/Desktop/R/test.rdata")

#### Filter ----

# Load the raw data
load("C:/Users/maya3/OneDrive/Desktop/R/test.rdata")

# Remove missing values
df <- df |> filter(!is.na(rt))

# Remove outliers in reaction times (RT)
df <- df |> filter(rt >= 300 & rt <= 3000)

# Save the filtered data
save(df, file = "C:/Users/maya3/OneDrive/Desktop/R/filtered_data.rdata")

#### Descriptive Statistics ----

# This script generates descriptives statistics for Stroop Analysis
# Written by: Maya Betzalel
# Last update: 28-12-2024

# Compute means and standard deviations for each condition
descriptive_stats <- df |> 
  group_by(task, congruency) |> 
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),         # Mean reaction time
    sd_rt = sd(rt, na.rm = TRUE),             # Standard deviation of reaction time
    mean_acc = mean(acc, na.rm = TRUE),       # Mean accuracy
    sd_acc = sd(acc, na.rm = TRUE)            # Standard deviation of accuracy
  ) |> 
  ungroup()

# Print the summary table
print(descriptive_stats)

#### Plots ----

# This script creates plots for Stroop Analysis
# Written by: Maya Betzalel
# Last update: 28-12-2024

# Plot mean reaction times (RT) by task and congruency
ggplot(descriptive_stats, aes(x = congruency, y = mean_rt, fill = task)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_rt - sd_rt, ymax = mean_rt + sd_rt), 
                position = position_dodge(0.9), width = 0.25) +
  labs(
    title = "Mean Reaction Times by Task and Congruency",
    x = "Congruency",
    y = "Mean Reaction Time (ms)",
    fill = "Task"
  ) +
  theme_minimal()

# Plot mean accuracy by task and congruency
ggplot(descriptive_stats, aes(x = congruency, y = mean_acc, fill = task)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_acc - sd_acc, ymax = mean_acc + sd_acc), 
                position = position_dodge(0.9), width = 0.25) +
  labs(
    title = "Mean Accuracy by Task and Congruency",
    x = "Congruency",
    y = "Mean Accuracy",
    fill = "Task"
  ) +
  theme_minimal()


