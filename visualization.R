# ---------------------------
# 7COM1079 Final Report – R Code (RStudio)
# RQ: Is there a difference in the mean CWUR Score between universities in Asia and Europe?
# ---------------------------

# Step 1 – Load required libraries (all part of tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Step 2 – Load the dataset
cwur <- read_csv("cwurData.csv")

# Step 3 – Define region groupings
asia_countries <- c("China", "India", "Japan", "South Korea", "Singapore", "Israel",
                    "Turkey", "Iran", "Saudi Arabia", "Taiwan", "Malaysia", "Indonesia",
                    "Thailand", "Pakistan")

europe_countries <- c("United Kingdom", "Germany", "France", "Italy", "Spain", "Netherlands",
                      "Sweden", "Switzerland", "Belgium", "Denmark", "Finland", "Austria",
                      "Norway", "Poland", "Greece")

# Step 4 – Add Region column and filter only Asia and Europe
cwur <- cwur %>%
  mutate(Region = case_when(
    country %in% asia_countries   ~ "Asia",
    country %in% europe_countries ~ "Europe",
    TRUE                          ~ "Other"
  )) %>%
  filter(Region %in% c("Asia", "Europe"))

# Step 5 – Create output folder
if (!dir.exists("outputs")) dir.create("outputs")

# Step 6 – Boxplot: CWUR Score by Region (white background)
box <- ggplot(cwur, aes(x = Region, y = score, fill = Region)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(title = "Boxplot of CWUR Scores by Region",
       x = "Region",
       y = "CWUR Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("outputs/boxplot.png", plot = box, width = 7, height = 5)



# Step 7 – Histogram with normal curve overlay (FREQUENCY + overlay)

# Compute values needed to scale the normal curve to frequencies
mean_score <- mean(cwur$score)
sd_score   <- sd(cwur$score)
n_scores   <- nrow(cwur)
bins       <- 30
binwidth   <- (max(cwur$score) - min(cwur$score)) / bins

hist <- ggplot(cwur, aes(x = score)) +
  # Frequency histogram (counts on y-axis)
  geom_histogram(bins = bins,
                 fill = "skyblue",
                 color = "white") +
  # Normal curve scaled to match frequencies: density * n * binwidth
  stat_function(
    fun = function(x) dnorm(x, mean = mean_score, sd = sd_score) * n_scores * binwidth,
    color = "orange",
    linewidth = 1.2
  ) +
  labs(title = "Histogram of CWUR Scores (Asia & Europe) with Normal Curve",
       x = "CWUR Score",
       y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("outputs/histogram.png", plot = hist, width = 7, height = 5)

# Step 8 – Mann-Whitney U test (Wilcoxon rank-sum) for difference between regions
# Chosen because the histogram + normal curve suggest non-normal distribution.
test_used   <- "Mann-Whitney U Test (Wilcoxon rank-sum)"
test_result <- wilcox.test(score ~ Region, data = cwur)

# Step 9 – Save test results to text file
sink("outputs/test_results.txt")
cat("Statistical Test Used:", test_used, "\n")
cat("Test Statistic:", test_result$statistic, "\n")
cat("P-value:", test_result$p.value, "\n")
sink()
