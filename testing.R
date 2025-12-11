# ---------------------------
# 7COM1079 Final Report – R Code (RStudio)
# RQ: Is there a difference in the mean CWUR Score between universities in Asia and Europe?
# ---------------------------

# Step 1 – Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Step 2 – Load the dataset (real CWUR CSV)
cwur <- read_csv("cwurData.csv")

# Step 3 – Define region groupings using the "full" lists
# (taken from your first script, matching the actual dataset country names)

asia_countries <- c(
  "Japan", "Israel", "South Korea", "Singapore", "China",
  "Taiwan", "Hong Kong", "Thailand", "Malaysia", "India",
  "Turkey", "Saudi Arabia", "Iran", "Lebanon", "United Arab Emirates"
)

europe_countries <- c(
  "United Kingdom", "Switzerland", "France", "Sweden", "Italy",
  "Germany", "Netherlands", "Finland", "Norway", "Denmark",
  "Belgium", "Spain", "Ireland", "Austria", "Portugal",
  "Czech Republic", "Greece", "Hungary", "Poland", "Iceland",
  "Slovenia", "Estonia", "Croatia", "Slovak Republic",
  "Bulgaria", "Lithuania", "Romania", "Cyprus", "Serbia"
)

# Step 4 – Add Region column and keep only Asia/Europe with real scores
asia_europe <- cwur %>%
  mutate(
    Region = case_when(
      country %in% asia_countries   ~ "Asia",
      country %in% europe_countries ~ "Europe",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(Region %in% c("Asia", "Europe"),
         !is.na(score))

# Make Region an ordered factor (Asia, then Europe)
asia_europe$Region <- factor(asia_europe$Region,
                             levels = c("Asia", "Europe"))

table(asia_europe$Region)

# Step 5 – Create output folder (if it does not exist)
if (!dir.exists("outputs")) dir.create("outputs")

# ---------------------------
# BOXPLOT (keep ggplot version, full score range up to 100)
# ---------------------------
box <- ggplot(asia_europe, aes(x = Region, y = score, fill = Region)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(title = "Boxplot of CWUR Scores by Region (Asia vs Europe)",
       x = "Region",
       y = "CWUR Score (index)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("outputs/boxplot.png", plot = box, width = 7, height = 5)

# ---------------------------
# HISTOGRAM + NORMAL CURVE (use base-R concept from first script)
# ---------------------------

# Compute mean and sd using the cleaned Asia/Europe data
m <- mean(asia_europe$score)
s <- sd(asia_europe$score)

# Export to PNG (inside outputs folder)
png("outputs/hist_normal_curve.png", width = 1200, height = 900, res = 150)
par(mar = c(5, 6, 5, 4))  # margins: bottom, left, top, right

# 1) Histogram of real scores (frequency on y-axis)
h <- hist(asia_europe$score,
          breaks = 30,
          freq   = TRUE,
          main   = "Distribution of CWUR Scores (Asia and Europe)",
          xlab   = "CWUR Score (index)",
          ylab   = "Frequency",
          cex.lab = 1.2,
          cex.axis= 1.1,
          cex.main= 1.3)

# 2) Normal curve scaled to match frequencies
x_vals <- seq(min(asia_europe$score),
              max(asia_europe$score),
              length = 200)

curve_vals <- dnorm(x_vals, mean = m, sd = s)
# scale density to match histogram counts: binwidth * N
bin_width <- diff(h$breaks)[1]
curve_vals <- curve_vals * bin_width * length(asia_europe$score)

lines(x_vals, curve_vals, lwd = 2, col = "red")

dev.off()

# ---------------------------
# OPTIONAL: Wilcoxon test (same data object used by plots)
# ---------------------------
test_used   <- "Mann-Whitney U Test (Wilcoxon rank-sum)"
test_result <- wilcox.test(score ~ Region, data = asia_europe)

sink("outputs/test_results.txt")
cat("Statistical Test Used:", test_used, "\n")
cat("Test Statistic:", test_result$statistic, "\n")
cat("P-value:", test_result$p.value, "\n")
sink()
