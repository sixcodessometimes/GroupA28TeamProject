# DS276 – CWUR Asia vs Europe Analysis & Visualisation
# Research Question:
# "Is there a significant difference in the mean CWUR score between universities in Asia and Europe?"

# 1. Load dataset
cwur <- read.csv("cwurData.csv")

head(cwur)
str(cwur)

# 2. Create Region variable (Asia vs Europe)
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

cwur$Region <- NA
cwur$Region[cwur$country %in% asia_countries]   <- "Asia"
cwur$Region[cwur$country %in% europe_countries] <- "Europe"

asia_europe <- subset(cwur,
                      Region %in% c("Asia", "Europe") &
                        !is.na(score))

asia_europe$Region <- factor(asia_europe$Region,
                             levels = c("Asia", "Europe"))

table(asia_europe$Region)
head(asia_europe)

# Create filtered dataset for cleaner boxplot (visualisation only)
plot_data <- subset(asia_europe, score < 50)

# 3. Boxplot (visualising the comparison)
png("cwur_boxplot.png", width = 900, height = 1200, res = 150)
par(mar = c(5, 6, 5, 4)) # margin (b, l, t, r)
boxplot(score ~ Region,
        data    = plot_data,
        main    = "CWUR Score by Region (Asia vs Europe)",
        xlab    = "Region",
        ylab    = "CWUR Score (index)",
        col     = c("grey80", "lightgreen"),
        border  = "black",
        notch   = FALSE,
        ylim    = c(42, 50),
        outline = TRUE,
        cex.lab = 1.2,
        cex.axis= 1.1,
        cex.main= 1.3)
dev.off()


# 4. Histogram with normal curve (required supplementary graphic)
# Compute mean and sd
m <- mean(asia_europe$score)
s <- sd(asia_europe$score)

# Export to PNG
png("hist_normal_curve.png", width = 1200, height = 900, res = 150)
par(mar = c(5, 6, 5, 4)) # margin (b, l, t, r)

# Histogram
hist(asia_europe$score,
     breaks = 30,
     freq   = TRUE,
     main   = "Distribution of CWUR Scores (Asia and Europe)",
     xlab   = "CWUR Score (index)",
     ylab   = "Frequency",
     cex.lab = 1.2,
     cex.axis= 1.1,
     cex.main= 1.3)

# Overlay normal curve
x_vals <- seq(min(asia_europe$score), max(asia_europe$score), length = 200)
curve_vals <- dnorm(x_vals, mean = m, sd = s)
curve_vals <- curve_vals * (diff(hist(asia_europe$score, plot = FALSE)$breaks)[1] *
                              length(asia_europe$score))

lines(x_vals, curve_vals, lwd = 2, col = "red")

dev.off()




x_vals <- seq(min(asia_europe$score, na.rm = TRUE),
              max(asia_europe$score, na.rm = TRUE),
              length.out = 200)

lines(x_vals,
      dnorm(x_vals,
            mean = mean(asia_europe$score, na.rm = TRUE),
            sd   = sd(asia_europe$score,   na.rm = TRUE)))

# 5. Summary statistics table
mean_by_region   <- tapply(asia_europe$score, asia_europe$Region, mean,   na.rm = TRUE)
median_by_region <- tapply(asia_europe$score, asia_europe$Region, median, na.rm = TRUE)
sd_by_region     <- tapply(asia_europe$score, asia_europe$Region, sd,     na.rm = TRUE)
min_by_region    <- tapply(asia_europe$score, asia_europe$Region, min,    na.rm = TRUE)
max_by_region    <- tapply(asia_europe$score, asia_europe$Region, max,    na.rm = TRUE)
n_by_region      <- tapply(asia_europe$score, asia_europe$Region, length)

summary_table <- data.frame(
  Region       = names(mean_by_region),
  Mean_Score   = as.numeric(mean_by_region),
  Median_Score = as.numeric(median_by_region),
  SD_Score     = as.numeric(sd_by_region),
  Min_Score    = as.numeric(min_by_region),
  Max_Score    = as.numeric(max_by_region),
  Count        = as.numeric(n_by_region)
)

print(summary_table)

# 6. Normality test
shapiro_result <- shapiro.test(asia_europe$score)
shapiro_result

# 7. Statistical tests
# Independent samples t-test (if distribution approximately normal)
t_test_result <- t.test(score ~ Region, data = asia_europe)
t_test_result

t_test_table <- data.frame(
  Test      = "Independent t-test",
  Statistic = as.numeric(t_test_result$statistic),
  p_value   = t_test_result$p.value
)

t_test_table

# Wilcoxon Rank-Sum test (if non-normal distribution)
wilcox_result <- wilcox.test(score ~ Region, data = asia_europe)
wilcox_result

wilcox_table <- data.frame(
  Test      = "Wilcoxon Rank-Sum",
  Statistic = as.numeric(wilcox_result$statistic),
  p_value   = wilcox_result$p.value
)

print(wilcox_table)

# END OF SCRIPT
