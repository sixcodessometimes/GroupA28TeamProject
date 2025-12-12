
# 7COM1079 Final Report – R Code (RStudio)
# RQ: Is there a difference in the mean CWUR (Center of World University Rankings) Score between universities in Asia and Europe?


# Load required packages
library(readr)
library(dplyr)


# 1. Load data

cwur <- read_csv("cwurData.csv")


# 2. Define region groupings

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

# 3. Create Region variable and filter to Asia/Europe with valid scores
# ---------------------------
asia_europe <- cwur %>%
  mutate(
    Region = case_when(
      country %in% asia_countries   ~ "Asia",
      country %in% europe_countries ~ "Europe",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(
    Region %in% c("Asia", "Europe"),
    !is.na(score)
  )

# Make Region an ordered factor (Asia, then Europe)
asia_europe$Region <- factor(asia_europe$Region,
                             levels = c("Asia", "Europe"))

# Quick check of sample sizes
print(table(asia_europe$Region))

# ---------------------------
# 4. Create outputs folder (if it does not exist)
# ---------------------------
if (!dir.exists("outputs")) dir.create("outputs")


# 5. BOXPLOT: CWUR Score by Region

png("outputs/boxplot.png", width = 1000, height = 1200, res = 150)


par(mar = c(5, 6, 5, 4))

colors <- c("#ff7771", "#00bfc3")

boxplot(score ~ Region,
        data     = asia_europe,
        main     = "University Rankings: CWUR Scores by Region",
        xlab     = "Region",
        ylab     = "Overall CWUR Score (0–100)",
        col      = colors,
        notch    = FALSE,
        ylim     = c(40, 100),
        outline  = TRUE,
        las      = 1,
        cex.lab  = 1.2,
        cex.axis = 1.0,
        cex.main = 1.3,
        boxwex   = 1.0)   # make the boxes wider for clearer quartiles

legend("topright",
       legend = c("Asia", "Europe"),
       fill   = colors)

dev.off()


# 6. HISTOGRAM + NORMAL CURVE OVERLAY (frequency on y-axis)


# Mean and SD for the combined Asia/Europe scores
m <- mean(asia_europe$score)
s <- sd(asia_europe$score)

png("outputs/hist_normal_curve.png", width = 1200, height = 900, res = 150)

par(mar = c(5, 6, 5, 4))

# Histogram with frequency on y-axis
h <- hist(asia_europe$score,
          breaks   = 30,
          freq     = TRUE,  # frequency, NOT density
          main     = "Histogram of CWUR Scores with Normal Curve Overlay",
          xlab     = "Overall CWUR Score (0–100)",
          ylab     = "Frequency",
          col      = "lightblue",
          las      = 1,
          cex.lab  = 1.2,
          cex.axis = 1.0,
          cex.main = 1.3)

# Normal curve scaled to match frequencies
x_vals <- seq(min(asia_europe$score),
              max(asia_europe$score),
              length.out = 200)

y_vals <- dnorm(x_vals, mean = m, sd = s)

# scale density to frequency scale of the histogram
y_vals <- y_vals * diff(h$breaks)[1] * length(asia_europe$score)

lines(x_vals, y_vals, lwd = 2, col = "orange")

dev.off()