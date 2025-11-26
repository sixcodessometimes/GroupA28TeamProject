cwur <- read.csv("cwurData.csv")

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

cwur_subset <- cwur[ , c("world_rank", "institution", "country",
                         "publications", "score", "year")]

head(cwur_subset, 5)

score_mean <- mean(cwur$score, na.rm = TRUE)
score_sd   <- sd(cwur$score, na.rm = TRUE)

plot(cwur$publications, cwur$score,
     pch = 16,
     col = "#00000025",
     cex = 0.6,
     main = "Scatterplot of Score vs Publications",
     xlab = "Publications",
     ylab = "Score",
     frame.plot = TRUE)
grid(col = "lightgray", lty = "dotted")
abline(lm(score ~ publications, data = cwur),
       col = "red",
       lwd = 3)

png("outputs/slide4_scatter_score_publications.png",
    width = 900, height = 650)
plot(cwur$publications, cwur$score,
     pch = 16,
     col = "#00000025",
     cex = 0.6,
     main = "Scatterplot of Score vs Publications",
     xlab = "Publications",
     ylab = "Score",
     frame.plot = TRUE)
grid(col = "lightgray", lty = "dotted")
abline(lm(score ~ publications, data = cwur),
       col = "red",
       lwd = 3)
dev.off()

hist(cwur$score,
     breaks = 40,
     freq   = FALSE,
     main   = "Histogram of Score with Normal Curve",
     xlab   = "Score")
curve(dnorm(x, mean = score_mean, sd = score_sd),
      add = TRUE, col = "blue", lwd = 2)

png("outputs/slide5_hist_score.png", width = 800, height = 600)
hist(cwur$score,
     breaks = 30,
     freq   = FALSE,
     main   = "Histogram of Score with Normal Curve",
     xlab   = "Score")
curve(dnorm(x, mean = score_mean, sd = score_sd),
      add = TRUE, col = "blue", lwd = 2)
dev.off()

set.seed(123)
score_sample <- sample(cwur$score, 500)
shapiro.test(score_sample)

cor_test_spearman <- cor.test(cwur$score,
                              cwur$publications,
                              method = "spearman",
                              use    = "complete.obs")

cor_test_pearson <- cor.test(cwur$score,
                             cwur$publications,
                             method = "pearson",
                             use    = "complete.obs")

sink("outputs/slide6_correlation_results.txt")
print(shapiro.test(score_sample))
print(cor_test_spearman)
print(cor_test_pearson)
sink()

spearman_rho <- cor_test_spearman$estimate
spearman_p   <- cor_test_spearman$p.value
pearson_r    <- cor_test_pearson$estimate
pearson_p    <- cor_test_pearson$p.value

spearman_rho
spearman_p
pearson_r
pearson_p
