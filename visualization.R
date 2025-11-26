cwur <- read.csv("cwurData.csv")

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

cwur_subset <- cwur[ , c("world_rank", "institution", "country",
                         "publications", "score", "year")]

head(cwur_subset, 5)
nrow(cwur)

score_mean <- mean(cwur$score, na.rm = TRUE)
score_sd   <- sd(cwur$score, na.rm = TRUE)

plot(cwur$publications, cwur$score,
     main = "Scatterplot of Score vs Publications",
     xlab = "Publications",
     ylab = "Score")
abline(lm(score ~ publications, data = cwur),
       col = "red", lwd = 2)

png("outputs/slide4_scatter_score_publications.png",
    width = 900, height = 650)
plot(cwur$publications, cwur$score,
     main = "Scatterplot of Score vs Publications",
     xlab = "Publications",
     ylab = "Score")
abline(lm(score ~ publications, data = cwur),
       col = "red", lwd = 2)
dev.off()

hist(cwur$score,
     breaks = 30,
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
shapiro_result <- shapiro.test(score_sample)

cor_test_spearman <- cor.test(cwur$score,
                              cwur$publications,
                              method = "spearman",
                              use    = "complete.obs")

spearman_rho <- cor_test_spearman$estimate
spearman_p   <- cor_test_spearman$p.value

spearman_rho
spearman_p

sink("outputs/slide6_correlation_results.txt")
print(shapiro_result)
print(cor_test_spearman)
sink()
