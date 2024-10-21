getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Homework3")
dataset <- read.csv('SVMtrain.csv')

colnames(dataset)
summary(dataset)

#Task 1, confidence intervals for 1%, 5% and 50% samples
z <- qnorm(1 - 0.025) #95% confidence interval z-value

#1% sample
threshold_1 <- max(dataset$PassengerId) * 0.01
sample_1 <- dataset[dataset$PassengerId <= threshold_1, ]

#numerical variable is age
mean_age_1 <- mean(sample_1$Age)
sd_age_1 <- sd(sample_1$Age)
se_age_1 <- sd_age_1 / sqrt(nrow(sample_1))
moe_age_1 <- z * se_age_1
ci_lower_age_1 <- mean_age_1 - moe_age_1
ci_upper_age_1 <- mean_age_1 + moe_age_1
#categorical variable: sex (proportion of females)
p_hat_sex_1 <- mean(sample_1$Sex == 'female')
se_sex_1 <- sqrt((p_hat_sex_1 * (1 - p_hat_sex_1)) / nrow(sample_1))
moe_sex_1 <- z * se_sex_1
ci_lower_sex_1 <- p_hat_sex_1 - moe_sex_1
ci_upper_sex_1 <- p_hat_sex_1 + moe_sex_1
cat("1% Sample - Age CI: [", ci_lower_age_1, ",", ci_upper_age_1, "]\n")
cat("1% Sample - Female Proportion CI: [", ci_lower_sex_1, ",", ci_upper_sex_1, "]\n")

#5% sample
thresold_5 <- max(dataset$PassengerId) * 0.05
sample_5 <- dataset[dataset$PassengerId <= thresold_5, ]

#numerical variable: age
mean_age_5 <- mean(sample_5$Age)
sd_age_5 <- sd(sample_5$Age)
se_age_5 <- sd_age_5 / sqrt(nrow(sample_5))
moe_age_5 <- z * se_age_5
ci_lower_age_5 <- mean_age_5 - moe_age_5
ci_upper_age_5 <- mean_age_5 + moe_age_5
#categorical variable: sex (proportion of females)
p_hat_sex_5 <- mean(sample_5$Sex == 'female')
se_sex_5 <- sqrt((p_hat_sex_5 * (1 - p_hat_sex_5)) / nrow(sample_5))
moe_sex_5 <- z * se_sex_5
ci_lower_sex_5 <- p_hat_sex_5 - moe_sex_5
ci_upper_sex_5 <- p_hat_sex_5 + moe_sex_5
cat("5% Sample - Age CI: [", ci_lower_age_5, ",", ci_upper_age_5, "]\n")
cat("5% Sample - Female Proportion CI: [", ci_lower_sex_5, ",", ci_upper_sex_5, "]\n")

#50% sample
threshold_50 <- max(dataset$PassengerId) * 0.50
sample_50 <- dataset[dataset$PassengerId <= threshold_50, ]

mean_age_50 <- mean(sample_50$Age)
sd_age_50 <- sd(sample_50$Age)
se_age_50 <- sd_age_50 / sqrt(nrow(sample_50))
moe_age_50 <- z * se_age_50
ci_lower_age_50 <- mean_age_50 - moe_age_50
ci_upper_age_50 <- mean_age_50 + moe_age_50
p_hat_sex_50 <- mean(sample_50$Sex == 'female')
se_sex_50 <- sqrt((p_hat_sex_50 * (1 - p_hat_sex_50)) / nrow(sample_50))
moe_sex_50 <- z * se_sex_50
ci_lower_sex_50 <- p_hat_sex_50 - moe_sex_50
ci_upper_sex_50 <- p_hat_sex_50 + moe_sex_50
cat("50% Sample - Age CI: [", ci_lower_age_50, ",", ci_upper_age_50, "]\n")
cat("50% Sample - Female Proportion CI: [", ci_lower_sex_50, ",", ci_upper_sex_50, "]\n")
#Confidence intervals for age:
#1% Sample: [21.37, 46.63]
#5% Sample: [29.45, 41.10]
#50% Sample: [33.20, 36.50]
#Confidence Interval for female proportion (sex is categorical variable)
#1% Sample: [0.04, 0.71]
#5% Sample: [0.35, 0.65]
#50% Sample: [0.34, 0.43]
cat("1% Sample - Age MOE:", moe_age_1, "\n")
cat("1% Sample - Female Proportion MOE:", moe_sex_1, "\n")
cat("5% Sample - Age MOE:", moe_age_5, "\n")
cat("5% Sample - Female Proportion MOE:", moe_sex_5, "\n")
cat("50% Sample - Age MOE:", moe_age_50, "\n")
cat("50% Sample - Female Proportion MOE:", moe_sex_50, "\n")
#Age MOE, goes from 12.63 at 1% sample to 5.82 at 5% sample to 1.65 at 50% sample
#Female proportion MOE goes from 0.34 at 1% sample to 0.15 at 5% sample to0.045 at 50% sample
#Key Takeaway: Margin of error decreases as sample size increases. This aligns with theory that a larger sample size provides more precise results. 
#WE calculated confidence intervals for mean o age and confidence intervals of specific value of sex (female)

#Task 2: smaller MER
new_mer_age <- moe_age_5 / 2 #50% smaller margin of error for age
new_mer_sex <- moe_sex_5 / 2 #50% smaller margin of error for female proportion

#Age
z_age_new <- new_mer_age / se_age_5
confidence_level_age <- 2 * pnorm(z_age_new) - 1

#Female proportion 
z_sex_new <- new_mer_sex / se_sex_5
confidence_level_sex <- 2 * pnorm(z_sex_new) - 1

cat("New Confidence Level for Age (with smaller MER):", confidence_level_age * 100, "%\n")
cat("New Confidence Level for Female Proportion (with smaller MER):", confidence_level_sex * 100, "%\n")

#Sample size needed to achieve new margin of error with 90% confidence
#For age
z_90 <- qnorm(1 - 0.05)
n_age_90 <- (z_90 * sd_age_5 / new_mer_age)^2

#For female proportion
p_hat <- p_hat_sex_5
n_sex_90 <- (z_90^2 * p_hat * (1 - p_hat)) / new_mer_sex^2

cat("Sample size needed for Age with 90% confidence:", n_age_90, "\n")
cat("Sample size needed for Female Proportion with 90% confidence:", n_sex_90, "\n")
#The new confidence level for both age and female proportion with smaller margin of error (50% smaller than the 5% sample size MOE) is %67.29.
#The smaller margin of error results in lower confidence level, which makes sense because smaller confidence intervals have less certainty.
#To achieve new margin of error with 90% confidence, we need sample size of 123.96 for both age and female proportion. 
#This sample size is larger than 5% sample since larger sample size is required to achieve smaller margin of error with higher confidence.

