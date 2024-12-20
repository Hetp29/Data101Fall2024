getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/hnp41_HW3")
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
#The new confidence level for both age and female proportion with smaller margin of error (50% smaller than the 5% sample size MOE) is 67.29%.
#The smaller margin of error results in lower confidence level, which makes sense because smaller confidence intervals have less certainty.
#To achieve new margin of error with 90% confidence, we need sample size of 123.96% for both age and female proportion. 
#This sample size is larger than 5% sample since larger sample size is required to achieve smaller margin of error with higher confidence.

#Task 3: confidence interval two categorical variables
C1 <- 'Sex'
C2 <- 'Survived'
c1 <- 'female'
c2 <- '1'

subset_S <- dataset[dataset$Sex == c1 & dataset$Survived == c2, ] 
total_tuples <- nrow(dataset)

p_hat_S <- nrow(subset_S) / total_tuples #fraction of tuples where both conditions hold

se_S <- sqrt((p_hat_S * (1 - p_hat_S)) / total_tuples) #standard error for proportion

z_90 <- qnorm(1 - 0.05) #z-value for 90% confidence
moe_S<- z_90 * se_S #margin of error

ci_lower_S <- p_hat_S - moe_S
ci_upper_S <- p_hat_S + moe_S

cat("Proportion of female survivors (p-hat):", p_hat_S, "\n")
cat("90% Confidence Interval: [", ci_lower_S, ",", ci_upper_S, "]\n")
#Proportion of female survivors is 0.2598, so approximately 26% of total dataset consist of female survivors
#90% confidence interval is [0.2356, 0.2840] which means that we're 90% confident that true proportion of female survivors falls within this range

#Task 4
#Define numerical variable and minimum subset size
N <- 'Age'
min_rows <- 0.05 * nrow(dataset) #Minimum size for each subset

#Define subsets based on Age ranges and ensure each has at least 5% of data points
subset1 <- dataset[dataset$Age <= 20, ]
subset2 <- dataset[dataset$Age > 20 & dataset$Age <= 40, ]
subset3 <- dataset[dataset$Age > 40 & dataset$Age <= 60, ]
subset4 <- dataset[dataset$Age > 60, ]

#Define the z-score for 90% confidence
z_90 <- qnorm(1 - 0.05)

#Calculate confidence intervals for each subset, or set values to NA if they don’t meet the minimum size

if (nrow(subset1) >= min_rows) {
  n1 <- nrow(subset1)
  mean_age1 <- mean(subset1[[N]])
  sd_age1 <- sd(subset1[[N]])
  se_age1 <- sd_age1 / sqrt(n1)
  moe_age1 <- z_90 * se_age1
  ci_lower1 <- mean_age1 - moe_age1
  ci_upper1 <- mean_age1 + moe_age1
  ci_width1 <- ci_upper1 - ci_lower1
} 

if (nrow(subset2) >= min_rows) {
  n2 <- nrow(subset2)
  mean_age2 <- mean(subset2[[N]])
  sd_age2 <- sd(subset2[[N]])
  se_age2 <- sd_age2 / sqrt(n2)
  moe_age2 <- z_90 * se_age2
  ci_lower2 <- mean_age2 - moe_age2
  ci_upper2 <- mean_age2 + moe_age2
  ci_width2 <- ci_upper2 - ci_lower2
} 

if (nrow(subset3) >= min_rows) {
  n3 <- nrow(subset3)
  mean_age3 <- mean(subset3[[N]])
  sd_age3 <- sd(subset3[[N]])
  se_age3 <- sd_age3 / sqrt(n3)
  moe_age3 <- z_90 * se_age3
  ci_lower3 <- mean_age3 - moe_age3
  ci_upper3 <- mean_age3 + moe_age3
  ci_width3 <- ci_upper3 - ci_lower3
} 

if (nrow(subset4) >= min_rows) {
  n4 <- nrow(subset4)
  mean_age4 <- mean(subset4[[N]])
  sd_age4 <- sd(subset4[[N]])
  se_age4 <- sd_age4 / sqrt(n4)
  moe_age4 <- z_90 * se_age4
  ci_lower4 <- mean_age4 - moe_age4
  ci_upper4 <- mean_age4 + moe_age4
  ci_width4 <- ci_upper4 - ci_lower4
} 

#Print each subset's confidence interval details
cat("Subset 1: Age <= 20\n")
cat("Size:", n1, "\nMean Age:", mean_age1, "\nCI Width:", ci_width1, "\n90% CI: [", ci_lower1, ",", ci_upper1, "]\n\n")

cat("Subset 2: 20 < Age <= 40\n")
cat("Size:", n2, "\nMean Age:", mean_age2, "\nCI Width:", ci_width2, "\n90% CI: [", ci_lower2, ",", ci_upper2, "]\n\n")

cat("Subset 3: 40 < Age <= 60\n")
cat("Size:", n3, "\nMean Age:", mean_age3, "\nCI Width:", ci_width3, "\n90% CI: [", ci_lower3, ",", ci_upper3, "]\n\n")

cat("Subset 4: Age > 60\n")
cat("Size:", n4, "\nMean Age:", mean_age4, "\nCI Width:", ci_width4, "\n90% CI: [", ci_lower4, ",", ci_upper4, "]\n\n")

cat("Best Subset: 20 < Age <= 40\n")
cat("Best Subset Size:", n2, "\n")
cat("Mean Age in Best Subset:", mean_age2, "\n")
cat("90% Confidence Interval for Mean Age: [", ci_lower2, ",", ci_upper2, "]\n")
cat("Narrowest CI Width:", ci_width2, "\n")

