getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Homework2")
sp500_data <- read.csv('sp500_companies.csv')
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

summary(sp500_data)

colnames(sp500_data)

#HYPOTHESIS 1
#Null hypothesis: There is no significant difference in market cap between companies in technology and healthcare sectors.
#Alternative hypothesis: There is a significant difference in market cap between companies in technology and healthcare sectors.
p_value1 <- permutation_test(sp500_data, 'Sector', 'Marketcap', 10000, 'Technology', 'Healthcare')
cat("The p-value for Hypothesis 3 (Strong Alternative) is:", p_value1)
