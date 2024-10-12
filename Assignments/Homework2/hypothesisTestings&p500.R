getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Homework2")
sp500_data <- read.csv('sp500_companies.csv')
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

summary(sp500_data)

colnames(sp500_data)

tapply(sp500_data$Marketcap, sp500_data$Sector, mean)
tapply(sp500_data$Currentprice, sp500_data$Sector, mean)
tapply(sp500_data$Fulltimeemployees, sp500_data$Sector, mean)

#HYPOTHESIS 1 (small p-value/strong alternative hypothesis, much less than 0.05)
#Null hypothesis: There is no significant difference in the number of full-time employees between Technology and Consumer Defensive sectors.
#Alternative hypothesis: There is a significant difference in the number of full-time employees between Technology and Consumer Defensive sectors.
p_value1 <- permutation_test(sp500_data, 'Sector', 'Fulltimeemployees', 50000, 'Technology', 'Consumer Defensive')
cat("Tech Giants or Staffing Powerhouses? Full-Time Employee Count Shows Massive Sector Gap!\n") #Title for first hypothesis test
cat("The significance level for this test is: 0.05\n")
cat("The p-value for hypothesis 1 (very low p-value) is:", p_value1, "\n")
#p-value is 0.00576, we can reject null hypothesis with confidence
#There is a significant difference in the number of full-time employees between companies in Technology and Consumer Defensive sectors.

#HYPOTHESIS 2 (close call, p-value less than 0.05 but close)
#Null hypothesis: There is no difference in the current price between companies in the energy and industrials sectors.
#Alternative hypothesis: There is a significant difference in the current price between companies in the energy and industrials sectors.
p_value2 <- permutation_test(sp500_data, 'Sector', 'Currentprice', 50000, 'Energy', 'Industrials')
cat("Energy vs Industrials: The Price Battle You Didn't See Coming!\n") #Title for second hypothesis test
cat("The significance level for this test is: 0.05\n")
cat("The p-value for hypothesis 2 (close call) is:", p_value2, "\n")
#p-value is 0.03896, we can reject null hypothesis but this one is a close call!
#there is enough evidence to declare that there is a significant difference in current price between companies in energy and industrials sectors.

#HYPOTHESIS 3 (reject null hypothesis, p-value exceeds 0.05)
#Null hypothesis: There is no significant difference in market cap between companies in the consumer defensive and healthcare sectors. 
#Alternative hypothesis: There is a significant difference in market cap between companies in the consumer defensive and healthcare sectors.
p_value3 <- permutation_test(sp500_data, 'Sector', 'Marketcap', 50000, 'Consumer Defensive', 'Healthcare')
cat("Marketcap Standoff: No Clear Winner Between Healthcare and Consumer Defensive!\n") #Title for third hypothesis test
cat("The significance level for this test is: 0.05\n")
cat("The p-value for hypothesis 3 (rejecting the null hypothesis) is:", p_value3, "\n")
#p-value is 0.42626, we cannot reject the null hypothesis 
#there is no significant difference in market cap between companies in consumer defensive and healthcare sectors

#Narrow query
M0 <- mean(sp500_data$Marketcap)
cat("The overall mean of Marketcap (M0) is:", M0, "\n")
#mean for overall market cap is: $103.8 billion

Narrow <- subset(sp500_data, Sector == 'Real Estate' & Exchange == 'NYQ')

M <- mean(Narrow$Marketcap)
cat("The mean of Marketcap for the narrow query (M) is:", M, "\n")
#mean market cap for real estate sector is $39.49 billion 

cat("M =", M, "and M0 =", M0, "\n")
cat("2 * M0 =", 2 * M0, "and 1/2 * M0 =", 0.5 * M0, "\n")

cat("Does M > 2 * M0? ", M > 2 * M0, "\n")
cat("Does M < 1/2 * M0? ", M < 0.5 * M0, "\n")
#1/2 * M0 equals $51.9 billion, and M = $39.49 billion, so M < 1/2 * M0 is satisfied
#Null hypothesis: There is no significant difference in market cap between entire dataset and narrow subset with real estate sector on NYQ exchange
#Alternative hypothesis: There is a significant difference in market cap between entire dataset and the narrow subset
#conclusion: Since M < 1/2 * M0, we can reject null hypothesis and conclude market cap for real estate sector on NYQ exchange is significantly smaller than overall mean
