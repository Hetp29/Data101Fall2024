getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/hnp41_HW4")

sp_500_dataset <- read.csv("sp500_companies.csv")
summary(sp_500_dataset)
colnames(sp_500_dataset)

mean(sp_500_dataset$Currentprice)
sd(sp_500_dataset$Marketcap)
table(sp_500_dataset$Sector)
table(sp_500_dataset$Industry)
table(sp_500_dataset$Sector, sp_500_dataset$Country)
tapply(sp_500_dataset$Revenuegrowth, sp_500_dataset$Sector, mean)

#Task 1: Belief and observation where posterior odds of belief increase significantly 
#Get mean and standard deviaton of Marketcap
mean_marketcap <- mean(sp_500_dataset$Marketcap)
sd_marketcap <- sd(sp_500_dataset$Marketcap)
z_value <- 0.25
#approximate top 9.940358% threshold using z-value 0.25
approx_top_10_threshold <- mean_marketcap + z_value * sd_marketcap
approx_top_10_threshold

#to validate threshold
total_companies <- nrow(sp_500_dataset)
count_below_threshold <- nrow(sp_500_dataset[sp_500_dataset$Marketcap <= approx_top_10_threshold, ]) #number of companies below threshold
count_above_threshold <- total_companies - count_below_threshold #companies above threshold is difference
percent_above_threshold <- (count_above_threshold / total_companies) * 100 #percent of companies above threshold
approx_top_10_threshold #179873094087
count_above_threshold #50
percent_above_threshold #9.940358 (close to 10%)

observation_top_10_marketcap <- sp_500_dataset$Marketcap > approx_top_10_threshold
#Prior odds of company being in communication services sector
PriorCommServices <- nrow(sp_500_dataset[sp_500_dataset$Sector == 'Communication Services', ]) / nrow(sp_500_dataset)
PriorCommServicesOdds <- PriorCommServices / (1 - PriorCommServices)
cat("The prior odds of a company being in the Communication Services sector are:", PriorCommServicesOdds, "\n")
#probability of top 10% market cap given in communication services sector
TruePositive <- nrow(sp_500_dataset[observation_top_10_marketcap & sp_500_dataset$Sector == 'Communication Services', ]) / 
  nrow(sp_500_dataset[sp_500_dataset$Sector == 'Communication Services', ])
cat("The True Positive rate is:", TruePositive, "\n")
#probability of top 10% market cap given not in communication services
FalsePositive <- nrow(sp_500_dataset[observation_top_10_marketcap & sp_500_dataset$Sector != 'Communication Services', ]) / 
  nrow(sp_500_dataset[sp_500_dataset$Sector != 'Communication Services', ])
cat("The False Positive rate is:", FalsePositive, "\n")
#likelihood ratio
LikelihoodRatio <- TruePositive / FalsePositive
cat("The Likelihood Ratio is:", LikelihoodRatio, "\n")
#posterior odds and proability
PosteriorCommServicesOdds <- LikelihoodRatio * PriorCommServicesOdds
cat("The Posterior Odds of a company being in the Communication Services sector given Top 10% Market Cap are:", PosteriorCommServicesOdds, "\n")
PosteriorCommServices <- PosteriorCommServicesOdds / (1 + PosteriorCommServicesOdds)
cat("The Posterior Probability of a company being in the Communication Services sector given Top 10% Market Cap is:", PosteriorCommServices, "\n")

#Belief is whether a company in the communication services sector 
#Observation is that company has a top 10% market cap
#Prior odds of company being in communication services before considering market cap (0.0457)
#True positive rate is probability of having a top 10% market cap given the company is in communication services (0.2727)
#False positive is probability of top 10% market cap given company is not in communication services (0.0915)
#Likelihood ratio is how much more likely a top 10% market cap is for communication services compare to other sectors, result is 2.98 so top 10% marekt cap is nearly 3 times as likely if a company is in communication services
#posterior odds and proability is updated probability that company is in communication service, given it has top 10% market cap


#We checked if top 9.94% market cap increases odds of company being in communication services. The prior odds were 0.0457. After applying likelihood ratio of 2.98, the posterior probility rose to 0.12, showing that high market cap companies are nearly three times as likely to be in the communication services sector!
#I calculated mean and sd of market cap to set approximate threshold for top 10% (or you could say top 9.94%) using z-value of 0.25, which gave threshold close to true top 10%, verifying accuracy

#Task 2: Contingency table T for two categorical variables in data
t <- table(sp_500_dataset$Sector, sp_500_dataset$Revenuegrowth > 0)
cat("Contingency table of Sector vs Positive Revenue Growth:\n")
t

#prior probability and prior odds for belief
PriorProb <- sum(t["Technology", ]) / sum(t)
PriorOdds <- PriorProb / (1 - PriorProb)
cat("Prior Odds of being in the Technology sector:", PriorOdds, "\n")

#true positive - probability of having positive revenue growth when in the Technology sector
TruePositive <- t["Technology", 2] / sum(t["Technology", ])
cat("True Positive rate for Technology sector and positive revenue growth:", TruePositive, "\n")

#false positive - probability of having positive revenue growth not in technology sector
FalsePositive <- sum(t[, 2]) - t["Technology", 2]
FalsePositive <- FalsePositive / (sum(t) - sum(t["Technology", ])) 
cat("False Positive rate for not being in Technology sector and positive revenue growth:", FalsePositive, "\n")

#likelihood to have positive revenue growth in technology sector than in others
LikelihoodRatio <- TruePositive / FalsePositive
cat("Likelihood Ratio for Technology sector with positive revenue growth:", LikelihoodRatio, "\n")

#posterior odds and posterior probability
PosteriorOdds <- LikelihoodRatio * PriorOdds
cat("Posterior Odds for Technology sector with positive revenue growth:", PosteriorOdds, "\n")

PosteriorProbability <- PosteriorOdds / (1 + PosteriorOdds)
cat("Posterior Probability for Technology sector with positive revenue growth:", PosteriorProbability, "\n")

#We have a contingency table t where rows are sector (beliefs) and columns (observations) are whether the company has positive revenue growth 
#This gives counts of companies in each sector that either have positive revenue growth (TRUE) or not (FALSE)
#Prior odds of being in technology sector is 0.1872038
#True positive rate for technology sector and positive revenue growth is 0.7341772 means that 73.4% of companues in technology sector have positive revenue growth
#False positive rate for not being in technology sector and positive revenue growth is 0.7440758 which means 74.4% of companies outside the technology sector have positive revenue growth 
#likelihood ratio for technology sector with positive revenue growth is 0.9866968 means likelihood of having positive revenue growth in technology sector is almost same as in other sectors, but bit less
#posterior odds for technology sector with positive revenue growth is 0.1847134, decreased because likelihood is less than 1
#Although the technology sector has a high true positive rate for positive revenue growth, it does not significantly outperform other sectors when considering false positives
#T[i, j] or t[i, j] is true positive or probability of positive revenue growth in technology sector
#T[k, l] or t[k, l] is false positive or probability of positive revenue growth outside the technology sector 