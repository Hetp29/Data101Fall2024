getwd()
#read dataset
sp500_data <- read.csv('sp500_companies.csv')
sp500_data

summary(sp500_data)

colnames(sp500_data)
#Query 1
tapply(sp500_data$Currentprice, sp500_data$Sector, mean) #mean price of each sector
#Consumer cyclical has highest average price at $444.67
#Energy and Utilities have lower average prices at $81.98 and $85.87
#Tech stocks have average price of $256.13 while financial services average around $186.80
#Tech and consumer cyclical are examples of sectors with higher-priced utilities
#CLickbait title: Consumer Cyclical stocks are 5x more expensive than energy stocks in S&P 500!

#Query 2
min_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, min)
max_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, max)
min_price_by_sector
max_price_by_sector
#Lowest-price stocks come from communication services at $8.49 and healthcare at $9.21
#most expensive stock in consumer cyclical is priced at 9369.58, with technology second at $1868.96
#Clickbait title: Consumer cyclical stocks are 1000x more expensive than energy!
#that stock is NVR Inc

#Query 3
top_10_marketcap <- head(sp500_data[order(-sp500_data$Marketcap), ], 10)
#sort data by market cap in decreasing order and take top 10
top_10_marketcap <- subset(top_10_marketcap, select = c(Symbol, Longname, Marketcap))
top_10_marketcap
#we sort data by market cap then select top 10 rows, subsetting columns we want
#top 10 companies by market cap in S&P is dominated by tech giants like AAPL and MSFT, only two 
#companies over $3 trillion
#Clickbait title: How Apple and Microsoft dominate with over $6 trillion of the S&P 500!
#Clickbait title: 8 of the 10 most valuable S&P 500 Companies are in tech!

#Query 4
#Calculation of stock prices within each sector
#high variance sectors show wide range of stock prices (high volatility)
#low variance suggests stock is more consistent
price_variance_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, var)
price_variance_by_sector
#Variance in stock prices across sectors shows Consumer Cyclical sector has highest price variance at 1,880,471.45
#utilities and energy sector exhibit lower price variance (more consistent pricing)
#Clickbait title: Consumer Cyclical Stocks have 1000x more price volatility than energy!

#Query 5
company_count_by_sector <- table(sp500_data$Sector)
company_count_by_sector
#Above we see number of companies in S&P 500 per sector
#technology leads with 80 companies, industrialis is second with 70
#Clickbait title: Technology dominates the S&P 500 with the most companies.

#Query 6
positive_growth_companies <- subset(sp500_data, Revenuegrowth > 0)
positive_growth_by_sector <- table(positive_growth_companies$Sector)
positive_growth_by_sector
#Technology has 58 companies showing positive revenue growth (the most)
#financial services and industrials have 53 each companies with positive revenue growth 
#consumer cyclical has 41 companies seeing growth 
#energy has 21 growing companies, utilities has 24
#Clickbait title: Technology, financial companies, and industrials have over 50 companies seeing positive revenue!

#Query 7
sd_ebitda_by_sector <- tapply(sp500_data$Ebitda, sp500_data$Sector, sd)
sd_ebitda_by_sector
#standard deviation of Editda is earnings before interest and taxes for each sector
#Communication services and technology have more volatile earnings 
#high amount indicates there's large variation in earnings among companies in that sector
#more companies in communication and technology have high earnings
#Clickbait title: Communication services earnings swing $35 billion in S&P 500

#Query 8
top_5_revenue_growth <- sp500_data[order(-sp500_data$Revenuegrowth), ]
top_5_revenue_growth <- head(subset(top_5_revenue_growth, select = c(Symbol, Longname, Revenuegrowth)), 5)
top_5_revenue_growth
#Top 5 companies by revenue growth in S&P 500 are lead by SMCI (143%), NVIDIA (122%), MU (81.5%), NEM (64.1%), and EXR (57.8%)
#CLickbait title: Super Micro Leads the Charge: 5 companies with explosive revenue growth in the S&P 500.

#Query 9
top_5_current_price <- sp500_data[order(-sp500_data$Currentprice), ]
top_5_current_price <- head(subset(top_5_current_price, select = c(Symbol, Longname, Currentprice)), 5)
top_5_current_price
#top 5 companies with highest stock prices are led by NVR, Booking Holdings, AZO, FICO, and MTD (highest price stocks in S&P 500)
#Clickbait title: The 5 most expensive stocks in the S&P 500-One costs over $9000

#Query 10
industry_distribution <- table(sp500_data$Industry)
sorted_industry_distribution <- sort(industry_distribution, decreasing = TRUE)
sorted_industry_distribution
#Utilities - Regulated Electric Industry has highest amount of companies in S&P 500
#The S&P 500's Biggest Players: Utilties lead with most companies in the index!

#barplot for top 5 companies by market cap
top_5_companies <- head(sp500_data[order(-sp500_data$Marketcap), ], 5)
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')
barplot(top_5_companies$Marketcap / 1e12, names.arg = top_5_companies$Symbol, col=colors, main="Top 5 Companies by Market Cap", xlab="Company", ylab="Market Cap (in Trillions)", 
        border="black")
#Clickbait Title: The 5 giants holding trillions in market cap in the S&P 500!

# Scatter plot for EBITDA vs Market Cap
plot(sp500_data$Ebitda / 1e12, sp500_data$Marketcap / 1e12, xlab="EBITDA", ylab="Market Cap", 
     main="Scatter Plot: Market Cap vs. EBITDA", col="blue", pch=19)
#Clickbait Title: Can bigger earnings before interest really lead to bigger market riches? The suprising truth!
