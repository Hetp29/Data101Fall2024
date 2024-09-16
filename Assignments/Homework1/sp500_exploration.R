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