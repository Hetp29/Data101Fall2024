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