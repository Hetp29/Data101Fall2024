getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Homework1")
sp500_data <- read.csv('sp500_companies.csv')
sp500_data

summary(sp500_data)

colnames(sp500_data)

tapply(sp500_data$Currentprice, sp500_data$Sector, mean) #mean price of each sector

min_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, min)
max_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, max)
min_price_by_sector
max_price_by_sector

top_10_marketcap <- head(sp500_data[order(-sp500_data$Marketcap), ], 10)
top_10_marketcap <- subset(top_10_marketcap, select = c(Symbol, Longname, Marketcap))
top_10_marketcap

price_variance_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, var)
price_variance_by_sector

company_count_by_sector <- table(sp500_data$Sector)
company_count_by_sector

positive_growth_companies <- subset(sp500_data, Revenuegrowth > 0)
positive_growth_by_sector <- table(positive_growth_companies$Sector)
positive_growth_by_sector

sd_ebitda_by_sector <- tapply(sp500_data$Ebitda, sp500_data$Sector, sd)
sd_ebitda_by_sector

top_5_revenue_growth <- sp500_data[order(-sp500_data$Revenuegrowth), ]
top_5_revenue_growth <- head(subset(top_5_revenue_growth, select = c(Symbol, Longname, Revenuegrowth)), 5)
top_5_revenue_growth

top_5_current_price <- sp500_data[order(-sp500_data$Currentprice), ]
top_5_current_price <- head(subset(top_5_current_price, select = c(Symbol, Longname, Currentprice)), 5)
top_5_current_price

industry_distribution <- table(sp500_data$Industry)
sorted_industry_distribution

#barplot for top 5 companies by market cap
top_5_companies <- head(sp500_data[order(-sp500_data$Marketcap), c("Symbol", "Marketcap")], 5)
top_5_companies
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')
barplot(top_5_companies$Marketcap / 1e12 ~ top_5_companies$Symbol, col=colors, main="Top 5 Companies by Market Cap", xlab="Company", ylab="Market Cap (in Trillions)", 
        border="black")

# Scatter plot for EBITDA vs Market Cap
sp500_data$Ebitda_trillion <- sp500_data$Ebitda / 1e12 #divided to get decimal values 
sp500_data$Marketcap_trillion <- sp500_data$Marketcap / 1e12
plot(sp500_data$Ebitda_trillion, sp500_data$Marketcap_trillion, 
     xlab = "EBITDA (Trillions)", ylab = "Market Cap (Trillions)", 
     main = "Scatter Plot: Market Cap vs. EBITDA", col = "blue")

#box plot for stock prices across sectors

colors <- c('red', 'blue', 'cyan', 'yellow')
boxplot(sp500_data$Currentprice ~ sp500_data$Sector, xlab="Sector", ylab = "Current Stock Price",
        main="Stock Prices by Top 5 Sectors", col="yellow", border="black")

#mosaic plot
sp500_data$RevenueGrowth_Category <- ifelse(sp500_data$Revenuegrowth >= 0, "Positive Growth", "Negative Growth")
colors <- c('red', 'blue')
mosaicplot(sp500_data$Sector ~ sp500_data$RevenueGrowth_Category, xlab = 'Sector', ylab = 'Revenue Growth Category', 
           main = "Revenue Growth by Sector", col = colors, border = "black")


#top 5 companies by current stock price (bar plot)
top_5_current_price <- head(subset(top_5_current_price, select = c(Symbol, Longname, Currentprice)), 5)
top_5_current_price
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')
barplot(top_5_current_price$Currentprice ~ top_5_current_price$Symbol, 
        col=colors, main="Top 5 Companies by Current Stock Price",
        xlab = "Company", ylab = "Current Stock Price", border = "black")

# boxplot for Revenue Growth by Sector
boxplot(sp500_data$Revenuegrowth ~ sp500_data$Sector, 
        col = 'red', 
        xlab = "Sector", 
        ylab = "Revenue Growth", 
        main = "Boxplot of Revenue Growth by Sector",
        border = "black") 




#scatter plot to see if there is relation between market cap and revenue growth
plot(sp500_data$Marketcap / 1e12, sp500_data$Revenuegrowth,
     xlab = "Market Cap (Trillions)",
     ylab = "Revenue Growth",
     main = "Scatter Plot: Market Cap vs Revenue Growth",)

