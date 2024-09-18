getwd()
sp500_data <- read.csv('sp500_companies.csv')
sp500_data

summary(sp500_data)

colnames(sp500_data)
#Query 1
tapply(sp500_data$Currentprice, sp500_data$Sector, mean) #mean price of each sector
#CLickbait title: Consumer Cyclical stocks are 5x more expensive than energy stocks in S&P 500!

#Query 2
min_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, min)
max_price_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, max)
min_price_by_sector
max_price_by_sector
#Clickbait title: Consumer cyclical stocks are 1000x more expensive than energy!
#that stock is NVR Inc

#Query 3
top_10_marketcap <- head(sp500_data[order(-sp500_data$Marketcap), ], 10)
#sort data by market cap in decreasing order and take top 10
top_10_marketcap <- subset(top_10_marketcap, select = c(Symbol, Longname, Marketcap))
top_10_marketcap
#Clickbait title: How Apple and Microsoft dominate with over $6 trillion of the S&P 500!
#Clickbait title: 8 of the 10 most valuable S&P 500 Companies are in tech!

#Query 4

price_variance_by_sector <- tapply(sp500_data$Currentprice, sp500_data$Sector, var)
price_variance_by_sector
#Clickbait title: Consumer Cyclical Stocks have 1000x more price volatility than energy!

#Query 5
company_count_by_sector <- table(sp500_data$Sector)
company_count_by_sector
#Clickbait title: Technology dominates the S&P 500 with the most companies.

#Query 6
positive_growth_companies <- subset(sp500_data, Revenuegrowth > 0)
positive_growth_by_sector <- table(positive_growth_companies$Sector)
positive_growth_by_sector
#Clickbait title: Technology, financial companies, and industrials have over 50 companies seeing positive revenue!

#Query 7
sd_ebitda_by_sector <- tapply(sp500_data$Ebitda, sp500_data$Sector, sd)
sd_ebitda_by_sector
#standard deviation of Editda is earnings before interest and taxes for each sector
#Clickbait title: Communication services earnings swing $35 billion in S&P 500

#Query 8
top_5_revenue_growth <- sp500_data[order(-sp500_data$Revenuegrowth), ]
top_5_revenue_growth <- head(subset(top_5_revenue_growth, select = c(Symbol, Longname, Revenuegrowth)), 5)
top_5_revenue_growth
#CLickbait title: Super Micro Leads the Charge: 5 companies with explosive revenue growth in the S&P 500.

#Query 9
top_5_current_price <- sp500_data[order(-sp500_data$Currentprice), ]
top_5_current_price <- head(subset(top_5_current_price, select = c(Symbol, Longname, Currentprice)), 5)
top_5_current_price
#Clickbait title: The 5 most expensive stocks in the S&P 500-One costs over $9000

#Query 10
industry_distribution <- table(sp500_data$Industry)
sorted_industry_distribution <- sort(industry_distribution, decreasing = TRUE)
sorted_industry_distribution
#Clickbait title: The S&P 500's Biggest Players: Utilties lead with most companies in the index!

#barplot for top 5 companies by market cap
top_5_companies <- head(sp500_data[order(-sp500_data$Marketcap), ], 5)
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')
barplot(top_5_companies$Marketcap / 1e12, names.arg = top_5_companies$Symbol, col=colors, main="Top 5 Companies by Market Cap", xlab="Company", ylab="Market Cap (in Trillions)", 
        border="black")
#Clickbait Title: The 5 giants holding trillions in market cap in the S&P 500!

# Scatter plot for EBITDA vs Market Cap
plot(sp500_data$Ebitda / 1e12, sp500_data$Marketcap / 1e12, xlab="EBITDA (Trillions)", ylab="Market Cap (Trillions)", 
     main="Scatter Plot: Market Cap vs. EBITDA", col="blue", pch=19)
abline(ln(sp500_data$Marketcap / 1e12 ~ sp500_data$Ebitda / 1e12), col = "red")
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
#Clickbait Title: Can bigger earnings before interest really lead to bigger market riches? The suprising truth!

#box plot for stock prices across sectors
top_5_sectors <- names(sort(table(sp500_data$Sector), decreasing = TRUE)[1:5])
boxplot(sp500_data$Currentprice ~ sp500_data$Sector, xlab="Sector", ylab = "Current Stock Price",
        main="Stock Prices by Top 5 Sectors", col="lightblue", border="black",
        subset = sp500_data$Sector == 'Technology' | sp500_data$Sector == 'Industrials' | 
                sp500_data$Sector == 'Healthcare' | sp500_data == 'Financial Services' |
                sp500_data$Sector == 'Consumer Cyclical')
#Clickbait Title: Explosive Stock Prices in Consumer Cyclical: One Sector's Stocks Skyrocket While Others Stay Grounded

#mosaic plot
# Adjust the layout of the mosaic plot
par(mai = c(1.5, 1, 1, 1))  
sp500_data$RevenueGrowth_Category <- ifelse(sp500_data$Revenuegrowth >= 0, "Positive Growth", "Negative Growth")
all_sectors_revenue_table <- table(sp500_data$Sector, sp500_data$RevenueGrowth_Category)
colors <- c('red', 'blue')
mosaicplot(all_sectors_revenue_table, xlab = 'Sector', ylab = 'Revenue Growth Category', 
           main = "Revenue Growth by Sector", col = colors, las = 2, 
           cex.axis = 0.8, border = "black")
#Clickbait title: Which Sectors are struggling? Discover revenue growth divide in S&P 500!

#top 5 companies by current stock price (bar plot)
top_5_current_price <- sp500_data[order(-sp500_data$Currentprice), ]
top_5_current_price <- head(subset(top_5_current_price, select = c(Symbol, Longname, Currentprice)), 5)
colors <- c('red', 'blue', 'cyan', 'yellow', 'green')
barplot(top_5_current_price$Currentprice, names.arg = top_5_current_price$Symbol,
        col=colors, main="Top 5 Companies by Current Stock Price",
        xlab = "Company", ylab = "Current Stock Price", border = "black")
#Clickbait title: The $9000 Stock: Meet the most expensive companies in the S&P 500!

# Create boxplot for Revenue Growth by Sector
par(mai = c(1.5, 1, 1, 1))
boxplot(sp500_data$Revenuegrowth ~ sp500_data$Sector, 
        col = rainbow(length(unique(sp500_data$Sector))), 
        xlab = "Sector", 
        ylab = "Revenue Growth", 
        main = "Boxplot of Revenue Growth by Sector",
        las = 2,  # Rotate x-axis labels for readability
        border = "black", 
        outline = FALSE, 
        ylim = c(min(sp500_data$Revenuegrowth, na.rm = TRUE), max(sp500_data$Revenuegrowth, na.rm = TRUE)),
        cex.axis = 0.8)  
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
#Clickbait title: Which sectors are leading revenue growth? Find out where the winners and losers of S&P 500 are!
#Technology dominates 

