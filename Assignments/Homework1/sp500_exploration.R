getwd()
#read dataset
airquality_data <- read.csv('airQuality.csv')

tapply(airquality_data$Temp_Range, airquality_data$City, mean)
#average temperature range for each city
#Chicago has an average Temp_Range of 16.50.
#Dallas has an average Temp_Range of 17.79.
#Phoenix has an average Temp_Range of 18.56.
tapply(airquality_data$Health_Risk_Score, airquality_data$City, mean)
#average health risk scores of each city 
#higher risk score = higher health impact, caused by poor air quality 
