data(mtcars) #load dataset 

head(mtcars) #take a look at first few rows of dataset
summary(mtcars) #summary statistics of dataset

#below we calculate mean miles per gallon (mpg)
mean_mpg <- mean(mtcars$mpg) 
print(paste("The mean miles per gallon is:", mean_mpg))

par(mar = c(4, 4, 2, 2)) # (bottom, left, top, right)

#create scatter plot of mpg vs horsepower(hp)
plot(mtcars$hp, mtcars$mpg,
     main="Scatter plot of Horsepower vs Miles Per Gallon",
     xlab="Horsepower (hp)", 
     ylab="Miles Per Gallon (mpg)",
     pch=19, col="blue")

abline(lm(mpg ~ hp, data=mtcars), col='red') #linear regression line


