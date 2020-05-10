# load required packages
library(readr)
library(dplyr)
library(ggplot2)
library(forecast)

getwd()

setwd("C:/Users/NeeN/codeahead/projects/time series")

## Step 1: Load the data into R

sales <- read.csv("Data/MonthlySales.csv")

# examine dataset
str(sales)
head(sales, n = 5)
View(sales)

# plotting sales data against time

options(repr.plot.width = 6, repr.plot.height = 3)

ggplot(sales, aes(x = month, y = sales, group = 1)) + geom_line() + geom_smooth(method = 'lm') +labs(x = "Time", y = "Monthly Sales")


# convert our sales data to a time series object
salesTS <- ts(sales$sales, frequency = 12, start = c(2013,1))
class

options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
plot(salesDecomp)

# log transform time series data
salesLog <- log(salesTS)

salesLogHW <- HoltWinters(salesLog)
salesLogHW

options(repr.plot.width = 6, repr.plot.height = 4)
plot(salesLogHW)

# forecast next year's sales
nextYearSales <- forecast(salesLogHW, h=12)
# plot
plot(nextYearSales)


#Result
nextYearSales

nextYearSales$residuals

#validation

acf(nextYearSales$residuals, na.action = na.pass, lag.max=20)

Box.test(nextYearSales$residuals, lag=20, type="Ljung-Box")
