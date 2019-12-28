##########################
#1.Business Understanding#
#########################

#Global Mart is an online store super giant operating worldwide
#It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office
#The store caters to 7 different market segments - Africa, APAC, Canada, EMEA, EU, LATAM, America

######################
#2.Problem Statement #
######################

#To forecast sales and demand for next 6 months that would help you manage the revenue and inventory accordingly

################################
#3. Loading required libraries #
###############################

library(ggplot2)
library(graphics)
library(forecast)
library(tseries)
library(lubridate)
library(dplyr)

rm(list = ls())

#3.1 Loading Data set

globalstore <-
  read.csv("Global Superstore.csv", stringsAsFactors = F)

#3.2 Checking missing values

sapply(globalstore, function(x)
  sum(is.na(x)))

#It's found that none of the variables required for analysis/modelling contains NA values
#Postal Code has 41,296 NAs but its is not required for analysis

#3.3 Checking for duplicates entries. There are none.

sum(duplicated(globalstore))

#3.4 Checking for any defective column names in the data. They seem fine.

colnames(globalstore)

#3.5 Understanding the structure of data

head(globalstore)
str(globalstore)

#3.6 Selecting relevent data required to perform further analysis

globalstore <- globalstore[, c(3, 8, 13, 19, 20, 22)]

#3.7 Changing data type of the variable Order.date in order to carry out analysis

globalstore$Order.Date <-
  as.Date(globalstore$Order.Date, "%d-%m-%Y")

#######################
#4. Data Segmentation #
#######################

#4.1 The global store dataset has 21 market buckets based on market and customer segment
unique(paste(globalstore$Segment, globalstore$Market, sep = "-- "))

#4.2 Creating cumulative sales data table of 21 segments focusing on attributes Profit, Sales and Quantity

TotalProfit <-
  aggregate(
    globalstore$Profit,
    by = list(globalstore$Market, globalstore$Segment),
    FUN = sum
  )
names(TotalProfit) <- c("Market", "Segment", "TotalProfit")

TotalSales <-
  aggregate(
    globalstore$Sales,
    by = list(globalstore$Market, globalstore$Segment),
    FUN = sum
  )
names(TotalSales) <- list("Market", "Segment", "TotalSalesAmount")

SalesQuantity <-
  aggregate(
    globalstore$Sales,
    by = list(globalstore$Market, globalstore$Segment),
    FUN = length
  )
names(SalesQuantity) <- list("Market", "Segment", "SalesQuantity")

SalesSummary <- data.frame(TotalProfit, TotalSales, SalesQuantity)
SalesSummary <- SalesSummary[, c(1, 2, 3, 6, 9)]

#4.3 Generating Profit/Sales plots

#4.3.1 Plotting Profit Summary vs Segment
# We see APAC-Consumer and EU-Consumer segments have generated the most profit
SalesSummary$segname <-
  paste(SalesSummary$Market, SalesSummary$Segment, sep = "-")
p1 <-
  ggplot(SalesSummary, aes(
    x = reorder(SalesSummary$segname, -SalesSummary$TotalProfit),
    y = SalesSummary$TotalProfit
  )) + geom_bar(stat = "identity") + xlab("Region & Segment") + ylab("Total Profit") +
  ggtitle("Region & Segment Wise Profit Comparison") + coord_flip()
p1
#4.3.2 Plotting Total Sales Amt vs Segment
# We see APAC-Consumer and EU-Consumer segments have generated the most Total Sales Amt
p2 <-
  ggplot(SalesSummary,
         aes(
           x = reorder(SalesSummary$segname, -SalesSummary$TotalSalesAmount),
           y = SalesSummary$TotalSalesAmount
         )) + geom_bar(stat = "identity") + xlab("Region & Segment") + ylab("Total Sales Amt") +
  ggtitle("Region & Segment Wise Total Sales Amt Comparison") + coord_flip()
p2
#4.3.3 Plotting Total Sales Qty vs Segment
# We see APAC-Consumer and LATAM-Consumer segments have generated the most Total Sales Qty
p3 <-
  ggplot(SalesSummary, aes(
    x = reorder(SalesSummary$segname, -SalesSummary$SalesQuantity),
    y = SalesSummary$SalesQuantity
  )) + geom_bar(stat = "identity") + xlab("Region & Segment") + ylab("Total Sales Qty") +
  ggtitle("Region & Segment Wise Total Sales Qty Comparison") + coord_flip()
p3

#Through these 3 plots we see APAC Consumer,EU-Consumer and LATAM consumer are profitable and highest selling segments.
# But we need to see whether these markets are consistent.
# Hence we will be using COV metric.

############################################
#5. Calculation of Coefficient of Variance #
############################################

#5.1 Creating monthly sales data table focusing attributes Profit, Sales & Quantity

MonthlyTotalProfit <-
  aggregate(
    globalstore$Profit,
    by = list(
      globalstore$Market,
      globalstore$Segment,
      format(as.Date(globalstore$Order.Date), "%y-%m")
    ),
    FUN = sum
  )
names(MonthlyTotalProfit) <-
  list("Market", "Segment", "OrderDate", "MonthlyTotalProfit")

MonthlyTotalSales <-
  aggregate(
    globalstore$Sales,
    by = list(
      globalstore$Market,
      globalstore$Segment,
      format(as.Date(globalstore$Order.Date), "%y-%m")
    ),
    FUN = sum
  )
names(MonthlyTotalSales) <-
  list("Market", "Segment", "OrderDate", "MonthlyTotalSalesAmount")

MonthlySalesQuantity <-
  aggregate(
    globalstore$Sales,
    by = list(
      globalstore$Market,
      globalstore$Segment,
      format(as.Date(globalstore$Order.Date), "%y-%m")
    ),
    FUN = length
  )
names(MonthlySalesQuantity) <-
  list("Market", "Segment", "OrderDate", "MonthlySalesQuantity")

MonthlySalesSummary <-
  data.frame(MonthlyTotalProfit, MonthlyTotalSales, MonthlySalesQuantity)
MonthlySalesSummary <- MonthlySalesSummary[, c(1, 2, 3, 4, 8, 12)]

#5.2 Finding coefficient of variation of the profit for various market segment and product category combinations
#Coefficient of variation (COV) = Standard deviation (SD) / Mean

#5.2.1 Finding Mean of Monthly Total Profit for 21 combinations
Mean_MonthlyTotalProfit <-
  aggregate(
    MonthlySalesSummary$MonthlyTotalProfit,
    by = list(MonthlySalesSummary$Market, MonthlySalesSummary$Segment),
    FUN = mean
  )
names(Mean_MonthlyTotalProfit) <-
  list("Market", "Segment", "Mean_MonthlyTotalProfit")

#5.2.2 Finding SD of Monthly Total Profit for 21 combinations
SD_MonthlyTotalProfit <-
  aggregate(
    MonthlySalesSummary$MonthlyTotalProfit,
    by = list(MonthlySalesSummary$Market, MonthlySalesSummary$Segment),
    FUN = sd
  )
names(SD_MonthlyTotalProfit) <-
  list("Market", "Segment", "SD_MonthlyTotalProfit")

#5.2.3 Adding SD and Mean Values to the SalesSummary
SalesSummary <-
  data.frame(SalesSummary, Mean_MonthlyTotalProfit, SD_MonthlyTotalProfit)
SalesSummary <- SalesSummary[, c(1, 2, 3, 4, 5, 6, 9, 12)]


#5.2.4 Calculating COV and adding column in SalesSummary
SalesSummary$COV_MonthlyTotalProfit <-
  SalesSummary$SD_MonthlyTotalProfit / SalesSummary$Mean_MonthlyTotalProfit
p4 <-
  ggplot(SalesSummary,
         aes(
           x = reorder(SalesSummary$segname, SalesSummary$COV_MonthlyTotalProfit),
           y = SalesSummary$COV_MonthlyTotalProfit
         )) + geom_bar(stat = "identity") + xlab("Region and Segment") + ylab("COV Value") +
  coord_flip()
p4

#It is found APAC-consumers and EU-consumers are the two most profitable market segments (with least COV values)
# implying they are consistently performing the best


####################################################
#6.Creating Monthly Time Series Data for Modelling #
####################################################

#Setting seed so that results dont vary
set.seed(100)

#6.1 Converting the OrderDate variable to date.
MonthlySalesSummary$OrderDate <-
  parse_date_time(MonthlySalesSummary$OrderDate, orders = c("ym"))

#6.2 Deriving Month numbers from the parsed date.
MonthlySalesSummary$month.number <-
  sapply(MonthlySalesSummary$OrderDate, function(x)
    length(seq(
      from = min(MonthlySalesSummary$OrderDate),
      to = x,
      by = 'month'
    )))
range(MonthlySalesSummary$month.number)

#6.3 Segregating the two most profitable markets from the Sales Summary.
apac_consumer <-
  subset(MonthlySalesSummary, Market == "APAC" &
           Segment == "Consumer")
eu_consumer <-
  subset(MonthlySalesSummary, Market == "EU" & Segment == "Consumer")

#6.4 Deriving demand and sales of APAC region.
apac_con_demand <-
  apac_consumer[, c("month.number", "MonthlySalesQuantity")]
apac_con_sales <-
  apac_consumer[, c("month.number", "MonthlyTotalSalesAmount")]
apac_con_demand_for_fcst <-
  apac_consumer[, c("month.number", "MonthlySalesQuantity")]
apac_con_sales_for_fcst <-
  apac_consumer[, c("month.number", "MonthlyTotalSalesAmount")]

#6.5 Deriving demand and sales of EU region.
eu_con_demand <-
  eu_consumer[, c("month.number", "MonthlySalesQuantity")]
eu_con_sales <-
  eu_consumer[, c("month.number", "MonthlyTotalSalesAmount")]
eu_con_demand_for_fcst <-
  eu_consumer[, c("month.number", "MonthlySalesQuantity")]
eu_con_sales_for_fcst <-
  eu_consumer[, c("month.number", "MonthlyTotalSalesAmount")]


########################################
#7.APAC Consumer Sales Qty Forecasting #
#######################################



#7.1 Assigning test data and model data.
apac_rows <- nrow(apac_con_demand)
#apac_con_demand<-apac_con_demand[order(apac_con_demand$month.number),]
#7.1.1 Creating Test Data Set using last 6 months of the data
apac_con_demand_test <- apac_con_demand[(apac_rows - 5):apac_rows, ]
apac_con_demand <- apac_con_demand[1:(apac_rows - 6), ]
apac_con_demand_timeseries <-
  ts(apac_con_demand[, "MonthlySalesQuantity"])

#7.1.2 The time series looks additive since magnitude of seasonal does not correlate with value of series
plot(apac_con_demand_timeseries)

#7.2 Finding the trend and other additive components in the model. Frequency is taken for 12months as a year.
apac_con_demand_decomp <-
  decompose(ts(apac_con_demand[, "MonthlySalesQuantity"], frequency = 12))
plot(apac_con_demand_decomp)
#The decomposed time series shows a linear trend and low amplitude sinusoidal seasonal behaviour -40 to +40

#7.3 Classical Decomposition of APAC Demand Time Series

#7.3.1 Defining a Moving Average Smoothing Function
ts_movavg_smoother <- function(inp_timsr, width)
{
  smothed_timsr <-
    stats::filter(
      inp_timsr,
      filter = rep(1 / (2 * width + 1), (2 * width + 1)),
      method = "convolution",
      sides = 2
    )
  #In smothed_timesr, the starting and ending records of lenght equal to width are missing therefore we will smooth and impute these missing values
  #Smoothing Left Half
  left_diff <- smothed_timsr[width + 2] - smothed_timsr[width + 1]
  for (i in seq(from = width, to = 1, by = -1))
  {
    smothed_timsr[i] <- smothed_timsr[i + 1] - left_diff
  }
  
  #Smoothing Right Half
  row_count <- length(smothed_timsr)
  right_diff <-
    smothed_timsr[row_count - width] - smothed_timsr[row_count - width - 1]
  for (i in seq(from = row_count - width,
                to = row_count,
                by = 1))
  {
    smothed_timsr[i] <- smothed_timsr[i - 1] + right_diff
  }
  
  return(as.vector(smothed_timsr))
}


#7.3.2 Smoothening the Demand APAC consumer timeseries.
apac_con_demand_smooth <-
  ts_movavg_smoother(apac_con_demand_timeseries, 1)
lines(apac_con_demand_smooth, col = "blue", lwd = 2)
apac_con_demand_smooth <-
  data.frame(apac_con_demand$month.number, apac_con_demand_smooth)
colnames(apac_con_demand_smooth) <- c("Month_Number", "Month_Demand")

#7.3.3 Modeling the smoothed dataframe.
apac_con_demand_lmfit <-
  lm(
    Month_Demand ~ sin(0.5 * Month_Number) * poly(Month_Number, 1) +
      cos(0.1 * Month_Number) * poly(Month_Number, 1) +
      tan(0.02 * Month_Number),
    data = apac_con_demand_smooth
  )

apac_con_demand_global <-
  predict(apac_con_demand_lmfit, data = apac_con_demand_smooth$Month_Number)
plot(apac_con_demand_global,
     col = "green",
     lwd = 2,
     type = "l")

accuracy(apac_con_demand_global, apac_con_demand_smooth$Month_Demand)
#ME     RMSE      MAE       MPE    MAPE
#Test set -9.558533e-14 18.13466 14.13713 -2.666868 13.5169

#7.3.4 Finding the local component by subtracting the global component from demand.
apac_con_demand_local <-
  apac_con_demand_timeseries - apac_con_demand_global

#7.3.5 Plotting the local component on the graph.
plot(apac_con_demand_local, col = "blue")

#7.3.6 Checking for any weak stationarity with autocorrelation function (acf) and pacf
acf(apac_con_demand_local)
pacf(apac_con_demand_local)

#7.3.7 Finding if any AR(p)or MA(q) modeling chance.
apac_con_demand_localfit <- auto.arima(apac_con_demand_local)
apac_con_demand_localfit
#Yielded ARMA(0,0,0) which shows that there are no AR or MA component.

#7.3.8 Testing the residual for white noise. Subtracting the fitted component from the local component. Which will
#yield noise.
apac_con_demand_resi <-
  apac_con_demand_local - fitted(apac_con_demand_localfit)

#7.3.9 Performing adf and kpss test for checking stationarity and Testing the residual for White Noise
adf.test(apac_con_demand_resi, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.01.
#This shows that the residual yielded after the local and global components is stationary.

kpss.test(apac_con_demand_resi)
#The KPSS Test for Level Stationarity shows a p-value=0.1
#This shows that the residual yielded after the local and global components is stationary.

#7.3.10 Demand forecast for the next 6 months.
apac_con_demand_forecast <-
  predict.lm(
    apac_con_demand_lmfit,
    data.frame(Month_Number = apac_con_demand_test$month.number)
  )
apac_con_demand_forecast

#7.3.11 Finding the accuracy of consumer demand.
apac_con_demand_accuracy <-
  accuracy(apac_con_demand_forecast,
           apac_con_demand_test$MonthlySalesQuantity)
apac_con_demand_mape_classical <- apac_con_demand_accuracy[5]

#ME    RMSE      MAE       MPE     MAPE
#Test set -5.868476 32.8864 26.01847 -8.189832 17.14682

#7.3.12 Plotting the original time series vs the forecast series.
apac_con_demand_classical <-
  ts(c(apac_con_demand_global, apac_con_demand_forecast))
plot(ts(apac_con_demand[, "MonthlySalesQuantity"]),
     col = "red",
     lwd = 2,
     ylab = "Monthly Sales Qty")
lines(apac_con_demand_classical, col = "blue", lwd = 2)
legend(
  0,
  190,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

# -------------------------------- End of Classical Decomposition ------------------
#7.4 Auto Arima Model
apac_con_demand_autoarima <- auto.arima(apac_con_demand_timeseries)
apac_con_demand_autoarima
#Series: apac_con_demand_timeseries
#ARIMA(0,0,0)(1,1,0)[12] with drift
#sigma^2 estimated as 523.6:  log likelihood=-136.82
#AIC=279.64   AICc=280.56   BIC=283.84

#7.4.1 Plotting the residuals
tsdiag(apac_con_demand_autoarima)

#7.4.2 Plotting original time series in red and auto arima in blue.
plot(
  apac_con_demand_autoarima$x,
  col = "red",
  lwd = 2,
  ylab = "APAC Consumer Demand"
)
lines(fitted(apac_con_demand_autoarima),
      col = "blue",
      lwd = 2)
legend(
  1,
  190,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

#The shift in the curve to the right can be due to the increased lag in response to demand.

#7.4.3 Checking the residual values after removing the autoarima function.
apac_con_demand_autoarima_residual <-
  apac_con_demand_timeseries - fitted(apac_con_demand_autoarima)

#7.4.4 Performing ADF and KPSS tests to check if the residual is stationary.
#ADF
adf.test(apac_con_demand_autoarima_residual, alternative = "stationary")
#The augmented dickey fuller test shows pvalue of 0.24 which is >0.05
#Which implies the residual is stationary.

#KPSS
kpss.test(apac_con_demand_autoarima_residual)
#The KPSS test shows pvalue of >0.05
#Which implies the residual is stationary.

#7.4.5 Forecasting the demand.
apac_con_demand_autoarima_forecast <-
  predict(apac_con_demand_autoarima, n.ahead = 6)
apac_con_demand_autoarima_forecast

#7.4.6 Testing the accuarcy
apac_con_demand_autoarima_accuracy <-
  accuracy(
    apac_con_demand_autoarima_forecast$pred,
    apac_con_demand_test$MonthlySalesQuantity
  )
apac_con_demand_autoarima_accuracy
apac_con_demand_autoarima_mape <-
  apac_con_demand_autoarima_accuracy[5]
apac_con_demand_autoarima_mape

#The accuracy results are as follows.
#     ME     RMSE  MAE       MPE     MAPE
#Test set 12.5 47.31983 42.5 -1.604416 25.95232

#7.4.7 Final plot of AutoArima vs the original time series
apac_con_demand_autoarima_final <-
  ts(c(
    fitted(apac_con_demand_autoarima),
    apac_con_demand_autoarima_forecast$pred
  ))
plot(ts(apac_con_demand[, "MonthlySalesQuantity"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlySalesQuantity")
lines(apac_con_demand_autoarima_final,
      col = "blue",
      lwd = 2)
legend(
  1,
  190,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)
# -------------------------------- End of Arima Modeling  ------------------


###########################################
#8.APAC CONSUMER SALES AMOUNT FORECASTING #
##########################################

#8.1 Assigning test data and model data.
apac_con_sales_test <- apac_con_sales[(apac_rows - 5):apac_rows, ]
apac_con_sales <- apac_con_sales[1:(apac_rows - 6), ]
apac_con_sales_timeseries <-
  ts(apac_con_sales[, "MonthlyTotalSalesAmount"])

#8.1.2 The series has additive nature.
plot(apac_con_sales_timeseries)

#8.2 Finding the trend and other additive components in the model. Frequency is taken for 12months as a year.
apac_con_sales_decomp <-
  decompose(ts(apac_con_sales[, "MonthlyTotalSalesAmount"], frequency = 12))
plot(apac_con_sales_decomp)
#The decomposed time series shows a linear trend and high amplitude sinusoidal seasonal behaviour -15000 to 15000

#8.3 Classical Decomposition for sales forecast of APAC consumer.

#8.3.1 Smoothening the Demand APAC consumer timeseries.
apac_con_sales_smooth <-
  ts_movavg_smoother(apac_con_sales_timeseries, 1)

#8.3.1.2 Plotting the smoothened time series vs original time series
plot(apac_con_sales_timeseries)
lines(apac_con_sales_smooth, col = "blue", lwd = 2)

#8.3.1.2 Creating dataframe for modelling
apac_con_sales_smooth <-
  cbind(apac_con_sales$month.number, apac_con_sales_smooth)
apac_con_sales_smooth <- data.frame(apac_con_sales_smooth)
colnames(apac_con_sales_smooth) <- c("Month_Number", "Month_Sales")

#8.3.2 Modeling the smoothed dataframe.
apac_con_sales_lmfit <-
  lm(
    Month_Sales ~ sin(0.5 * Month_Number) * poly(Month_Number, 1) +
      cos(0.1 * Month_Number) * poly(Month_Number, 1) +
      tan(0.02 * Month_Number),
    data = apac_con_sales_smooth
  )
apac_con_sales_lmfit

apac_con_sales_global <-
  predict(apac_con_sales_lmfit, data = apac_con_sales_smooth$Month_Number)

#8.3.2.1 Plotting Global Component
plot(apac_con_sales_global, type = "l", col = "green")

accuracy(apac_con_sales_global, apac_con_sales_smooth$Month_Sales)
#ME     RMSE      MAE       MPE     MAPE
#Test set -1.520158e-11 5809.511 4767.205 -2.692046 14.67617

# 8.3.3 Finding the local component by subtracting the global component from demand.
apac_con_sales_local <-
  apac_con_sales_timeseries - apac_con_sales_global

#8.3.4 Plotting the local component on the graph.
plot(apac_con_sales_local, col = "blue")

#8.3.5 Checking for any weak stationarity with autocorrelation function (acf) and pacf
acf(apac_con_sales_local)
pacf(apac_con_sales_local)

# 8.3.6 Finding if any AR(p)or MA(q) modeling chance.
apac_con_sales_localfit <- auto.arima(apac_con_sales_local)
apac_con_sales_localfit
#Yielded ARMA(0,0,0) which shows that there are no AR or MA component.
#sigma^2 estimated as 87824827:  log likelihood=-444.41
#AIC=892.82   AICc=893.13   BIC=896.3

#8.3.7 Testing the residual for white noise. Subtracting the fitted component from the local component. Which will
#yield noise.

apac_con_sales_resi <-
  apac_con_sales_local - fitted(apac_con_sales_localfit)

#8.3.8 Performing adf and kpss test for checking stationarity.

#Testing the residual for White Noise
adf.test(apac_con_sales_resi, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.017.
#This shows that the residual yielded after the local and global components is stationary.

kpss.test(apac_con_sales_resi)
#The KPSS Test for Level Stationarity shows a p-value=0.1
#This shows that the residual yielded after the local and global components is stationary.

#8.3.9 Sales forecast for the next 6 months.
apac_con_sales_forecast <-
  predict.lm(apac_con_sales_lmfit,
             data.frame(Month_Number = apac_con_sales_test$month.number))
apac_con_sales_forecast
#Forecast values
#1        2        3        4        5        6
#45072.24 47683.96 50331.90 52639.52 54387.59 55565.95

#8.3.10 Finding the accuracy of consumer sales.
apac_con_sales_accuracy <-
  accuracy(apac_con_sales_forecast,
           apac_con_sales_test$MonthlyTotalSalesAmount)
apac_con_sales_mape_classical <- apac_con_sales_accuracy[5]

# ME     RMSE      MAE      MPE     MAPE
#Test set 9800.081 17180.13 14600.94 10.34701 22.53589

#8.3.11 Plotting the original time series vs the forecast series.
apac_con_sales_classical <-
  ts(c(apac_con_sales_global, apac_con_sales_forecast))
plot(ts(apac_con_sales[, "MonthlyTotalSalesAmount"]),
     col = "red",
     lwd = 2,
     ylab = "Monthly total sales amount")
lines(apac_con_sales_classical, col = "blue", lwd = 2)
legend(
  1,
  70000,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

# -------------------------------- End of Classical Decomposition ------------------

#8.4 Auto Arima Model
apac_con_sales_autoarima <- auto.arima(apac_con_sales_timeseries)
apac_con_sales_autoarima
#Series: apac_con_sales_timeseries
#ARIMA(0,1,1) - Indicates differencing of 1 stage and the time series is modeled as MA(1)
#Coefficients:
#  ma1
#-0.7559
#s.e.   0.1381

#sigma^2 estimated as 174361555:  log likelihood=-447.11
#AIC=898.23   AICc=898.55   BIC=901.66

##8.4.1 Plotting the residuals
tsdiag(apac_con_sales_autoarima)

#8.4.2 Checking the residual values after removing the autoarima function. Which should ideally be whitenoise
apac_con_sales_autoarima_residual <-
  apac_con_sales_timeseries - fitted(apac_con_sales_autoarima)
#Performing ADF and KPSS tests to check if the residual is stationary.

#ADF
adf.test(apac_con_sales_autoarima_residual, alternative = "stationary")
#The augmented dickey fuller test shows pvalue of 0.01 which is <0.05
#Which implies the residual is stationary.

#KPSS
kpss.test(apac_con_sales_autoarima_residual)
#The KPSS test shows pvalue of >0.05
#Which implies the residual is stationary.

#8.4.3 Forecasting the Sales.
apac_con_sales_autoarima_forecast <-
  predict(apac_con_sales_autoarima, n.ahead = 6)
apac_con_sales_autoarima_forecast

#8.4.4 Testing the accuarcy
apac_con_sales_autoarima_accuracy <-
  accuracy(
    apac_con_sales_autoarima_forecast$pred,
    apac_con_sales_test$MonthlyTotalSalesAmount
  )
apac_con_sales_autoarima_accuracy
apac_con_sales_autoarima_mape <- apac_con_sales_autoarima_accuracy[5]
apac_con_sales_autoarima_mape

#The accuracy results are as follows.
#      ME     RMSE      MAE      MPE     MAPE
#Test set 15848.24 22755.75 18780.19 19.73091 27.68952

#8.4.5 Final plot of AutoArima vs the original time series
apac_con_sales_autoarima_final <-
  ts(c(
    fitted(apac_con_sales_autoarima),
    apac_con_sales_autoarima_forecast$pred
  ))
plot(ts(apac_con_sales[, "MonthlyTotalSalesAmount"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlyTotalSalesAmount")
lines(apac_con_sales_autoarima_final,
      col = "blue",
      lwd = 2)
legend(
  1,
  70000,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)
#The autoarima model shows very bad fit when superimposed on the original time series. Which shows the model not predicting the values.

# -------------------------------- End of Arima Modeling ------------------


###############################
#9.EU SALES QTY FORECASTING   #
###############################

#9.1 APAC modeling the demand.
eu_rows <- nrow(eu_con_demand)

#9.2 Assigning test data and model data.
eu_con_demand_test <- eu_con_demand[(eu_rows - 5):eu_rows, ]
eu_con_demand <- eu_con_demand[1:(eu_rows - 6), ]

eu_con_demand_timeseries <-
  ts(eu_con_demand[, "MonthlySalesQuantity"])
#9.2.1 The Plot of the EU Sales Qty time series shows its an Additive time Series
plot(eu_con_demand_timeseries)

#9.3 Finding the trend and other additive components in the model. Frequency is taken for 12months as a year.
eu_con_demand_decomp <-
  decompose(ts(eu_con_demand[, "MonthlySalesQuantity"], frequency = 12))
plot(eu_con_demand_decomp)
#The decomposed time series shows a linear trend and low amplitude sinusoidal seasonal behaviour -40 to +40

#9.4 Classical Decomposition for demand forecast of APAC consumer.

#9.4.1 Smoothening the Demand EU consumer timeseries.
eu_con_demand_smooth <-
  ts_movavg_smoother(eu_con_demand_timeseries, 1)

#9.4.1.2 Plotting time series with smoothened time series
plot(eu_con_demand_timeseries)
lines(eu_con_demand_smooth, col = "blue", lwd = 2)
#9.4.1.3 Creating smoothened dataframe for modelling
eu_con_demand_smooth <-
  cbind(eu_con_demand$month.number, eu_con_demand_smooth)
eu_con_demand_smooth <- data.frame(eu_con_demand_smooth)
colnames(eu_con_demand_smooth) <- c("Month_Number", "Month_Demand")

#9.4.2 Modeling the smoothed dataframe.
eu_con_demand_lmfit <-
  lm(
    Month_Demand ~ sin(0.5 * Month_Number) * poly(Month_Number, 1) +
      cos(0.1 * Month_Number) * poly(Month_Number, 1) +
      tan(0.02 * Month_Number),
    data = eu_con_demand_smooth
  )
eu_con_demand_lmfit

eu_con_demand_global <-
  predict(eu_con_demand_lmfit, data = eu_con_demand_smooth$Month_Number)
#9.4.2.1 Plotting EU Con demand global series
plot(eu_con_demand_global,
     col = "green",
     lwd = 2,
     type = "l")

accuracy(eu_con_demand_global, eu_con_demand_smooth$Month_Demand)
#ME     RMSE      MAE       MPE     MAPE
#Test set 2.080879e-14 12.07203 9.774717 -1.519217 10.32261

#9.4.3 Finding the local component by subtracting the global component from demand.
eu_con_demand_local <-
  eu_con_demand_timeseries - eu_con_demand_global

#9.4.4 Plotting the local component on the graph.
plot(eu_con_demand_local, col = "blue")

#9.4.5 Checking for any weak stationarity with autocorrelation function (acf) and pacf
acf(eu_con_demand_local)
pacf(eu_con_demand_local)

#9.4.6 Finding if any AR(p)or MA(q) modeling chance.
eu_con_demand_localfit <- auto.arima(eu_con_demand_local)
eu_con_demand_localfit
#Yielded ARMA(0,0,0) which shows that there are no AR or MA component.

#9.4.7 Testing the residual for white noise. Subtracting the fitted component from the local component. Which will
#yield noise.

eu_con_demand_resi <-
  eu_con_demand_local - fitted(eu_con_demand_localfit)

#9.4.8 Performing adf and kpss test for checking stationarity.

#Testing the residual for White Noise
adf.test(eu_con_demand_resi, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p value of 0.01973.
#This shows that the residual yielded after the local and global components is stationary.

kpss.test(eu_con_demand_resi)
#The KPSS Test for Level Stationarity shows a p-value=0.1
#This shows that the residual yielded after the local and global components is stationary.

#9.4.9 Demand forecast for the next 6 months.
eu_con_demand_forecast <-
  predict.lm(eu_con_demand_lmfit,
             data.frame(Month_Number = eu_con_demand_test$month.number))
eu_con_demand_forecast

#1        2        3        4        5        6
#144.7717 166.2285 189.2933 211.2837 230.1208 244.8849

#9.4.10 Finding the accuracy of consumer demand.
eu_con_demand_accuracy <-
  accuracy(eu_con_demand_forecast,
           eu_con_demand_test$MonthlySalesQuantity)
eu_con_demand_mape_classical <- eu_con_demand_accuracy[5]

# ME     RMSE      MAE       MPE     MAPE
#Test set -11.09718 48.65461 36.68766 -12.45764 22.98871

#Plotting the original time series vs the forecast series.
eu_con_demand_classical <-
  ts(c(eu_con_demand_global, eu_con_demand_forecast))
plot(ts(eu_con_demand[, "MonthlySalesQuantity"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlySalesQuantity")
lines(eu_con_demand_classical, col = "blue", lwd = 2)
legend(
  0,
  176,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

# -------------------------------- End of Classical Decomposition ------------------

#9.5 Auto Arima Model
eu_con_demand_autoarima <- auto.arima(eu_con_demand_timeseries)
eu_con_demand_autoarima
#Series: eu_con_demand_timeseries
#ARIMA(2,1,0) which shows differencing of 1 and AR(2) model with the following coefficients.

#Coefficients:
#  ar1      ar2
#-0.7167  -0.6005
#s.e.   0.1210   0.1169

#sigma^2 estimated as 1468:  log likelihood=-207.19
#AIC=420.37   AICc=421.02   BIC=425.51


#9.5.1 Plotting the residuals chart
tsdiag(eu_con_demand_autoarima)
#The auto arima model does not show accurate predictions when overlapped with original time series.

#9.5.3 Checking the residual values after removing the autoarima function.
eu_con_demand_autoarima_residual <-
  eu_con_demand_timeseries - fitted(eu_con_demand_autoarima)
#Performing ADF and KPSS tests to check if the residual is stationary.

#ADF
adf.test(eu_con_demand_autoarima_residual, alternative = "stationary")
#The augmented dickey fuller test shows pvalue of 0.03 which is <0.05
#Which implies the residual is stationary.

#KPSS
kpss.test(eu_con_demand_autoarima_residual)
#The KPSS test shows pvalue of >0.05
#Which implies the residual is stationary.

#9.5.4 Forecasting the demand.
eu_con_demand_autoarima_forecast <-
  predict(eu_con_demand_autoarima, n.ahead = 6)
eu_con_demand_autoarima_forecast

#9.5.5 Testing the accuarcy
eu_con_demand_autoarima_accuracy <-
  accuracy(eu_con_demand_autoarima_forecast$pred,
           eu_con_demand_test$MonthlySalesQuantity)
eu_con_demand_autoarima_accuracy
eu_con_demand_autoarima_mape <- eu_con_demand_autoarima_accuracy[5]
eu_con_demand_autoarima_mape

#The accuracy results are as follows.
#    ME     RMSE      MAE      MPE     MAPE
#Test set 61.38164 78.87617 63.49613 27.14558 29.01681

#9.5.6 Final plot of AutoArima vs the original time series
eu_con_demand_autoarima_final <-
  ts(c(
    fitted(eu_con_demand_autoarima),
    eu_con_demand_autoarima_forecast$pred
  ))
plot(ts(eu_con_demand[, "MonthlySalesQuantity"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlySalesQuantity")
lines(eu_con_demand_autoarima_final,
      col = "blue",
      lwd = 2)
legend(
  1,
  170,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)
# -------------------------------- End of Arima Modeling ------------------

###################################
#10. EU SALES AMOUNT FORECASTING  #
###################################

#10.1 Assigning test data and model data.
eu_con_sales_test <- eu_con_sales[(eu_rows - 5):eu_rows, ]
eu_con_sales <- eu_con_sales[1:(eu_rows - 6), ]

eu_con_sales_timeseries <-
  ts(eu_con_sales[, "MonthlyTotalSalesAmount"])
#10.1.1 The time series has additive components as can be seen from the plot
plot(eu_con_sales_timeseries)

#10.2 Finding the trend and other additive components in the model. Frequency is taken for 12months as a year.
eu_con_sales_decomp <-
  decompose(ts(eu_con_sales[, "MonthlyTotalSalesAmount"], frequency = 12))
plot(eu_con_sales_decomp)
#The decomposed time series shows a linear trend and low amplitude sinusoidal seasonal behaviour -15000 to 15000

#10.3 Classical Decomposition for Sales forecast of EU consumer.

#10.3.1 Smoothening the Sales EU consumer timeseries.
eu_con_sales_smooth <- ts_movavg_smoother(eu_con_sales_timeseries, 1)
lines(eu_con_sales_smooth, col = "blue", lwd = 2)
eu_con_sales_smooth <-
  cbind(eu_con_sales$month.number, eu_con_sales_smooth)
eu_con_sales_smooth <- data.frame(eu_con_sales_smooth)
colnames(eu_con_sales_smooth) <- c("Month_Number", "Month_Sales")

#10.3.2 Modeling the smoothed dataframe.
eu_con_sales_lmfit <-
  lm(
    Month_Sales ~ sin(0.5 * Month_Number) * poly(Month_Number, 1) +
      cos(0.1 * Month_Number) * poly(Month_Number, 1) +
      tan(0.02 * Month_Number),
    data = eu_con_sales_smooth
  )
eu_con_sales_lmfit

eu_con_sales_global <-
  predict(eu_con_sales_lmfit, data = eu_con_sales_smooth$Month_Number)

#10.3.2.1 Plot EU Sales Global Component
plot(eu_con_sales_global,
     col = "green",
     lwd = 2,
     type = "l")

accuracy(eu_con_sales_global, eu_con_sales_smooth$Month_Sales)
#ME     RMSE      MAE       MPE     MAPE
#Test set 3.334877e-12 4368.172 3583.233 -2.577753 13.72255

#10.3.3 Finding the local component by subtracting the global component from demand.
eu_con_sales_local <- eu_con_sales_timeseries - eu_con_sales_global

#10.3.4 Plotting the local component on the graph.
plot(eu_con_sales_local, col = "blue")

#10.3.4 Checking for any weak stationarity with autocorrelation function (acf) and pacf
acf(eu_con_sales_local)
pacf(eu_con_sales_local)

#10.3.5 Finding if any AR(p)or MA(q) modeling chance.
eu_con_sales_localfit <- auto.arima(eu_con_sales_local)
eu_con_sales_localfit
#Yielded ARMA(0,0,0) which shows that there are no AR or MA component.

#10.3.6 Subtracting the fitted component from the local component. Which will
#yield noise.

eu_con_sales_resi <-
  eu_con_sales_local - fitted(eu_con_sales_localfit)

#10.3.7 Performing adf and kpss test for checking stationarity on the residuals.Checking for White Noise
adf.test(eu_con_sales_resi, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01.
#This shows that the residual yielded after the local and global components is stationary.

kpss.test(eu_con_sales_resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This shows that the residual yielded after the local and global components is stationary.

#10.3.8 Demand forecast for the next 6 months.
eu_con_sales_forecast <-
  predict.lm(eu_con_sales_lmfit,
             data.frame(Month_Number = eu_con_sales_test$month.number))
eu_con_sales_forecast

#10.3.9 Finding the accuracy of consumer demand.
eu_con_sales_accuracy <-
  accuracy(eu_con_sales_forecast,
           eu_con_sales_test$MonthlyTotalSalesAmount)
eu_con_sales_mape_classical <- eu_con_sales_accuracy[5]

#ME     RMSE   MAE       MPE    MAPE
#Test set -7984.093 15763.74 13886 -22.13802 30.6975

#10.3.10 Plotting the original time series vs the forecast series.
eu_con_sales_classical <-
  ts(c(eu_con_sales_global, eu_con_sales_forecast))
plot(ts(eu_con_sales[, "MonthlyTotalSalesAmount"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlyTotalSalesAmount")
lines(eu_con_sales_classical, col = "blue", lwd = 2)
legend(
  1,
  55000,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

# -------------------------------- End of Classical Decomposition ------------------

#10.4 Auto Arima Model
eu_con_sales_autoarima <- auto.arima(eu_con_sales_timeseries)
eu_con_sales_autoarima
#Series: eu_con_sales_timeseries
#ARIMA(2,1,0) which shows differential 1 and AR modeling of AR(2) with coefficients,
#Coefficients:
#  ar1      ar2
#-0.5796  -0.4906
#s.e.   0.1346   0.1310
#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81

#10.4.1 Plotting the residuals
tsdiag(eu_con_sales_autoarima)
#The curve does not give an accurate prediction when compared to the original response
#shifts in the curve to the right can be due to the increased lag in response to demand.

#10.4.2 Checking the residual values after removing the autoarima function.
eu_con_sales_autoarima_residual <-
  eu_con_sales_timeseries - fitted(eu_con_sales_autoarima)
#Performing ADF and KPSS tests to check if the residual is stationary.

#ADF
adf.test(eu_con_sales_autoarima_residual, alternative = "stationary")
#The augmented dickey fuller test shows pvalue of 0.01 which is <0.05
#Which implies the residual is stationary.

#KPSS
kpss.test(eu_con_sales_autoarima_residual)
#The KPSS test shows pvalue of >0.05
#Which implies the residual is stationary.

#10.4.3 Forecasting the demand.
eu_con_sales_autoarima_forecast <-
  predict(eu_con_sales_autoarima, n.ahead = 6)
eu_con_sales_autoarima_forecast

#10.4.4 Testing the accuarcy
eu_con_sales_autoarima_accuracy <-
  accuracy(eu_con_sales_autoarima_forecast$pred,
           eu_con_sales_test$MonthlyTotalSalesAmount)
eu_con_sales_autoarima_accuracy
eu_con_sales_autoarima_mape <- eu_con_sales_autoarima_accuracy[5]
eu_con_sales_autoarima_mape

#The accuracy results are as follows.
#     ME     RMSE     MAE    MPE    MAPE
#Test set 12935.21 19499.14 16687.6 17.678 28.9226

#10.4.5 Final plot of AutoArima vs the original time series
eu_con_sales_autoarima_final <-
  ts(c(
    fitted(eu_con_sales_autoarima),
    eu_con_sales_autoarima_forecast$pred
  ))
plot(ts(eu_con_sales[, "MonthlyTotalSalesAmount"]),
     col = "red",
     lwd = 2,
     ylab = "MonthlyTotalSalesAmount")
lines(eu_con_sales_autoarima_final, col = "blue", lwd = 2)
legend(
  1,
  55000,
  legend = c("Actual Sales", "Forecast Sales"),
  col = c("red", "blue"),
  lty = 1,
  cex = 0.8
)

# -------------------------------- End of Arima Modelling ------------------

##########################################################
#11.Model Evaluation and Future Forecast for EU and APAC #
##########################################################

#11.1 We will evaluate models by comparing the accuracy for EU and APAC consumer and sales results

#11.1.1 APAC Consumer Demand Accuracy by Classical Decomp.
apac_con_demand_accuracy[5] #MAPE 17.14%

#11.1.2 APAC Consumer Demand Accuracy by Auto Arima
apac_con_demand_autoarima_accuracy[5] #MAPE 25.95%

#11.1.3 APAC Consumer Sales Accuracy by Classical Decomp

apac_con_sales_accuracy[5] #MAPE22.53%

#11.1.4 APAC Consumer Sales Accuracy by Auto Arima

apac_con_sales_autoarima_accuracy[5] #MAPE 27.6%

#11.1.5 EU Consumer Demand Accuracy by Classical Decomp

eu_con_demand_accuracy[5] #MAPE 22.98%

#11.1.6 EU Consumer Demand Accuracy by Auto Arima

eu_con_demand_autoarima_accuracy[5] #MAPE 29.01%

#11.1.7 EU Consumer Sales Accuracy by Classical Decomp

eu_con_sales_accuracy[5] #MAPE 30.6%

#11.1.8 EU Consumer Demand Accuracy by Auto Arima
eu_con_sales_autoarima_accuracy[5] #MAPE 28.92%

#11.2 Forecasting the next 6 months for Demand and Sales values using the best method (least MAPE)
#Defining out of next  6 month forecast variables
forecast_month <- c(49:54)

#11.3 APAC consumer demand future forecast

#Classical model performs better than Auto Arima Model. Predicting future 6 month demand using Classical Model.
#Hence we are using Classical Model to predict APAC Consumer Demand
apac_con_demand_futfor <-
  predict.lm(apac_con_demand_lmfit,
             data.frame(Month_Number = forecast_month))
#Plotting the future 6 month forecast graph. The consumer demand is expected to rise.
apac_con_demand_futfor <- as.vector(apac_con_demand_futfor)
apac_con_demand_futfor <-
  data.frame(forecast_month, apac_con_demand_futfor, flag = 1)
apac_con_demand_for_fcst <-
  data.frame(apac_con_demand_for_fcst, flag = 0)
colnames(apac_con_demand_futfor) <- colnames(apac_con_demand_for_fcst)
apac_plot_df <- rbind(apac_con_demand_for_fcst, apac_con_demand_futfor)
ggplot(apac_plot_df,
       aes(x = month.number, y = MonthlySalesQuantity, color = flag)) + geom_line() +
  ggtitle("APAC Monthly Sales Qty Demand Trend with Forecast") + theme(legend.position =
                                                                         'none')

#11.4 APAC consumer sales amt future forecast.
#According to MAPE values, Classical model performs better than Auto Arima Model.
#Predicting future 6 month demand using Classical Model. We see an increasing trend
apac_con_sales_futfor <-
  predict.lm(apac_con_sales_lmfit,
             data.frame(Month_Number = forecast_month))
apac_con_sales_futfor <- as.vector(apac_con_sales_futfor)
apac_con_sales_for_fcst <- data.frame(apac_con_sales_for_fcst, flag = 0)
apac_con_salesfutfor <-
  data.frame(forecast_month, apac_con_sales_futfor, flag = 1)
colnames(apac_con_salesfutfor) <- colnames(apac_con_sales_for_fcst)
apac_con_sales_df <-
  rbind(apac_con_sales_for_fcst, apac_con_salesfutfor)
ggplot(apac_con_sales_df,
       aes(x = month.number, y = MonthlyTotalSalesAmount, color = flag)) + geom_line() +
  ggtitle("APAC MonthlyTotalSalesAmount Trend with Forecast") + theme(legend.position =
                                                                        'none')



#11.5 EU consumer demand future forecast.
#According to MAPE values, Classical model performs better than Auto Arima Model.
#Predicting future 6 month demand using Classical Model.The demand will increase over next 6 months
eu_con_demand_futfor <-
  predict.lm(eu_con_demand_lmfit,
             data.frame(Month_Number = forecast_month))
eu_con_demand_futfor <- as.vector(eu_con_demand_futfor)
eu_con_demand_futfor <-
  data.frame(forecast_month, eu_con_demand_futfor, flag = 1)
eu_con_demand_for_fcst <- data.frame(eu_con_demand_for_fcst, flag = 0)
colnames(eu_con_demand_futfor) = colnames(eu_con_demand_for_fcst)
eu_con_demand_df <- rbind(eu_con_demand_for_fcst, eu_con_demand_futfor)
ggplot(eu_con_demand_df,
       aes(x = month.number, y = MonthlySalesQuantity, color = flag)) + geom_line() +
  ggtitle("EU MonthlySalesQuantity Trend with Forecast") + theme(legend.position =
                                                                   'none')


#11.6 EU consumer sales future forecast.
#According to MAPE values, Auto.arima performs better than Clasiical Decomp Model.
#Predicting future 12 months using Auto.arima Model since we want forecast for 6 months "after" test data set and we are using the model on train dataset
# Please note we wont use 6 months since we will be getting the prediction for test dataset which was made above.
eu_con_sales_futfor <- predict(eu_con_sales_autoarima, n.ahead = 12)
eu_con_sales_futfor <- as.data.frame(eu_con_sales_futfor)
eu_con_sales_futfor_2 <-
  data.frame(forecast_month, eu_con_sales_futfor$pred[7:12], flag = 1)
eu_con_sales_for_fcst <- data.frame(eu_con_sales_for_fcst, flag = 0)
colnames(eu_con_sales_futfor_2) = colnames(eu_con_sales_for_fcst)
eu_con_sales_df = rbind(eu_con_sales_for_fcst, eu_con_sales_futfor_2)
ggplot(eu_con_sales_df,
       aes(x = month.number, y = MonthlyTotalSalesAmount, color = flag)) + geom_line() +
  ggtitle("EU MonthlyTotalSalesAmount Trend with Forecast") + theme(legend.position =
                                                                      'none')
