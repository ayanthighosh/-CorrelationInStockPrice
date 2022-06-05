library(readxl)
library(corrplot)
library(pdfetch)

stocks <- vector()
stocks <- c("IBM", "GOOGL", "AAPL", "QCOM", "AMZN", "NFLX", "MSFT", "ADBE", "CSCO", "SAP")

for(i in 1:10 )
{  
    stock_val <- pdfetch_YAHOO(stocks[i],fields	= c("open","high","low","close","adjclose","volume"), from = as.Date("2007-01-01"), to = Sys.Date(), interval = "1d") 
    stock_cor <- round(cor(stock_val),2)
    print(as.matrix(stock_cor))
    corrplot(stock_cor, method="number")
}

#After analysing, I find that all the price are highly correlated.
#To avoid data redundancy, I am choosing Volume and adjclose price to proceed further

stock_val <- pdfetch_YAHOO(stocks ,  fields	= c("adjclose","volume"), from = as.Date("2007-01-01"), to = Sys.Date(), interval = "1d") 
df = as.data.frame(stock_val)
View(df)

#Finding correlation matrix between all the stocks
#This will give us clearity, if one stock price is going up
#then how is it effecting other stock's volume

#data.frame(stocks_cor)
stock_cor <- round(cor(stock_val),2)
print(as.matrix(stock_cor))
corrplot(stock_cor, method="number")

#applying -.95<x<.95 filter to the above 20x20 correlated matrix
filter1 <- as.data.frame(stock_cor)
filter1[ filter1>0.95 | filter1< -0.95 ] <- "NA"
View( filter1 )
print(as.matrix(filter1))

#applying -.8<x<.8 filter to the above 20x20 correlated matrix
filter2 <- as.data.frame(stock_cor)
filter2[ filter2 > 0.8 | filter2 < -0.8 ] <- "NA"
View( filter2 )
print(as.matrix(filter2))




