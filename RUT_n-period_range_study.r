setwd("C:/Users/Shobeir/Desktop/Fiverr/20160330/josephminckler/")
require(quantmod)
require(TTR)
library(dplyr)

#install.packages("psych",dependencies=TRUE) #the minimum requirement or
#install.packages(c("psych","GPArotation"),dependencies=TRUE) #required for factor analysis

library(psych) #Only need to make psych active once a session

#S&P 500
getSymbols(c("^RUT", "^VIX"), from="2000-01-01")
len <- 20
min(tail(RUT$RUT.Low, len))
max(tail(RUT$RUT.High, len))

n <- 20
period_table= c()
for (i in 1: (dim(RUT)[1] - n)){
    period_min <- min(RUT$RUT.Low[i:(i+n)], na.rm = TRUE)
    period_max <- max(RUT$RUT.High[i:(i+n)], na.rm = TRUE)
    period_close <- max(RUT$RUT.Close[(i+n)], na.rm = TRUE)
    today_open <- max(round(RUT$RUT.Open[i], 2), na.rm = TRUE)
    period_table <- rbind(period_table,c( as.character(as.Date(index(RUT[i]),origin = "1970-01-01")),
                                          as.character(as.Date(index(RUT[(i+ n)]),origin = "1970-01-01")),
                                          round(RUT$RUT.Open[i], 2),
                                          round(RUT$RUT.Close[i],2),
                                          round(period_min, 2),
                                          round(period_max,2),
                                          round(period_close, 2),
                                          round((period_close - today_open)/RUT$RUT.Open[i],3)*100.0,
                                          round(period_min/RUT$RUT.Open[i] - 1,3)*100.0, # Rounding the result up to 6 digits.
                                          round(period_max/RUT$RUT.Open[i] - 1,3)*100.0,
                                          round((period_max - period_min)/RUT$RUT.Open[i],3)*100.0,
                                          round(VIX$VIX.Adjusted[i], 2)
                                          
    ))
}

a <- data.frame(period_table,row.names = NULL)
names(a) <- c("BeginDate","EndDate","Open","Close","PeriodLow","PeriodHigh","PeriodClose","PeriodGainPercent","OpenToLowPercent","OpenToHighPercent","RangePercent","VIX")
b <- tbl_df(a)
b

write.table(x = b,file = "RUT.csv",sep = c(",")) #optional output creating function


##show standard deviations of range
t <- as.numeric(as.character(a$RangePercent))
quantile(t, c(.70, .80, .90, .95, .995))
