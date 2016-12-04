setwd("~/R")
require(quantmod)
require(TTR)
library(dplyr)


#S&P 500
getSymbols(c("^GSPC", "^VIX"), from="2000-01-01")
len <- 20
min(tail(GSPC$GSPC.Low, len))
max(tail(GSPC$GSPC.High, len))

n <- 20
period_table= c()
for (i in 1: (dim(GSPC)[1] - n)){
    period_min <- min(GSPC$GSPC.Low[i:(i+n)], na.rm = TRUE)
    period_max <- max(GSPC$GSPC.High[i:(i+n)], na.rm = TRUE)
    period_close <- max(GSPC$GSPC.Close[(i+n)], na.rm = TRUE)
    today_open <- max(round(GSPC$GSPC.Open[i], 2), na.rm = TRUE)
    period_table <- rbind(period_table,c( as.character(as.Date(index(GSPC[i]),origin = "1970-01-01")),
                                  as.character(as.Date(index(GSPC[(i+ n)]),origin = "1970-01-01")),
                                  round(GSPC$GSPC.Open[i], 2),
                                  round(GSPC$GSPC.Close[i],2),
                                  round(period_min, 2),
                                  round(period_max,2),
                                  round(period_close, 2),
                                  round((period_close - today_open)/GSPC$GSPC.Open[i],3)*100.0,
                                  round(period_min/GSPC$GSPC.Open[i] - 1,3)*100.0, # Rounding the result up to 6 digits.
                                  round(period_max/GSPC$GSPC.Open[i] - 1,3)*100.0,
                                  round((period_max - period_min)/GSPC$GSPC.Open[i],3)*100.0,
                                  round(VIX$VIX.Adjusted[i], 2)
    ))
}

a <- data.frame(period_table,row.names = NULL)
names(a) <- c("BeginDate","EndDate","Open","Close","PeriodLow","PeriodHigh","PeriodClose","PeriodGainPercent","OpenToLowPercent","OpenToHighPercent","RangePercent","VIX")
b <- tbl_df(a)
b

write.table(x = b,file = "SPX.csv",sep = c(",")) #optional output creating function


##show standard deviations of range
t <- as.numeric(as.character(a$RangePercent))
quantile(t, c(.70, .80, .90, .95, .995))
