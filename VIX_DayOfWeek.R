setwd("E:\\Data\\CloudStorage\\OneDrive\\Trading\\R\\n-day range")

require(quantmod)
require(TTR)
library(dplyr)

#change symbol here
sym = "VIX"

startDate <- '1990-01-01'
#S&P 500 ia ^GSPC, RUT is ^RUT, VIX is ^VIX
#note, if you are working with a normal stock symbol, not and index, then
#you do not need the leading ^ that is inserted by the line below
symbol <- paste("^", sym, sep="")
getSymbols(symbol, from = startDate)
#change symbol name here
instrument <- VIX
#number of periods
n <- 20
outFile = paste(sym, "_", startDate, "_table.csv", sep="")


#strip the symbol name off of the columns
colnames(instrument) <- c("Open","High","Low","Close","Volume","Adjusted")

instrument$PrevClose <- lag(instrument$Close)
instrument$Gap <- round(instrument$Open - lag(instrument$Close), 2)
instrument$Range <- round(instrument$High - instrument$Low, 2)
instrument$HigherOpen <- instrument$Open > lag(instrument$Close)
instrument$HigherClose <- instrument$Close > lag(instrument$Close)
instrument$Adjusted <- NULL
instrument$CloseToCloseRange <- round(abs(instrument$Close - lag(instrument$Close)), 2)
instrument$DayOfWeek <- .indexwday(instrument)
tail(instrument)

#show percentage of higher open by day of week
Dow <- round(aggregate(HigherOpen ~ DayOfWeek, instrument, mean), 2)
DoW$HigherOpen <- percent(DoW$HigherOpen)
DoW
DoWClose <- round(aggregate(HigherClose ~ DayOfWeek, instrument, mean), 2)
DoWClose$HigherClose <- percent(DoWClose$HigherClose)
DoWClose