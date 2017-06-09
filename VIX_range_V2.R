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

#Russell
#getSymbols('^RUT', from = '1990-01-01')
#len <- 60
#min(tail(RUT$RUT.Low, len))
#max(tail(RUT$RUT.High, len))


my_table= c()
for (i in 1: (dim(instrument)[1] - n)){
    my_min <- min(instrument$Low[i:(i+n)], na.rm = TRUE)
    my_max <- max(instrument$High[i:(i+n)], na.rm = TRUE)
    my_close <- instrument$Close[i+n]
    my_table <- rbind(my_table,c( as.character(as.Date(index(instrument[i]),origin = "1970-01-01")),
                                  as.character(as.Date(index(instrument[(i+ n)]),origin = "1970-01-01")),
                                  if(is.numeric(instrument$Open[i])){instrument$Open[i]} else {'NaN'},
                                  if(is.numeric(my_close)){my_close} else {'NaN'},
                                  round(instrument$Close[i] - instrument$Open[i], 2),
                                  my_min,
                                  round(my_min/instrument$Open[i] - 1,6)*100.0, # Rounding the result up to 6 digits.
                                  my_max,
                                  round(my_max/instrument$Open[i] - 1,6)*100.0,
                                  round(my_max - my_min, 2),
                                  round((my_max - my_min)/instrument$Open[i],6)*100.0
    ))
}

a <- data.frame(my_table,row.names = NULL)
names(a) <- c("BeginDate","EndDate","Open","PeriodClose","DayNetChange","PeriodLow","OpenToLowPercent","PeriodHigh","OpenToHighPercent","Range","RangePercent")
b <- tbl_df(a)
b
write.table(x = b,file = outFile ,sep = c(",")) #optional output creating function

msg <- paste("Percent Range in", n, "days")
print(msg)
round(quantile(as.numeric(my_table[,11]), c(.70, .80, .90, .95, .99, .999, 1)), 1)

msg <- paste("Absolute Range in", n, "days (points)")
print(msg)
round(quantile(as.numeric(my_table[,10]), c(.50,.70, .80, .90, .95, .99, .999, 1)), 1)


msg <- paste("Open to Low Percent in", n, "days")
print(msg)
round(quantile(abs(as.numeric(my_table[,7])), c(.70, .80, .90, .95, .99, .999, 1)), 1) * -1

msg <- paste("Open to High Percent in", n, "days")
print(msg)
round(quantile(as.numeric(my_table[,9]), c(.70, .80, .90, .95, .99, .999, 1)), 1)

msg <- paste("Net Change (in points)", n, "days")
print(msg)
round(quantile(as.numeric(levels(b$NetChange)),c(0, .1, .2,.3, .4, .50,.60,.70,.80,.90,1)), 2)

max(as.numeric(my_table[,10]))
#plot 
plot(as.numeric(my_table[,10]))

hist(as.numeric(my_table[,11]), breaks=30, border="black", col="blue",xlab="Range Percentage Move", labels=F, main = paste(n, "Day Range % since" , startDate))

hist(as.numeric(my_table[,10]), breaks=30, border="black", col="blue",xlab="Range", labels=F, main = paste(n, "Day Range since" , startDate))

hist(as.numeric(my_table[,5]), breaks=30, border="black", col="blue",xlab="NetChange", labels=F, main = paste(n, "Day Net Change since" , startDate))

hist(as.numeric(my_table[,9]), breaks=30, border="blue", col="green", xlab="Open to High Percentage Move", labels=F, main = paste(n, "Day Open to High Range % since", startDate))

hist(as.numeric(my_table[,7]), breaks=30, border="blue", col="red", xlab="Open to Low Percentage Move", labels=F, main = paste(n, "Day Open to Low Range % since", startDate))


#read the CSV back in and work with it - haven't figured out how to do this to my_table directly

foo <- read.csv(outFile)

foo$PeriodNetChange <- foo$PeriodClose - foo$Open
foo$OpenToHigh <- foo$PeriodHigh - foo$Open
foo$OpenToLow <- foo$PeriodLow - foo$Open

sd(foo$OpenToHigh)
sd(foo$OpenToLow)
sd(foo$PeriodNetChange)

round(quantile(foo$PeriodNetChange, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)), 2)
round(quantile(foo$OpenToLow, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)), 2)
round(quantile(foo$OpenToHigh, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)), 2)


hist(as.numeric(foo$PeriodNetChange), breaks=30, border="black", col="blue",xlab="Range In Points", labels=F, main = paste(n, "Day VIX Open to Close change (in points) since" , startDate))

hist(as.numeric(foo$OpenToHigh), breaks=30, border="black", col="red",xlab="Open To High", labels=F, main = paste(n, "Day VIX Open to High change (in points) since" , startDate))

hist(as.numeric(foo$OpenToLow), breaks=30, border="black", col="green",xlab="Open To Low", labels=F, main = paste(n, "Day VIX Open to Low change (in points) since" , startDate))


#get dates where VIX was below 11
b$Open <- as.numeric(as.character(b$Open))
b$PeriodClose <- as.numeric(as.character(b$PeriodClose)) 
b$PeriodHigh <- as.numeric(as.character(b$PeriodHigh)) 
vix10 <- subset(b, Open  < 11)
hist(as.numeric(vix10$PeriodClose), breaks=10, border="black", col="red",xlab=paste("VIX Level", n, "days later"), labels=F, main = paste(n, "Day VIX closing price when VIX open below 11 since" , startDate))
hist(as.numeric(vix10$PeriodHigh), breaks=10, border="black", col="red",xlab=paste("Highest VIX Level in next", n, "days"), labels=F, main = paste(n, "Day VIX Highest price when VIX open below 11 since" , startDate))
