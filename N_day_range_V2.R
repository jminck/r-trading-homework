setwd("E:\\Data\\CloudStorage\\OneDrive\\Trading\\R\\n-day range")

require(quantmod)
require(TTR)
library(dplyr)

#change symbol here
sym = "GSPC"

startDate <- '1990-01-01'
#S&P 500 ia ^GSPC, RUT is ^RUT, VIX is ^VIX
#note, if you are working with a normal stock symbol, not and index, then
#you do not need the leading ^ that is inserted by the line below
symbol <- paste("^", sym, sep="")
getSymbols(symbol, from = startDate)
#change symbol name here
instrument <- GSPC
#number of periods
n <- 20

outFile = paste(sym, "_", startDate, "_table.csv", sep="")


#strip the symbol name off of the columns
colnames(instrument) <- c("Open","High","Low","Close","Volume","Adjusted")

#add day of week column
instrument$DayOfWeek <- .indexwday(instrument)


my_table= c()
for (i in 1: (dim(instrument)[1] - n)){
    my_min <- min(instrument$Low[i:(i+n)], na.rm = TRUE)
    my_max <- max(instrument$High[i:(i+n)], na.rm = TRUE)
    my_table <- rbind(my_table,c( as.character(as.Date(index(instrument[i]),origin = "1970-01-01")),
                                  as.character(as.Date(index(instrument[(i+ n)]),origin = "1970-01-01")),
                                  instrument$Open[i],
                                  instrument$Close[i],
                                  my_min,
                                  round(my_min/instrument$Open[i] - 1,6)*100.0, # Rounding the result up to 6 digits.
                                  my_max,
                                  round(my_max/instrument$Open[i] - 1,6)*100.0,
                                  round(my_max - my_min, 2),
                                  round((my_max - my_min)/instrument$Open[i],6)*100.0
    ))
}

a <- data.frame(my_table,row.names = NULL)
names(a) <- c("BeginDate","EndDate","Open","Close","Low","OpenToLowPercent","High","OpenToHighPercent","Range","RangePercent")
b <- tbl_df(a)
b
write.table(x = b,file = outFile ,sep = c(",")) #optional output creating function

msg <- paste("Percent Range in", n, "days")
print(msg)
round(quantile(as.numeric(my_table[,10]), c(.70, .80, .90, .95, .99, .999, 1)), 1)

msg <- paste("Open to Low Percent in", n, "days")
print(msg)
round(quantile(abs(as.numeric(my_table[,6])), c(.70, .80, .90, .95, .99, .999, 1)), 1) * -1

msg <- paste("Open to High Percent in", n, "days")
print(msg)
round(quantile(as.numeric(my_table[,8]), c(.70, .80, .90, .95, .99, .999, 1)), 1)

max(as.numeric(my_table[,9]))
#plot 
plot(as.numeric(my_table[,9]))

hist(as.numeric(my_table[,10]), breaks=30, border="black", col="blue",xlab="Range Percentage Move", labels=F, main = paste(n, "Day Range % since" , startDate))

hist(as.numeric(my_table[,9]), breaks=30, border="black", col="blue",xlab="Range", labels=F, main = paste(n, "Day Range since" , startDate))

hist(as.numeric(my_table[,8]), breaks=30, border="blue", col="green", xlab="Open to High Percentage Move", labels=F, main = paste(n, "Day Open to High Range % since", startDate))

hist(as.numeric(my_table[,6]), breaks=30, border="blue", col="red", xlab="Open to Low Percentage Move", labels=F, main = paste(n, "Day Open to Low Range % since", startDate))

##filtering rows that had OpenToHighPercent > 5
#
#b$OpenToHighPercent <- as.numeric(as.character(b$OpenToHighPercent))
#x <- filter(b, as.numeric(OpenToHighPercent) > 5)
#print(tbl_df(x), n=40)
#write.table(x = x,file = "5_Percent_Upmove.csv",sep = c(",")) #optional output creating function

#add day of week to the data frame
b$DayOfWeek <- weekdays(as.Date(b$BeginDate))
