setwd("c:\users\joseph\git\r-trading_homework")

# this script currently depends on downloading CSV format of historical data from yahoo
# and the file being named GSPC.csv
# https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC


require(quantmod)
require(TTR)
library(dplyr)
library(plyr)

function(x, shiftLen = 1L) {
    r <- (1L + shiftLen):(length(x) + shiftLen)
    r[r<1] <- NA
    return(x[r])
}


#save today's date for labeling plot graphics
today <- Sys.Date()

#change symbol here
sym = "GSPC"

startDate <- '2017-01-01'
#S&P 500 ia ^GSPC, RUT is ^RUT, VIX is ^VIX
#note, if you are working with a normal stock symbol, not and index, then
#you do not need the leading ^ that is inserted by the line below
symbol <- paste("^", sym, sep="")
getSymbols(symbol, from = startDate)
#change symbol name here
instrument <- GSPC
#number of periods

QQ <- read.csv('GSPC.csv', na.strings='null', colClasses=c("character", rep("numeric",6)))
QQ$Date <- as.Date(QQ$Date)
QQ$PrevClose <- rowShift(QQ$Close, -1)
QQ$Gap <- round(QQ$Open - rowShift(QQ$Close, -1), 2)
QQ$Range <- round(QQ$High - QQ$Low, 2)
QQ$HigherOpen <- QQ$Open > rowShift(QQ$Close, -1)
QQ$HigherClose <- QQ$Close > rowShift(QQ$Close, -1)
QQ$Adj.Close <- NULL
QQ$CloseToCloseRange <- round(abs(QQ$Close - rowShift(QQ$Close, -1)), 2)

RR <- filter(QQ, HigherOpen == TRUE)
RR$GapClose <- RR$Low < rowShift(RR$Close)
SS <- filter(QQ, HigherOpen == FALSE)
SS$GapClose <- SS$High > rowShift(SS$Close)

#add year column to data
dates <- as.Date(QQ$Date, "%d-%b-%y")
years <- format(dates, "%Y")
QQ$Year <- years

#get by date subset
Recent <- subset(QQ, Date > "2012-01-01")

#plot for Close to Close range
u <- NULL
means <- NULL
maxs <- NULL
means <- aggregate(CloseToCloseRange ~ Year, Recent, mean)
maxs <- aggregate(CloseToCloseRange ~ Year, Recent, max)
means$CloseToCloseRange <- round(means$CloseToCloseRange, 2)
u <- ggplot(data=Recent, aes(x=Year, y=CloseToCloseRange))
meds  <- aggregate(CloseToCloseRange ~ Year, Recent, median)
u <- u + geom_jitter(aes(colour=HigherClose)) +
    geom_boxplot(alpha=0.7, outlier.shape = NA,  na.rm = FALSE, size = .5) +
    geom_text(data = means, aes(label = CloseToCloseRange, y = CloseToCloseRange)) +
    geom_text(data = maxs, aes(label = CloseToCloseRange, y =CloseToCloseRange))

u <- u + xlab("Year") +
    ylab("Close To Close Range") +
    ggtitle(paste("SPX Range from One Daily Close To The Next [ through", today, "]")) +
    theme(axis.title.x = element_text(colour="Black", size=20),
          axis.title.y = element_text(colour="Black", size=20),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.position = c(1,1),
          legend.justification = c(1,1),
          plot.title = element_text(colour="DarkBlue", size=25))
u

#Plot for Open to Close range
v <- NULL
means <- NULL
maxs <- NULL
means <- aggregate(Range ~ Year, Recent, mean)
maxs <- aggregate(Range ~ Year, Recent, max)
means$Range <- round(means$Range, 2)
v <- ggplot(data=Recent, aes(x=Year, y=Range))
meds  <- aggregate(Range ~ Year, Recent, median)
v <- v + geom_jitter(aes(colour=HigherClose)) +
    geom_boxplot(alpha=0.7, outlier.shape = NA,  na.rm = FALSE, size = .5) +
    geom_text(data = means, aes(label = Range, y = Range)) +
    geom_text(data = maxs, aes(label = Range, y =Range))

v <- v + xlab("Year") +
    ylab("Daily Range") +
    ggtitle(paste("SPX Range from Open to Close [ through", today, "]")) +
    theme(axis.title.x = element_text(colour="Black", size=20),
          axis.title.y = element_text(colour="Black", size=20),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.position = c(1,1),
          legend.justification = c(1,1),
          plot.title = element_text(colour="DarkBlue", size=25))

v


#stats
quantile(Recent$Range, na.rm = TRUE, c(.5, .8,.9,.95, .99,1))
quantile(Recent$CloseToCloseRange, na.rm = TRUE, c(.5, .8,.9,.95, .99,1))

#another visualization of daily and close to close ranges by date
qplot(x=Date, y=Range,
      data=Recent, na.rm=TRUE,
      main="Daily Range",
      xlab="Date", ylab="Range")

qplot(x=Date, y=CloseToCloseRange,
      data=Recent, na.rm=TRUE,
      main="Close To Close Range",
      xlab="Date", ylab="Close to Close Range")

#line chart of range
ggplot(subset(Recent, Date > "2016-01-01"), aes(Date, Range)) + geom_line()


#histogram with mode and standard deviation markers
#node mode and SD are static, need to fix so that they're per year
getmode <- function(v, round=0) {
    uniqv <- round(unique(v), round)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

mymode <- getmode(round(Recent$Range, 0))
mysd <- round(sd(Recent$Range, na.rm = TRUE), 1)
ggplot(Recent, aes(x=round(Range, 0))) +
    geom_histogram(binwidth=1, colour="black", fill="grey") +
    geom_vline(aes(xintercept=mymode, na.rm=T),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=(quantile(Recent$Range, .7, na.rm = TRUE)), na.rm=T),   # Ignore NA values for mean
               color="black", linetype="solid", size=1) +
    facet_grid(. ~ Year) + facet_wrap(~ Year, ncol = 3)

#filtering by year
min(Recent[ which(Recent$Year == 2016),]$Range, na.rm = TRUE)
getmode(Recent[ which(Recent$Year == 2016),]$Range)
count(round(unique(Recent[ which(Recent$Year == 2015),]$Range)))

#add day of week to the data frame
Recent$DayOfWeek <- weekdays(as.Date(Recent$Date))

#plot for Close to Close range
u <- NULL
means <- NULL
maxs <- NULL
means <- aggregate(CloseToCloseRange ~ DayOfWeek, Recent, mean)
maxs <- aggregate(CloseToCloseRange ~ DayOfWeek, Recent, max)
means$CloseToCloseRange <- round(means$CloseToCloseRange, 2)
u <- ggplot(data=Recent, aes(x=DayOfWeek, y=CloseToCloseRange))
meds  <- aggregate(CloseToCloseRange ~ DayOfWeek, Recent, median)
u <- u + geom_jitter(aes(colour=HigherClose)) +
    geom_boxplot(alpha=0.7, outlier.shape = NA,  na.rm = FALSE, size = .5) +
    geom_text(data = means, aes(label = CloseToCloseRange, y = CloseToCloseRange)) +
    geom_text(data = maxs, aes(label = CloseToCloseRange, y =CloseToCloseRange))

u <- u + xlab("Day of Week") +
    ylab("Close To Close Range") +
    ggtitle(paste("SPX Range from One Daily Close To The Next [ through", today, "]")) +
    theme(axis.title.x = element_text(colour="Black", size=20),
          axis.title.y = element_text(colour="Black", size=20),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.position = c(1,1),
          legend.justification = c(1,1),
          plot.title = element_text(colour="DarkBlue", size=25))
u

#range by day of week
round(quantile(Recent[ which(Recent$DayOfWeek == 'Monday'),]$Range, c(.7, .8, .9,  .99, 1)), 1)
round(quantile(Recent[ which(Recent$DayOfWeek == 'Monday'),]$CloseToCloseRange, c(.7, .8, .9,  .99, 1)), 1)
