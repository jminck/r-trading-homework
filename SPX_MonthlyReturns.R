require(quantmod)
require(TTR)
library(dplyr)
library(ggplot2)

#save today's date for labeling plot graphics
today <- Sys.Date()

#change symbol here
sym = "GSPC"

startDate <- '1990-01-01'
#S&P 500 ia ^GSPC, RUT is ^RUT, VIX is ^VIX
#note, if you are working with a normal stock symbol, not and index, then
#you do not need the leading ^ that is inserted by the line below
symbol <- paste("^", sym, sep="")
getSymbols(symbol, from = startDate)

startDate <- index(GSPC)[1]
m <- to.monthly(GSPC)
m$ReturnPct <- round((m$GSPC.Close - m$GSPC.Open) / m$GSPC.Open, 4) * 100
m$Month <- .indexmon(m)
aggregate(data = m, ReturnPct ~ Month, mean)

#plot for Close to Close range
myPlot <- NULL
means <- NULL
maxs <- NULL
means <- round(aggregate(ReturnPct ~  Month, m, mean), 2)
maxs <- aggregate(ReturnPct ~  Month, m,  max)
mins <- aggregate(ReturnPct ~  Month, m,  min)

myPlot <- ggplot(data=m, aes(x=Month, y=ReturnPct))
meds  <- aggregate(ReturnPct ~ Month, m, median)
myPlot <- myPlot + geom_jitter(aes(colour=ReturnPct)) +
    geom_boxplot(alpha=0.7, outlier.shape = NA,  na.rm = FALSE, size = .5, aes(group=Month)) +
    geom_text(data = means, aes(label = ReturnPct, y = ReturnPct)) +
    geom_text(data = maxs, aes(label = ReturnPct, y =ReturnPct)) +
    geom_text(data = mins, aes(label = ReturnPct, y =ReturnPct))

myPlot <- myPlot + xlab("Month") +
    ylab("Return Percentage") +
    ggtitle(paste("SPX monthly return [ from", startDate, "through", today, "]")) +
    theme(axis.title.x = element_text(colour="Black", size=20),
          axis.title.y = element_text(colour="Black", size=20),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.position = c(1,1),
          legend.justification = c(1,1),
          plot.title = element_text(colour="DarkBlue", size=25))

#Show plot
myPlot
