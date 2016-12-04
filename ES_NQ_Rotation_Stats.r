library(quantmod)

esdata <- read.csv("C:\\Users\\joseph\\git\\r-trading_homework\\input_data\\ES_Weekly_Export.csv", header = TRUE)
range <- data$High - data$Low
esdata <- cbind(esdata, range)

hist(range)
sd(range)
quantile(tail(range, 20), c(.70, .95))
mean(tail(range, 20))


prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
prevrows(range, 4)


########################################################
## zig zag rotation stats
nqzig <- read.csv("C:\\Users\\joseph\\git\\r-trading_homework\\input_data\\NQ_ZIG.csv", header = TRUE)


print("Biggest negative rotation")
min(nqzig$zzChange)
print("Biggest positive rotation")
max(nqzig$zzChange)

zzpos <- nqzig$zzChange[nqzig$zzChange>0 & !is.na(nqzig$zzChange)]
print("1 and 2 STD Deviation positive rotations")
quantile(zzpos, c(.6827, .9545))

print("Positive Point of control")
names(sort(-table(zzpos)))[1]

zzneg <- nqzig$zzChange[nqzig$zzChange<0 & !is.na(nqzig$zzChange)]
print("1 and 2 STD Deviation negative rotations")
quantile(abs(zzneg), c(.6827, .9545))

print("Negative Point of control")
names(sort(-table(zzneg)))[1]



