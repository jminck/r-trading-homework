# this looks at a 1 standard deviation move across a variety of
# timeframes and implied volatilities to produce a table like the one below

#   iv price move_1d move_5d move_10d move_20d move_1y
# 1  9  2405   11.33   25.32    35.81    50.65  179.79
# 2 10  2405   12.58   28.14    39.79    56.28  199.77
# 3 15  2405   18.88   42.21    59.69    84.42  299.65
# 4 20  2405   25.17   56.28    79.59   112.56  399.53
# 5 25  2405   31.46   70.35    99.49   140.69  499.41
# 6 30  2405   37.75   84.42   119.38   168.83  599.30
# 7 40  2405   50.34  112.56   159.18   225.11  799.06
# 8 50  2405   62.92  140.69   198.97   281.39  998.83

computeMove <-function(currprice, iv, numdays, sd = 1)
{
    round(currprice * (iv/100) * sqrt(numdays/365.25) * sd, 2)
}

currprice <- 2405
iv <- c(9,10, 15, 20, 25, 30, 40, 50)

mydf <- data.frame(iv=iv)
mydf$price <- rep(currprice)
mydf$move_1d <- computeMove(currprice, iv, 1)
mydf$move_5d <- computeMove(currprice, iv, 5)
mydf$move_10d <- computeMove(currprice, iv, 10)
mydf$move_20d <- computeMove(currprice, iv, 20)
mydf$move_1y <- computeMove(currprice, iv, 252)
mydf
