#this calculates a 1 day standard deviation move based on the current price and an array of
#implied volatilities

computeMove <-function(currprice, iv, numdays, sd = 1)
{
    round(currprice * (iv/100) * sqrt(numdays/365.25) * sd, 2)
}

currprice <- 2405
sd <- 1.7
iv <- 7.97


#calculate a 1 day 1.7 standard deviation move to determine today's
#downside adjustment point (where to buy a long put)
mySym <- NULL
mySym <- matrix()
mySym$currprice <- currprice
mySym$move <- computeMove(currprice, iv, 1, sd)
mySym$down <- currprice - m$move
mySym$up <- currprice + m$move

#show results
mySym
