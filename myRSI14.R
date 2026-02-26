myRSI14 <- function (price){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- 1
    } else {
      D[i] <- 1
    }
    if (i>14){
      AvgUp <- mean(U[(i-14+1):i])
      AvgDn <- mean(D[(i-14+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}

getSymbols("AAPL")
rsi <- myRSI(Cl(AAPL), n=14)
tail(rsi,n=3)

Cl(AAPL)
