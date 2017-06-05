library(fOptions)
library(quantmod)
library(tidyverse)
library(sde)

ticker <- "AAPL"
expiration <- "2018-01-19"
quote <- getQuote(ticker)
option.chain <- getOptionChain(ticker, Exp = expiration)

# we need some info to compute the volatility
# we need to get this automatically from the option chain info or let the user input this
# probably build a Shiny app that pull the option chain and let's the user input

S <- quote$Last
price <- 10.75
X <- 155
t <- as.numeric(difftime(as.Date(expiration), Sys.Date())) / 365

# compute volatility
GBSVolatility(price = price, TypeFlag = "c", S = S, X = X, Time = t, r = 0.03, b = 0.03)

# simulate prices

dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt)


for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}

##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
     ylab="Price P(t)",xlab="time t")
for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
