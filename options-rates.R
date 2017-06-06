library(fOptions)
library(quantmod)
library(tidyverse)
library(sde)

ticker <- "AAPL"
option.type <- "Call"
expiration <- "2018-01-19"

r <- 0.03
b <- 0.03 
n <- 100

# get the current stock price
quote <- getQuote(ticker)

# get the option chains for the given expiration
type <- NULL
option.chain <- getOptionChain(ticker, Exp = expiration)
if (option.type == "Call") 
{
  type <- "c"
  option.chain <- option.chain$calls
}

if (option.type == "Put") 
{
  type <- "p"
  option.chain <- option.chain$puts
}

# get historical data
historical <- getSymbols(ticker, from = Sys.Date() - 90, auto.assign = FALSE)
historical <- data_frame(date = time(historical), price = drop(coredata(Cl(historical))))
historical <- historical %>%
  bind_rows(data_frame(date = Sys.Date(), price = quote$Last))

# 
# compute volatility ----------------------------------------------------------

current.price <- quote$Last
option.chain <- option.chain %>%
  mutate(diff = abs(Strike - current.price)) %>%
  filter(diff == min(diff)) 
option.price <- option.chain$Last
strike.price <- option.chain$Strike
t <- as.numeric(difftime(as.Date(expiration), Sys.Date())) / 365

# compute volatility
sigma <- GBSVolatility(option.price, type, current.price, strike.price, t, r, b)

#
# simulate prices -------------------------------------------------------------

random.walk <- replicate(n, GBM(current.price, r, sigma, t, t * 365), FALSE) %>%
  data_frame(price = .) %>%
  mutate(iterate = row_number()) %>%
  unnest() %>%
  group_by(iterate) %>%
  mutate(day = row_number())

library(plotly)
plot_ly(random.walk, x = ~day, y = ~price, type = 'scatter', mode = 'lines')

random.walk <- random.walk %>%
  group_by(day) %>%
  summarise(sd = sd(price)) %>%
  mutate(sd.plus = current.price + sd) %>%
  mutate(sd.minus = current.price - sd) %>%
  select(-sd) %>%
  gather(bound, price, -day)

plot_ly(random.walk, x = ~day, y = ~price, color = ~bound, type = 'scatter', mode = 'lines')

# combine historical and trajectories
random.walk <- random.walk %>%
  mutate(date = Sys.Date() + day)

plot_ly(historical, x = ~date, y = ~price, type = 'scatter', mode = 'lines') %>%
  add_trace(data = random.walk, x = ~date, y = ~price, color = ~bound)
