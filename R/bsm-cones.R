GetVolatility <- function(quote, strike.price, option.chain, expiration, option.type, r, b)
{
  # parameters to the GBSVolatility function
  option.chain <- option.chain %>%
    filter(Strike == strike.price)
  option.price <- option.chain$Last
  t <- as.numeric(difftime(as.Date(expiration), Sys.Date())) / 365
  
  # compute volatility
  if (option.type == "Call") 
  {
    return(GBSVolatility(option.price, "c", quote$Last, strike.price, t, r, b))
  }
  
  # compute volatility
  if (option.type == "Put") 
  {
    return(GBSVolatility(option.price, "p", quote$Last, strike.price, t, r, b))
  }
}

SimulateTrajectories <- function(quote, r, sigma, expiration, n) 
{
  # parameters to the GMB function
  current.price <- quote$Last
  t <- as.numeric(difftime(as.Date(expiration), Sys.Date())) / 365
  
  # use GBM to simulate the trajectories
  random.walk <- replicate(n, GBM(current.price, r, sigma, t, t * 365), FALSE) %>%
    data_frame(price = .) %>%
    mutate(iterate = row_number()) %>%
    unnest() %>%
    group_by(iterate) %>%
    mutate(day = row_number())
  
  # aggregate the data for plotting
  random.walk <- random.walk %>%
    group_by(day) %>%
    summarise(sd = sd(price)) %>%
    mutate(sd.plus = current.price + sd) %>%
    mutate(sd.minus = current.price - sd) %>%
    select(-sd) %>%
    gather(bound, price, -day)
  
  # add actual dates
  random.walk <- random.walk %>%
    mutate(date = Sys.Date() + day)
  
  # return 
  return(random.walk)
}

GetPriceHistory <- function(ticker, days)
{
  historical <- getSymbols(ticker, from = Sys.Date() - days, auto.assign = FALSE)
  historical <- data_frame(date = time(historical), price = drop(coredata(Cl(historical))))
  
  # return
  return(historical)
}
