GetVolatility <- function(quote, option.chain, expiration, option.type, r, b)
{
  # parameters to the GBSVolatility function
  current.price <- quote$Last
  option.chain <- option.chain %>%
    mutate(diff = abs(Strike - current.price)) %>%
    filter(diff == min(diff)) 
  option.price <- option.chain$Last
  strike.price <- option.chain$Strike
  t <- as.numeric(difftime(as.Date(expiration), Sys.Date())) / 365
  
  # compute volatility
  if (option.type == "Call") 
  {
    return(GBSVolatility(option.price, "c", current.price, strike.price, t, r, b))
  }
  
  # compute volatility
  if (option.type == "Put") 
  {
    return(GBSVolatility(option.price, "p", current.price, strike.price, t, r, b))
  }
}

SimulateTrajectories <- function(quote, r, sigma, expiration, n) 
{
  # parameters to the GMB function
  current.price <- quote$Last
  t <- as.numeric(difftime(as.Date(expiration), Sys.Date()))
  
  # use GBM to simulate the trajectories
  random.walk <- replicate(n, GBM(current.price, r, sigma, t / 365, t), FALSE) %>%
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
  historical <- historical %>%
    bind_rows(data_frame(date = Sys.Date(), price = quote$Last))
  
  # return
  return(historical)
}