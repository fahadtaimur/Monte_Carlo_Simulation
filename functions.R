# Monte-Carlo Simulation

stock_price_model <- function(cp, days, gr, vol, forecast_days){
  
  # cp - current price  / days - annual trading days / gr - growth rate / vol - annual volatility
  
  # calculate the daily mean 
  daily_mean <- (gr - 0.5 * (vol^2)) / days
  daily_std <- vol/sqrt(days)
  
  # the price at the beginning (current price)
  begin_price <- cp
  
  # stock price in 3 months
  for(i in 1:forecast_days){
    # generate normally distributed random number
    growth_factor <- rnorm(n = 1, mean = daily_mean, sd = daily_std)
    # calculate ending price
    end_price <- exp(growth_factor) * begin_price
    # set the next beginning price to the latest ending price
    begin_price <- end_price
  }
  
  # return the ending price at the end of the trading
  return(end_price)
  
}

hist_plotter <- function(df, current_price){
  plotly::ggplotly(
    df %>%
      as.tibble() %>%
      mutate(label = case_when(
        value > current_price ~ "Buy",
        TRUE ~ "Not Buy"
      )) %>%
      ggplot(aes(value, fill=label)) + 
      geom_histogram(show.legend = FALSE, bins = 50) +
      theme_bw() +
      geom_vline(aes(xintercept=mean(value))) +
      labs(x = "Price of Stock", y= "Count", title = "Stock Price Estimates")
  )
}

density_plotter <- function(df, current_price){
  plotly::ggplotly(
    df %>%
      as.tibble() %>%
      mutate(label = case_when(
        value > current_price ~ "Buy",
        TRUE ~ "Not Buy"
      )) %>%
      ggplot(aes(value, fill=label)) + 
      geom_density(show.legend = FALSE, fill = "blue") +
      theme_bw() +
      geom_vline(aes(xintercept=mean(value))) +
      labs(x = "Price of Stock", title = "Stock Price Estimates")
  )
}




