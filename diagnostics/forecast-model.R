#' A forecast model to predict the performance based on a demographic
#' The default is predicting different types of regions


forecast = function(data, demographics = c("Oceania", "Europe", "Americas", "Asia", "Africa")) {
  dfs = NULL
  for (i in seq(1:length(demographics))) {
    means = (avgPerformance("region") %>% filter(type == demographics[i]))[["mean"]]
    model = arima(means)
    forecasting = predict(model, 1)
    df = data.frame(performance = c(forecasting$pred[1]), type = demographics[i])
    dfs = rbind(dfs, df)
  }
  return(dfs)
}