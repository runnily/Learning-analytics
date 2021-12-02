#' This is a linear regression model
#' used for predicting the student performance


linearModelPerformance = function(dataset) {
  set.seed(777)
  sample_size = floor(0.8*nrow(dataset))
  picked = sample(1:nrow(dataset),size = sample_size)
  train_set = dataset[picked,]
  test_set  = dataset[-picked,]
  lsq_fit = lm(performance~., data = train_set)
  summary = summary(lsq_fit)
  train_yhat = predict(lsq_fit, train_set[-9])
  test_yhat = predict(lsq_fit, test_set[-9])
  test_error = mean((test_set$performance - test_yhat)^2)
  train_error = mean((train_set$performance - train_yhat)^2)
  l = list(lsq_fit = lsq_fit, test_error = test_error, train_error = train_error, coefficients = summary$coefficients)
  return(l)
}
