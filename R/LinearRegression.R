#' Training and Test Creation
#'
#' @details Sets parameters and response set
#' @param dataset Dataset regression is used on
#' @param response Response value of the dataset
#' @param split %of dataset goes to training
#' @return training and testing sets
#' @export
TrainTest <- function(dataset, response, split){
  split <- split/100

  x_parameters <<- dataset[,!(names(dataset) %in% response)]
  x_parameters <<- data.matrix(x_parameters)
  y_response <<- dataset[[response]]

  #% of the sample size for training set
  train <- sample(1:nrow(dataset), split * nrow(dataset))
  test <- setdiff(1:nrow(dataset), train)

  #Training Set Data
  X.train <<- data.matrix(dataset[train,!(names(dataset) %in% response)])
  Y.train <<- dataset[train, response]

  #Test Set Data
  X.test <<- data.matrix(dataset[test,!(names(dataset) %in% response)])
  Y.test <<- dataset[test, response]

  #Complete Training and Test Sets
  trainset <<- dataset[train, ]
  testset <<- dataset[test, ]
}


#' OLS Regression
#'
#' @details Returns OLS Regression
#' @param dataset Dataset regression is used on
#' @param response Response value of the dataset
#' @return OLS summary
#' @export
OLSRegression <- function(dataset, response){
  OLS <- lm((paste(response, "~ .")) ,dataset)
  return(summary(OLS))
}


#' Ridge Regression
#'
#' @details Returns Ridge Regression
#'
#' @param dataset Dataset regression is used on
#' @param resposne Response value of the dataset
#' @param split %of dataset goes to training
#' @return Ridge Summary
#' @export
RidgeRegression <- function(dataset, response, split){

  split <- split/100
  grid.lambda <- 10^seq(10, -2, length=100)
  x_parameters <- dataset[,!(names(dataset) %in% response)]
  x_parameters <- data.matrix(x_parameters)
  y_response <- dataset[[response]]
  train <- sample(1:nrow(dataset), split * nrow(dataset))
  test <- setdiff(1:nrow(dataset), train)
  X.train <- data.matrix(dataset[train,!(names(dataset) %in% response)])
  Y.train <- dataset[train, response]
  X.test <- data.matrix(dataset[test,!(names(dataset) %in% response)])
  Y.test <- dataset[test, response]

  ridge.model <- glmnet(x_parameters, y_response, alpha = 0, lambda = grid.lambda, family = "gaussian")

  set.seed(123)
  ell2.norm <- numeric()
  for (i in 1:length(grid.lambda)){
    ell2.norm[i] <- sqrt(sum(coef(ridge.model)[-1, i]^2))
  }
  plot(x = grid.lambda, y = ell2.norm, xlab = expression(Lambda), ylab = "L2 Norm", xlim = c(10,10000))

  #Min MSPE
  ridge.model.train <- glmnet(X.train, Y.train, alpha=0, lambda = grid.lambda)
  cv.out <- cv.glmnet(X.train, Y.train, alpha=0, family = "gaussian")
  best.lambda <- cv.out$lambda.min
  best.lambda
  plot(cv.out)
  abline(v = log(best.lambda), col="blue")
  cat("Minimum Ridge MSPE:", min(cv.out$cvm), "\n")

  #Final Model
  final.model <- glmnet(x_parameters, y_response, alpha = 0, lambda = best.lambda)
  Coef.Ridge <<- coef(final.model)[1:ncol(dataset), ]
  cat("Coefficient of Ridge Regression:")
  print(Coef.Ridge)
  cat(sep = "\n")

  #Average MSPE
  ridge.pred <- predict(ridge.model.train, s = best.lambda, newx = X.test)
  mspe.ridge <<- mean((ridge.pred - Y.test)^2)
  cat("Average Ridge MSPE:", mspe.ridge, "\n")
}

#' Lasso Regression
#'
#' @details Returns Lasso Regression
#' @param dataset Dataset regression is used on
#' @param response Response value of the dataset
#' @return OLS summary
#' @export
LassoRegression <- function(dataset, response, split){

  split <- split/100
  grid.lambda <- 10^seq(10, -2, length=100)
  x_parameters <- dataset[,!(names(dataset) %in% response)]
  x_parameters <- data.matrix(x_parameters)
  y_response <- dataset[[response]]
  train <- sample(1:nrow(dataset), split * nrow(dataset))
  test <- setdiff(1:nrow(dataset), train)
  X.train <- data.matrix(dataset[train,!(names(dataset) %in% response)])
  Y.train <- dataset[train, response]
  X.test <- data.matrix(dataset[test,!(names(dataset) %in% response)])
  Y.test <- dataset[test, response]

  lasso.model <- glmnet(x_parameters, y_response, alpha = 0, lambda = grid.lambda, family = "gaussian")

  set.seed(123)
  ell2.norm <- numeric()
  for (i in 1:length(grid.lambda)){
    ell2.norm[i] <- sqrt(sum(coef(lasso.model)[-1, i]^2))
  }
  plot(x = grid.lambda, y = ell2.norm, xlab = expression(Lambda), ylab = "L2 Norm", xlim = c(10,10000))

  #Min MSPE
  lasso.model.train <- glmnet(X.train, Y.train, alpha=0, lambda = grid.lambda)
  cv.out <- cv.glmnet(X.train, Y.train, alpha=0, family = "gaussian")
  best.lambda <- cv.out$lambda.min
  best.lambda
  plot(cv.out)
  abline(v = log(best.lambda), col="blue")
  cat("Minimum Lasso MSPE:", min(cv.out$cvm), "\n")

  #Final Model
  final.model <- glmnet(x_parameters, y_response, alpha = 0, lambda = best.lambda)
  Coef.Lasso <<- coef(final.model)[1:ncol(dataset), ]
  cat("Coefficient of Lasso Regression:")
  print(Coef.Lasso)
  cat(sep = "\n")

  #Average MSPE
  lasso.pred <- predict(lasso.model.train, s = best.lambda, newx = X.test)
  mspe.lasso <<- mean((lasso.pred - Y.test)^2)
  cat("Average Lasso MSPE:", mspe.lasso, "\n")
}
