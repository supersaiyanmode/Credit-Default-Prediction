library(nnet)

data = read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
data_input = data[,3:length(data)-1]
data_output = data[length(data)]

dot <- function(vec1, vec2) { return(sum(vec1*vec2)) }
mag <- function(vec){ return(dot(vec, vec)) }
sigmoid <- function(x) { return(1.0/(1.0+exp(-x))) }
calc_error <- function(yhat, y) { return(mag(yhat-y)); }

logistic_regression <- function(X, Y) {
  w = rep(0, length(X))
  alpha = 0.05
  for (index in 1:50) {
    grad = rep(0, length(X))
    for (i in 1:nrow(X)) {
      grad = grad + (Y[i] - sigmoid(dot(X[i,], w))) * X[i,]
    }
    w = w + alpha * grad
  }
  return(w)
}

next_important_var <- function(cur_var_indices, x, y) {
  remaining_indices = setdiff(1:length(x), cur_var_indices)
  min_index = 0
  min_error = 10^100
  for (i in remaining_indices) {
    cur_x <- x[c(cur_var_indices, i)]
    cur_weights <- logistic_regression(cur_x, y)
    cur_error = calc_errors(as.matrix(cur_x) %*% as.matrix(cur_weights), y)
    if (cur_error < min_error) {
      min_index <- i
      min_error <- cur_error
    }
  }
  cat(paste("Error: ", min_error, "\n"))
  return(min_index)
}

normalize_columns <- function(data, columns) {
  for (i in 1:nrow(data)) {
    s = sum(data[i, columns])
    if (s == 0) {
      data[i, columns] = 0
    } else {
      data[i, columns] = data[i, columns] / s
    }
  }
  return(data)
}

new_data <- normalize_columns(data_input, c(12:17))
new_data <- normalize_columns(new_data, c(18:23))

X = new_data
Y = data_output

next_important_var(c(), X, Y)
require(nnet)
obj <- nnet(X, Y, size=35, rang=0.5, maxit=100000)
ps <- predict(obj, x1=Y)
table(Y, predict(obj, X))

