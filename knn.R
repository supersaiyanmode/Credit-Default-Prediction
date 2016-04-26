data = read.csv("credit.csv",header=TRUE,skip=1)
train_features <- data[1:29900, 2:(ncol(data)-1)]
train_responses <- data[1:29900, ncol(data)]
test_features <- data[29901:30000, 2:(ncol(data)-1)]
test_responses <- data[29901:30000,ncol(data)]

col_sd <- function(data) {
  #Calculate standard deviation for each of the features.
  features_sd = c()
  for (i in 1:ncol(data)) {
    features_sd = c(features_sd, sd(as.vector(data[,i])))
  }
  return(features_sd)
}

knn <- function(data, responses, features_sd, row) {
  min_error = 10e10;
  min_error_index = -1
  for( i in 1:nrow(data)) {
    error = sum(((data[i,] - row)/features_sd)^2)
    if (min_error >error) {
      min_error_index = i
      min_error = error
    }
  }
  return(responses[min_error_index])
}

test_knn <- function(train_features, train_responses, test_features, test_responses) {
  features_sd = col_sd(train_features)
  
  error = c()
  
  for (i in 1:nrow(test_features)) {
    cat(paste("Processing test data: #", i, "\n"))
    res = knn(train_features, as.matrix(train_responses), features_sd, test_features[i,])
    error = c(error, res)
  }
  
  return(error)
}

cat("Error: ")
error = test_knn(train_features, train_responses, test_features, test_responses)
print(table(error, test_responses))
