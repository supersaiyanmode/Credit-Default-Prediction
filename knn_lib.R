library(class)

source('data.R')

run_knn <- function(train_features, train_responses, test_features, test_responses, k) {
  result = knn(train_features, test_features, train_responses, k = k, prob = FALSE)
  t = table(result,test_responses)
  return(list(
    confusion=t,
    accuracy=(t[1,1] + t[2,2])/sum(t)
  ))
}

plot_best_k <- function(train_features, train_responses, test_features, test_responses) {
  all_k = seq(10,150,10)
  accuracies = c()
  for (k in all_k) {
    res = run_knn(train_features, train_responses, test_features, test_responses, k)
    accuracies = c(accuracies, res$accuracy)
  }
  plot(all_k, accuracies, type="l", xlab="K values", ylab="Accuracy", main="Plot of accuracy against 'K'")
}

dataset <- get_credit_dataset()
data = split_data(dataset)
#plot_best_k(data$train_features, data$train_responses, data$test_features, data$test_responses)

result = run_knn(data$train_features, data$train_responses, data$test_features, data$test_responses, 70)
print(result$confusion)
print(result$accuracy)
