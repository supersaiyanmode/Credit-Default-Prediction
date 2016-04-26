set.seed(1234567890)
source("data.R")
library("neuralnet")
require(neuralnet)

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

run_nnet <- function(dataset, hidden, bucket_size, verbose=TRUE) {
  result=preprocess(dataset, bucket_size)
  trainset=result$train
  testset=result$test
  n <- names(trainset[,-c(1)])
  f <- as.formula(paste("default ~", paste(n[!n %in% "y"], collapse = " + ")))
  credit <- neuralnet(f, trainset, hidden = hidden, lifesign = "minimal", 
                      linear.output = FALSE, threshold = 0.1)
  if (verbose) {
    plot(credit, rep = "best")
  }
  
  temp_test <- subset(testset, select = c(2:ncol(testset)))
  credit.results <- compute(credit, temp_test)
  results <- data.frame(actual = testset$default, prediction = credit.results$net.result)
  diff=sum(abs(results$actual-results$prediction))
  results$prediction <- round(results$prediction)
  tbl = table(results$actual,results$prediction)
  
  if (verbose) {
    View(results$prediction)
    View(results)
    print(tbl)
  }
  return(list(
    accuracy=(tbl[1,1] + tbl[2,2])/sum(tbl),
    table=tbl
  ))
}

plot_bucket_size_nnet <- function(dataset) {
  all_buckets = c(3,6,10,15,20,30)
  accuracies = c()
  for (bucket_size in all_buckets) {
    res = run_nnet(dataset, 5, bucket_size, verbose = FALSE)
    accuracies = c(accuracies, res$accuracy)
  }
  plot(all_buckets, accuracies, type="l")
}

plot_hidden_count_nnet <- function(dataset) {
  all_hidden = c(4,7,9,11,15)
  accuracies = c()
  for (hidden in all_hidden) {
    set.seed(1234567890)
    res = run_nnet(dataset, hidden, 15, verbose = FALSE)
    accuracies = c(accuracies, res$accuracy)
  }
  plot(all_hidden, accuracies, type="l")
}

dataset <- get_credit_dataset()
#plot_bucket_size_nnet(dataset)
run_nnet(dataset, 10, 15, verbose=TRUE)
