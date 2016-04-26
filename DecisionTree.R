library(Hmisc)
library(MASS)
library(rpart)
library(data.tree)

source("data.R")
source("Preprocess.R")

entropy <- function(responses) {
  sum = 0
  for (unique_val in unique(responses)) {
    prob = length(responses[responses==unique_val]) / length(responses)
    sum = sum - (prob * log(prob))
  }
  return(sum)
}

get_max_prob <- function(responses) {
  if (nrow(as.matrix(responses[responses==1])) > nrow(as.matrix(responses[responses==0]))) {
    return(1)
  } else {
    return(0)
  }
}

id3 <- function(node, features, responses, cutoff) {
  if (length(unique(responses)) == 1) {
    node$splitBy = NULL
    node$response = responses[1]
    return()
  }
  
  if (node$depth >= cutoff) {
    node$splitBy = NULL
    node$response = get_max_prob(responses)
    return()
  }
  
  #set temporary responses in case we encounter new edge in testing phase
  node$response = get_max_prob(responses)
  
  min_feature_entropy = 10e10
  min_feature_entropy_index = -1
  for (feature_index in 1:ncol(features)) {
    sum_entropy = 0
    for (unique_val in unique(features[,feature_index])) {
      subset = responses[features[,feature_index] == unique_val]
      sum_entropy = sum_entropy + length(subset) / nrow(features) * entropy(subset)
    }
    if (min_feature_entropy > sum_entropy) {
      min_feature_entropy =  sum_entropy
      min_feature_entropy_index = feature_index
    }
  }
  
  node$splitBy = colnames(features)[min_feature_entropy_index]
  for (unique_val in unique(features[,min_feature_entropy_index])) {
    child = node$AddChild(as.character(paste(colnames(features)[min_feature_entropy_index],"=",unique_val)))
    child$depth = node$depth + 1
    id3(child,
        features[features[min_feature_entropy_index]==unique_val, -c(min_feature_entropy_index)], 
        responses[features[min_feature_entropy_index]==unique_val],
        cutoff)
  }
}

dtree_train <- function(features, responses, cutoff) {
  node = Node$new("root")
  node$depth = 0
  id3(node, features, as.vector(responses), cutoff)
  return(node)
}

dtree_predict <- function(node, row) {
  if (is.null(node$splitBy)) {
    return(node$response)
  }
  if (exists(paste(node$splitBy,"=",get(node$splitBy, row)), node)) {
    return(dtree_predict(get(paste(node$splitBy,"=",get(node$splitBy, row)), node), row))
  } else {
    return(node$response)
  }
}

dtree_test <- function(node, features) {
  res = c()
  for (i in 1:nrow(features)) {
    row = features[i,]
    ret = dtree_predict(node, row)
    res = c(res, ret)
  }
  return(res)
}

train_test <- function(trainset, testset, cutoff, verbose=FALSE) {
  model = dtree_train(trainset[,2:ncol(trainset)], as.matrix(trainset[,c(1)]), cutoff)
  if (verbose) {
    print(model, "splitBy", "response")
  }
  results = dtree_test(model, testset[,2:ncol(testset)])
  #View(results)
  results=cbind(testset[,c(1)],results)
  ret = table(results[,1],results[,2])
  if (verbose) {
    print(ret)
  }
  accuracy = (ret[1,1]+ret[2,2])/(ret[1,1] + ret[1,2] + ret[2,1] + ret[2,2])
  return(list(tn=ret[1,1], tp=ret[2,2], accuracy=accuracy))
}

plot_bucket_size <- function(dataset) {
  result = c()
  bucket_sizes = c(seq(1,10), seq(12, 21, 3))
  for (i in bucket_sizes) {
    ret = preprocess(dataset, i)
    trainset = ret$train
    testset = ret$test
    res = train_test(trainset, testset, 4)
    cat(paste("Accuracy with bucket_size=, ", i, " is: ", res$accuracy, "\n"))
    result = c(result, res)
  }
  plot(bucket_sizes, result[seq(3, length(result), 3)], type="l", ylab="Overall Accuracy", xlab="Decision tree Bucket size")
}

plot_cutoff <- function(dataset) {
  result = c()
  cut_offs = seq(1,15)
  for (cutoff in cut_offs) {
    ret = preprocess(dataset, 5)
    trainset = ret$train
    testset = ret$test
    res = train_test(trainset, testset, cutoff)
    cat(paste("Accuracy with cut_off= ", cutoff, " is: ", res$accuracy, "\n"))
    result = c(result, res)
  }
  plot(cut_offs, result[seq(3, length(result), 3)], type="l", ylab="Overall Accuracy", xlab="Decision tree Cut Off")
}

run_dtree <- function(dataset) {
  ret = preprocess(dataset, 5)
  trainset = ret$train
  testset = ret$test
  res = train_test(trainset, testset, 3, verbose=TRUE)
  cat(paste("Accuracy: ", res$accuracy, "\n"))
}

dataset <- get_credit_dataset()
#plot_bucket_size(dataset)
#plot_cutoff(dataset)
run_dtree(dataset)