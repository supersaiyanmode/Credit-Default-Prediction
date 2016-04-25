library(data.tree)

data = read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)

entropy <- function(responses) {
  sum = 0
  for (unique_val in unique(responses)) {
    prob = length(responses[responses==unique_val]) / length(responses)
    sum = sum - (prob * log(prob))
  }
  return(sum)
}

id3 <- function(node, features, responses) {
  if (nrow(unique(responses)) == 1) {
    node$splitBy = NULL
    node$response = responses[1,1]
    return()
  }
  if (node$depth >= 6) {
    node$splitBy = NULL
    if (nrow(responses[responses==1]) > nrow(responses[responses==0])) {
      node$response = 1
    } else {
      node$response = 0
    }
    return()
  }
  
  min_feature_entropy = 10e10
  min_feature_entropy_index = -1
  for (feature_index in 1:ncol(features)) {
    sum_entropy = 0
    for (unique_val in unique(features[,feature_index])) {
      subset = responses[features[,feature_index] == unique_val,]
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
        responses[features[min_feature_entropy_index]==unique_val,,drop=FALSE])
  }
}

dtree_train <- function(features, responses) {
  node = Node$new("root")
  node$depth=0
  id3(node, features, as.vector(responses))
  return(node)
}

dtree_predict <- function(node, row) {
  if (is.null(node$splitBy)) {
    return(node$response)
  }
  return(dtree_predict(get(paste(node$splitBy,"=",get(node$splitBy, row)), node), row))
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

data("mushroom")
#mushroom = as.table(mushroom)
View(mushroom)
model = dtree_train(mushroom[,1:3], mushroom[4])
results = dtree_test(model, mushroom[,1:3])
View(results)
#print(results, "splitBy", "response")
#cat("Results:\n")
#print(results)
#print(mushroom[4] - results)