library(data.tree)

data = read.csv("credit.csv",header=TRUE,skip=1)
pairs(data[,12:17])

entropy <- function(responses) {
  sum = 0
  for (unique_val in unique(responses)) {
    prob = length(responses[responses==unique_val]) / length(responses)
    sum = sum - prob * log(prob)
  }
  return(sum)
}

id3 <- function(node, features, responses) {
  if (length(unique(responses)) == 1) {
    node$splitBy = NULL
    node$response = unique(responses)
    return()
  }
  
  
  min_feature_entropy = 10e10
  min_feature_entropy_index = -1
  for (feature_index in 1:ncol(features)) {
    sum_entropy = 0
    for (unique_val in unique(features[,feature_index])) {
      sum_entropy = sum_entropy + entropy(features[features[,feature_index] == unique_val, c(-feature_index)])
    }
    if (min_feature_entropy < sum_entropy) {
      min_feature_entropy =  sum_entropy
      min_feature_entropy_index = feature_index
    }
  }
  
  node$splitBy = min_feature_entropy_index
  for (unique_val in unique(features)) {
    child = node$AddChild(colnames(features)[min_feature_entropy_index])
    id3(child,
        features[features[min_feature_entropy_index]==unique_val, -c(min_feature_entropy_index)], 
        responses[features[min_feature_entropy_index]==unique_val])
  }
}

dtree <- function(features, responses) {
  node = Node$new("root")
  id3(node, features, responses)
  return(node)
}
data("mushroom")

res = dtree(mushroom[,1:3], mushroom[4])
