library(Hmisc)
library(MASS)
library(rpart)
library(data.tree)

preprocess <- function(dataset){
  dataset <- subset(dataset, select = -c(1))
  # making buckets for the columns for Bill_Amt and Pay_Amt
  df <- subset(dataset, select = -c((2:4),(6:11)))
  for(i in 1:(ncol(df)-1))
  {
    str=paste(colnames(df)[i],"NEW")  
    cat(paste0(str))
    bins<-15
    cutpoints<-unique(quantile(df[,i],(0:bins)/bins))
    df[,str]<-NA
    df[,str]=as.numeric(cut(df[,i],cutpoints,include.lowest=TRUE))
  }

  df <- subset(df, select = -c(1:14))
  df1<- subset(dataset, select = -c(1,5,(12:24)))
  df=cbind(df,df1)
  colnames(df)[1]=c("default")
  df2 <- subset(df, select = -c(1))
  names(df) <- sub(" ", ".", names(df))
  trainset <- df[1:15000,]
  testset <- df[15001:30000,]

  return(list(train=trainset, test=testset))
}

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

train_test <- function(trainset, testset, cutoff) {
  model = dtree_train(trainset[,2:ncol(trainset)], as.matrix(trainset[,c(1)]), cutoff)
  #print(model, "splitBy", "response")
  results = dtree_test(model, testset[,2:ncol(testset)])
  #View(results)
  results=cbind(testset[,c(1)],results)
  ret = table(results[,1],results[,2])
  return((ret[1,1]+ret[2,2])/(ret[1,1] + ret[1,2] + ret[2,1] + ret[2,2]))
}

dataset <- read.csv("credit.csv",header=TRUE,skip=1)
ret = preprocess(dataset)
trainset = ret$train
testset = ret$test

for (i in 1:15) {
  accuracy = train_test(trainset, testset, i)
  cat(paste("Accuracy with cutoff=", i, " is: ", accuracy, "\n"))
}