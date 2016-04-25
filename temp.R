library(Hmisc)
library(MASS)
library(rpart)
library(data.tree)


preprocess <-function(dataset){
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

            # prediction for thr decision tree
            if(FALSE) {
                    n <- names(df[,-c(1)])
                f <- as.formula(paste("default~", paste(n[!n %in% "y"], collapse = " + ")))
                    creditTree <- rpart(f,data=trainset, method = 'class')
                    plot(creditTree)
                        text(creditTree, pretty=0)
                        p <- predict(creditTree,testset[,-c(1)],type="class")
                            View(table(p,testset[,c(1)]))
                          }
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

id3 <- function(node, features, responses) {
      if (length(unique(responses)) == 1) {
              node$splitBy = NULL
    node$response = responses[1]
        return()
      }
  
  if (node$depth >= 5) {
          node$splitBy = NULL
      if (nrow(as.matrix(responses[responses==1])) > nrow(as.matrix(responses[responses==0]))) {
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
                                  responses[features[min_feature_entropy_index]==unique_val])
            }
}

dtree_train <- function(features, responses) {
      node = Node$new("root")
  node$depth = 0
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

dataset <- read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)

retval = preprocess(dataset)
trainset = retval$train
testset = retval$test

model = dtree_train(trainset[,2:ncol(trainset)], as.matrix(trainset[,c(1)]))
print(model, "splitBy", "response")
results = dtree_test(model, as.matrix(trainset[,2:ncol(trainset)]))
View(results)
results=cbind(testset[,c(1)],results)
print(table(results[,1],results[,2]))

