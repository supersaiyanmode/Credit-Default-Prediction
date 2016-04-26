source("data.R")

set.seed(1234567890)
library("neuralnet")
require(neuralnet)

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
  
  return(split_data_row(df))
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

run_nnet <- function(dataset, hidden, verbose=TRUE) {
  result=preprocess(dataset)
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

plot_hidden_count <- function(dataset) {
  all_hidden = c(4,7,9,11,15)
  accuracies = c()
  for (hidden in all_hidden) {
    res = run_nnet(dataset, hidden, verbose = FALSE)
    accuracies = c(accuracies, res$accuracy)
  }
  plot(all_hidden, accuracies, type="l")
}

dataset <- read.csv("credit.csv",header=TRUE,skip=1)
#plot_hidden_count(dataset)
run_nnet(dataset, 15, verbose=TRUE)