set.seed(1234567890)
library("neuralnet")
require(neuralnet)
dataset <- read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
#new_data <- normalize_columns(dataset, c(13:18))
#new_data <- normalize_columns(new_data, c(19:24))
#trainset <- new_data[1:15000, 13:25]
#testset <- new_data[15001:30000,13:25]
result=preprocess(dataset)
trainset=result$train
testset=result$test
n <- names(trainset[,-c(1)])
f <- as.formula(paste("default ~", paste(n[!n %in% "y"], collapse = " + ")))
credit <- neuralnet(f, trainset, hidden = 4, lifesign = "minimal", 
                    linear.output = FALSE, threshold = 0.1)
plot(credit, rep = "best")
temp_test <- subset(testset, select = c(2:ncol(testset)))

credit.results <- compute(credit, temp_test)
results <- data.frame(actual = testset$default, prediction = credit.results$net.result)
diff=sum(abs(results$actual-results$prediction))
results$prediction <- round(results$prediction)
View(results$prediction)
View(results)
print(table(results$actual,results$prediction))



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


