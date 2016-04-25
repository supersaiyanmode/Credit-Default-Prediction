library(class)
data = read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
train_features <- data[1:15000, 2:(ncol(data)-1)]
train_responses <- data[1:15000, ncol(data)]
test_features <- data[15001:30000, 2:(ncol(data)-1)]
test_responses <- data[15001:30000,ncol(data)]
val=knn(train_features, test_features, train_responses, k = 10, prob = FALSE)
print(table(val,test_responses))