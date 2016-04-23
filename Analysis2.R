set.seed(1234567890)
library("neuralnet")
require(neuralnet)
dataset <- read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
new_data <- normalize_columns(dataset, c(13:18))
new_data <- normalize_columns(new_data, c(19:24))
trainset <- new_data[1:15000, 13:25]
testset <- new_data[15001:30000,13:25]
n <- names(trainset[,-c(length(trainset))])
f <- as.formula(paste("default.payment.next.month ~", paste(n[!n %in% "y"], collapse = " + ")))
credit <- neuralnet(f, trainset, hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1)
plot(credit, rep = "best")
temp_test <- subset(testset, select = c(1:length(testset)-1))

credit.results <- compute(credit, temp_test)
results <- data.frame(actual = testset$default.payment.next.month, prediction = credit.results$net.result)
diff=sum(abs(results$actual-results$prediction))
results$prediction <- round(results$prediction)
View(results$prediction)
View(results)


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


