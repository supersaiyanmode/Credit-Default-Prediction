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

predict.dnn<- function(model, data = X.test) {
  # new data, transfer to matrix
  new.data <- data.matrix(data)
  
  # Feed Forwad
  hidden.layer <- sweep(new.data %*% model$W1 ,2, model$b1, '+')
  # neurons : Rectified Linear
  hidden.layer <- pmax(hidden.layer, 0)
  score <- sweep(hidden.layer %*% model$W2, 2, model$b2, '+')
  
  # Loss Function: softmax
  score.exp <- exp(score)
  probs <-sweep(score.exp, 1, rowSums(score.exp), '/') 
  
  # select max possiblity
  labels.predicted <- max.col(probs)
  return(labels.predicted)
}
train.dnn <- function(x, y, traindata=data, testdata=NULL,
                      # set hidden layers and neurons
                      # currently, only support 1 hidden layer
                      hidden=c(6), 
                      # max iteration steps
                      maxit=2000,
                      # delta loss 
                      abstol=1e-2,
                      # learning rate
                      lr = 1e-2,
                      # regularization rate
                      reg = 1e-3,
                      # show results every 'display' step
                      display = 100,
                      random.seed = 1)
{
  # to make the case reproducible.
  set.seed(random.seed)
  
  # total number of training set
  N <- nrow(traindata)
  
  # extract the data and label
  # don't need atribute 
  X <- unname(data.matrix(traindata[,x]))
  Y <- traindata[,y]
  if(is.factor(Y)) { Y <- as.integer(Y) }
  # create index for both row and col
  Y.index <- cbind(1:N, Y)
  
  # number of input features
  D <- ncol(X)
  # number of categories for classification
  K <- length(unique(Y))
  H <-  hidden
  
  # create and init weights and bias 
  W1 <- 0.01*matrix(rnorm(D*H, sd=0.5), nrow=D, ncol=H)
  b1 <- matrix(0, nrow=1, ncol=H)
  
  W2 <- 0.01*matrix(rnorm(H*K, sd=0.5), nrow=H, ncol=K)
  b2 <- matrix(0, nrow=1, ncol=K)
  
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  
  # Training the network
  i <- 0
  while(i < maxit || loss < abstol ) {
    
    # iteration index
    i <- i +1
    
   
    hidden.layer <- sweep(X %*% W1 ,2, b1, '+')
   
    hidden.layer <- pmax(hidden.layer, 0)
    score <- sweep(hidden.layer %*% W2, 2, b2, '+')
    
    # softmax
    score.exp <- exp(score)
    probs <-sweep(score.exp, 1, rowSums(score.exp), '/') 
    
    # compute the loss
    corect.logprobs <- -log(probs[Y.index])
    data.loss  <- sum(corect.logprobs)/batchsize
    reg.loss   <- 0.5*reg* (sum(W1*W1) + sum(W2*W2))
    loss <- data.loss + reg.loss
    
    # display results and update model
    if( i %% display == 0) {
      if(!is.null(testdata)) {
        model <- list( D = D,
                       H = H,
                       K = K,
                       # weights and bias
                       W1 = W1, 
                       b1 = b1, 
                       W2 = W2, 
                       b2 = b2)
        labs <- predict.dnn(model, testdata[,-y])
        accuracy <- mean(as.integer(testdata[,y]) == labs)
        cat(i, loss, accuracy, "n")
      } else {
        cat(i, loss, "n")
      }
    }
    
    # backward ....
    dscores <- probs
    dscores[Y.index] <- dscores[Y.index] -1
    dscores <- dscores / batchsize
    
    
    dW2 <- t(hidden.layer) %*% dscores 
    db2 <- colSums(dscores)
    
    dhidden <- dscores %*% t(W2)
    dhidden[hidden.layer <= 0] <- 0
    
    dW1 <- t(X) %*% dhidden
    db1 <- colSums(dhidden) 
    
    # update ....
    dW2 <- dW2 + reg*W2
    dW1 <- dW1  + reg*W1
    
    W1 <- W1 - lr * dW1
    b1 <- b1 - lr * db1
    
    W2 <- W2 - lr * dW2
    b2 <- b2 - lr * db2
    
  }
  
  # final results
  # creat list to store learned parameters
  # you can add more parameters for debug and visualization
  # such as residuals, fitted.values ...
  model <- list( D = D,
                 H = H,
                 K = K,
                 # weights and bias
                 W1= W1, 
                 b1= b1, 
                 W2= W2, 
                 b2= b2)
  
  return(model)
}


dataset <- read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
new_data <- normalize_columns(dataset, c(13:18))
new_data <- normalize_columns(new_data, c(19:24))
trainset <- new_data[1:15000, 13:25]
testset <- new_data[15001:30000,13:25]
set.seed(1)

# 0. EDA
#summary(iris)
#plot(iris)

# 1. split data into test/train
#samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

# 2. train model

#ir.model <- train.dnn(x=1:4, y=5, traindata=iris[samp,], testdata=iris[-samp,], hidden=6, maxit=2000, display=50)
ir.model <- train.dnn(x=1:12, y=13, traindata=trainset, testdata=testset, hidden=50, maxit=1000, display=50)

# 3. prediction

#labels.dnn <- predict.dnn(ir.model, iris[-samp, -5])
labels.dnn <- predict.dnn(ir.model, testset[,-13])
View(labels.dnn)

# 4. verify the results

#table(iris[-samp,5], labels.dnn)
table(testset[,13], labels.dnn)

#          labels.dnn
#            1  2  3
#setosa     25  0  0
#versicolor  0 24  1
#virginica   0  0 25

#accuracy

#mean(as.integer(iris[-samp, 5]) == labels.dnn)
mean(as.integer(testset[, 13]) == labels.dnn)

# 0.98