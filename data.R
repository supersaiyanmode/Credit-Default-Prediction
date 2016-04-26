get_credit_dataset <- function() {
  dataset = read.csv("credit.csv",header=TRUE,skip=1)
  ones = dataset[dataset$default.payment.next.month == 1,]
  zeroes = dataset[dataset$default.payment.next.month == 0,]
  
  res = rbind(
    ones[sample(nrow(ones), 6000),],
    zeroes[sample(nrow(ones), 6000),]
  )
  return(res[sample(nrow(res)),])
}


split_data <- function(data) {
  pos = nrow(data) * 0.8
  return(list(
    train_features=data[1:pos, 2:(ncol(data)-1)],
    train_responses=data[1:pos, ncol(data)],
    test_features=data[(pos + 1):nrow(data), 2:(ncol(data)-1)],
    test_responses=data[(pos + 1):nrow(data),ncol(data)]
  ))
}

split_data_row <- function(data) {
  return(list(
    train=head(data, n=0.8*nrow(data)),
    test=tail(data, n=0.2*nrow(data))
  ))
}