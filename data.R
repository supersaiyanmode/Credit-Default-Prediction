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