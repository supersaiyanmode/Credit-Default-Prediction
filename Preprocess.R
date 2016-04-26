preprocess <- function(dataset, bucket_size){
  dataset <- subset(dataset, select = -c(1))
  # making buckets for the columns for Bill_Amt and Pay_Amt
  df <- subset(dataset, select = -c((2:4),(6:11)))
  for(i in 1:(ncol(df)-1))
  {
    str=paste(colnames(df)[i],"NEW")  
    cat(paste0(str))
    bins<-bucket_size
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
  trainset <- head(df, n=nrow(dataset)*0.8)
  testset <- tail(df, n=nrow(dataset)*0.2)
  
  return(list(train=trainset, test=testset))
}
