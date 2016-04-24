library(Hmisc)
library(MASS)
library(rpart)
dataset <- read.csv("/home/rudrani/python-neural-network/backprop/credit.csv",header=TRUE,skip=1)
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

# prediction for thr decision tree

colnames(df)[1]=c("default")
df2 <- subset(df, select = -c(1))
names(df) <- sub(" ", ".", names(df))
trainset <- df[1:15000,]
testset <- df[15001:30000,]
n <- names(df[,-c(1)])
f <- as.formula(paste("default~", paste(n[!n %in% "y"], collapse = " + ")))
creditTree <- rpart(f,data=trainset, method = 'class')
plot(creditTree)
text(creditTree, pretty=0)
p <- predict(creditTree,testset[,-c(1)],type="class")
View(table(p,testset[,c(1)]))


