str(iris)
data <- iris
set.seed(1234)
idx = sample(1:nrow(data),nrow(data)*0.7)
idx
train <- data[idx,]
test <- data[-idx,]

train_x <- train[-5]
train_y <- train[5]

test_x <- test[-5]
test_y <- test[5]


nor <- function(x){
  return( (x-min(x))/(max(x)-min(x)))
}
n_train_x <- nor(train_x)
n_test_x <- nor(test_x)

library(class)

score <- c(0)
number <- c(0)
for (i in 1:50){
  cat(i)
  pred<-knn(train_x,test_x,train_y[,1],k=i)
  cr<- CrossTable(x=pred, y=test_y[,1],prop.chisq =F )
  score[[(length(score)+1)]]<- ((cr$t[1,1]+cr$t[2,2])/(sum(cr$t[1,])+sum(cr$t[2,])))
  number[[(length(number)+1)]]<-i
}
score <- score[2:51]
number <- number[2:51]


for (i in number){
  if (max(score) == score[i]){
    cat(max(score), 'k =>',i)
  }
}
  
#0.969697 k => 6



