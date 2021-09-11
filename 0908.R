library(Amelia) # 결측치 시각화
library(mice) #결측값 

#train data 
train_df <- read.csv("train.csv")
label <- subset(train_df, select=Survived)
test <- read.csv("test.csv")

data <- rbind(train_df[-2],test)#행 결합 하여 컬럼을 기준으로 합침
missmap(data)
#Age와 Fare에 결측 값 존재

#결측치 대체
mice_mod <- mice(data[, c("Age","Fare")], method='cart')
mice_complete <- complete(mice_mod)

data[, c("Age","Fare")] <- mice_complete
missmap(data)
##

train_df <- data[1:891,]
test<- data[892:1309,]
train_df <- cbind(train_df,label)

##

train_df$Sex<- ifelse(train_df$Sex =='male',1,0)
test$Sex<- ifelse(test$Sex =='male',1,0)


table(train_df$Embarked)
sum(is.na(train_df$Embarked))

train_df[train_df$Embarked == '',]$Embarked <- 'C'
test[test$Embarked=='']$Embarked <- 'C'

tapply(train_df$Fare, train_df$Embarked,mean)

train_df$Embarked <- ifelse(train_df$Embarked=='C',0,
                            ifelse(train_df$Embarked =='Q',1,2))
test$Embarked <- ifelse(test$Embarked=='C',0,
                            ifelse(test$Embarked =='Q',1,2))


train_df$Embarked

train_df$Age <- ifelse(train_df$Age < 18, 1,0)
test$Age <- ifelse(test$Age < 18.00, 1,0)

#aggregate(data=train_df, Fare~Embarked, mean)
#train %>% group_by(Embarked) %>%
#  summaries(fare_mean=mean(Fare))
normalize <- function(x){
  num <- x - min(x)
  denom <- max(x)-min(x)
  return(num/denom)
  }
train_df$Fare <-normalize(train_df$Fare)
test$Fare <-normalize(test$Fare)



#validation data 0.2
valid_size <- ceiling(length(train_df$PassengerId)*0.2)
valid_idx <- c(sample(1:length(train_df$PassengerId),size=valid_size))
valid<- train_df[valid_idx,]
valid
# validation 으로 뽑힌 데이터 제거
train_ <-train_df[-(valid_idx),]

#train and validation label 분류
train_x <- subset(train_, select=-Survived)
train_x
train_y <- subset(train_, select=Survived)
train_y

valid_x <- subset(valid, select=-Survived)
valid_x
valid_y <- subset(valid, select=Survived)
valid_y


# df<-data.frame(PassengerId=아이디저장된변수이름, Survived=예측결과가저장된변수이름)
# write.csv(df, "제출파일명.csv")

library(class)
library(gmodels)


score <- c()
for (i in (12:30)){
  pred<-knn(train=train_x[,-c(1,3,8,10)], test=valid_x[,-c(1,3,8,10)],cl=train_y[,1], k=i)
  cr<- CrossTable(x=pred, y=valid_y[,1],prop.chisq =F  )
  score <- c(score,(cr$t[1]+cr$t[4])/(sum(cr$t[1:4])*100))
}
which.max(score)
print(score)
#pred<-knn(train=train_x[,-c(1,3,8,10)], test=valid_x[,-c(1,3,8,10)],cl=train_y[,1], k=12)
#pred
#length(pred)
#length(valid_y[,])

#install.packages('gemodels')


#tab <- CrossTable(x=pred, y=valid_y[,])
#tab$t[1:4]

#train_df[,-c(1,3,8,10)]
#test[,-c(1,3,8,10)]


##########################
length(pred)
length(label[,1])
pred







print(score)

#write.csv(data.frame(pred), "pred.csv")
#install.packages("caret",dependencies = TRUE)

library(caret)

#model <- train(train_x[,c(2,5,6,7,9)],
 #              train_y,
  #             method='knn',
   #            preProcess = 'scale'
    #           )


pred<-knn(train=train_x[,c(2,5,6,7,9)], test=test[,c(2,5,6,7,9)],cl=train_y[,1], k=22)
pred
cr<- CrossTable(x=pred, y=valid_y[,],prop.chisq =F  )
highacc <- (cr$t[1,1]+cr$t[2,2])/(sum(cr$t[1,])+sum(cr$t[2,]))
highacc

pred<-knn(train=train_x[,-c(1,3,8,10)], test=test[,-c(1,3,8,10)],cl=train_y[,1], k=9)

submission = read.csv('gender_submission.csv')
submission$Survived = pred


df<-data.frame(PassengerId=submission$PassengerId, Survived=submission$Survived)
write.csv(df, 'submission.csv', row.names = F)

