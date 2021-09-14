#1
mtcars <- as.data.frame(mtcars)
str(mtcars)

mtcars$weight <- ifelse(mtcars$wt > mean(mtcars$wt),'heavy','light')
table(mtcars$weight)

#2
library(ggplot2)
midwest <- as.data.frame(midwest)
#ggplot2 의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악
str(midwest)
#poptotal(전체 인구)을 total 로, popasian(아시아 인구)을 asian 으로 변수명을 수정
midwest <- rename(midwest,
                  total = poptotal,
                  asian = popasian)
midwest #rename
#total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수 생성
midwest$asian_total <- (midwest$asian/midwest$total)*100
#아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수 생성
midwest$asian_ratio <- ifelse(midwest$asian_total >mean(midwest$asian_total),'large','small')
#"large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도수를 출력
table(midwest$asian_ratio)

#3 
#타이타닉 데이터 불러오기
train<-read.csv('train.csv')

#생존자 수, 사망자 수 출력
train$Survived <- ifelse(train$Survived=='0','died','survived')
table(train$Survived)

#pclass, embarked 별 승객수 출력(비율)
table(train$Pclass)
table(train$Embarked)
table(train %>% group_by(Pclass) %>% select(Embarked))

#Name에서 호칭 종류 출력, 호칭 종류 별 승객수 출력
library(stringr)
pattern <- '\\, [[:alpha:]]{1,} ?[[:alpha:]]{0,}'
nick <- str_extract(train$Name,pattern = pattern)
nick <- str_remove_all(nick,'\\, ')
table(nick)

#호칭을 아래와 같이 변경하여 name2열에 추가
nick<-gsub('Mlle|Ms|Lady|Dona','Miss',nick)
nick<-gsub('Mme','Mrs',nick)
nick<-gsub('Capt|Col|Major|Dr|Rev|Don|Sir|the Countess|Jonkheer','Officer',nick)
nick<-gsub('Master','Others',nick)
train$name2<-nick

#name2 열을 factor로(5가지 범주) 변환
train$name2<- factor(train$name2)
train$name2
#name2열의 호칭별 인원수 출력
table(train$name2)

tapply(train[!is.na(train$Age),]$Age,train[!is.na(train$Age),]$name2, mean)

nick_mean_age <- tapply(train[!is.na(train$Age),]$Age,train[!is.na(train$Age),]$name2, mean)



table(train$Age)
summary(train$Age)
sum(is.na(train$Age))

#호칭 정보를 바탕으로 나이(Age) 결측값 대체(호칭 별 나이의 평균값)
ifelse(train[is.na(train$Age),]$name2=='Miss',train[is.na(train$Age),]$Age<-nick_mean_age[['Miss']],
         ifelse(train[is.na(train$Age),]$name2=='Mr',train[is.na(train$Age),]$Age<-nick_mean_age[['Mr']],
               ifelse(train[is.na(train$Age),]$name2=='Mrs',train[is.na(train$Age),]$Age<-nick_mean_age[['Mrs']],
                      ifelse(train[is.na(train$Age),]$name2=='Officer',train[is.na(train$Age),]$Age<-nick_mean_age[['Officer']],train[is.na(train$Age),]$Age<-nick_mean_age[['Others']])
                )
         )
  )
table(train$Age)
sum(is.na(train$Age))


train$Age_cut<- ifelse(train$Age < 10, '10대 미만',
         ifelse(10 <= train$Age & train$Age< 20, '10대',
               ifelse(20 <= train$Age  & train$Age < 30, '20대',
                     ifelse(30 <= train$Age  & train$Age< 40, '30대',
                           ifelse(40 <= train$Age  & train$Age< 50, '40대','50대'
                                 )))))
#-age열의 구간별 인원수 출력 10대 미만, 10대, 20대, 30대, 40대, 50대 이상
table(train$Age_cut)

sum(is.na(train$Cabin)) 
#687개 NA; 891-687 = 204개의 값
train$Cabin<- ifelse(train$Cabin =='',NA,train$Cabin)
#cabin 컬럼의 1번째 글자 출력(NA는 제외)
cabin_1<- str_extract(train[!is.na(train$Cabin),]$Cabin,'[[:alpha:]]')
cabin_1
summary(train$Fare)
#sum(is.na(train$Fare)) 0개
#fare열 값에 대해 최대/최소/평균/표준편차 출력
sd(train$Fare) #49.69343

#sibsp + parch를 더하여 새롭게 family열에 저장
train$family <- train$SibSp + train$Parch
train$family
