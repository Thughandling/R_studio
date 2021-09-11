# 1
score <- c(80,60,70,50,90)
print(score)
# 2
mean(score)
# 3
mean_score = mean(score)
print(mean_score)
# 4
fruits = data.frame('제품' = c('사과','딸기','수박'),
                    '가격' = c(1800,1500,3000),
                    '판매량' = c(24,38,13))
fruits

# 5
mean(fruits$가격)
mean(fruits$판매량)
# 6
library(ggplot2)  
mpg
#cty 도시연비 hwy 고속도로 연비

# Q1
mpg_ <- data.frame(mpg)
mpg_
# Q2
mpg_ <- rename(mpg_,'city'='cty','highway'='hwy')
mpg_
# Q3
# mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
# exam %>% filter(math > 50)
# mtcars %>% filter(disp > 400)$mpg

four <- mpg_ %>% filter(displ <= 4) 
four <- mean(four$highway)

five <- mpg_ %>% filter(displ >= 5)
five <- mean(five$highway)

high_displ <- ifelse(four > five, '4 이하','5 이상')
high_displ
print(four)
print(five)
# Q5
audi <- mpg_ %>% filter(manufacturer == 'audi') %>% select(city)
audi <- mean(audi)

toyota <- mpg_ %>% filter(mpg_$manufacturer == 'toyota')
toyota <- mean(toyota$city)

high_city <- ifelse(audi > toyota,'audi','toyota' )
high_city
print(audi)
print(toyota)


# Q6
suv <- mpg_ %>% filter(class=='suv')
suv <- mean(suv$city)
compact <- mpg_ %>% filter(class=='compact')
compact <- mean(compact$city)

class_city <- ifelse(suv > compact,'suv','compact' )
class_city
print(suv)
print(compact)

# 7
ss <- read.csv("samsung.csv")
ss <- ss %>% filter(Date > '2021-01-01')
ss$Date
#write.csv(ss,'2021_samsung.csv')
# 1)
str(ss[c("Open","High", "Low", "Close", "Adj.Close", "Volume")])
# 2)
summary(ss[c("Open","High", "Low", "Close", "Adj.Close", "Volume")])
# 3)
normalization <- function(v){
  (v - min(v)) / (max(v) - min(v))
}
normalization(ss[c("Open","High", "Low", "Close", "Adj.Close", "Volume")])

Standardization <-function(v){
  (v-mean(v))/sd(v)
}
Standardization(ss$Open)
Standardization(ss$High)
Standardization(ss$Low)
Standardization(ss$Close)
Standardization(ss$Adj.Close)
Standardization(ss$Volume)

# 4)


  