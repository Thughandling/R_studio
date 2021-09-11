# Q1. 시험 점수 변수 만들고 출력하기
score <- c(80,60,70,50,90)
score

# Q2. 전체 평균 구하기
mean(score)

# Q3. 전체 평균 변수 만들고 출력하기
library(ggplot2)
midwest_df <- data.frame(midwest)
midwest_df

#문제 1. 
str(midwest_df)

# 문제 2. 
#mpg <- rename(mpg, company = manufacturer)
midwest_df <- rename(midwest_df, total = poptotal, asian = popasian)
str(midwest_df$total)
str(midwest_df$asian)


# 문제 3.
# rate = 전체인구 대비 아시아 인구 백분율 변수
midwest_df$rate <- ((midwest_df$asian /midwest_df$total)*100)
summary(midwest_df$rate)
hist(midwest_df$rate)

# 문제 4.
midwest_df$mean_size <- ifelse(midwest_df$rate > mean(midwest_df$rate) ,'large','small')
#문제 5.
table(midwest_df$mean_size)
qplot(midwest_df$mean_size)

