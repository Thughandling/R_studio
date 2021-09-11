#spss - sav 파일
#install.packages('foreign')
#install.packages('data.table')
library(data.table)
library(foreign)
library(readxl)
library(ggplot2)
welfare <- read.spss('Koweps_hpc10_2015_beta1.sav',to.data.frame = T)
class(welfare)
str(welfare)

library("dplyr")#rename
welfare <- rename(welfare,
                  sex = h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7)
                  # 지역 코드




welfare <- data.frame(sex= welfare$sex,
                      birth= welfare$birth,
                      marriage= welfare$marriage,
                      religion= welfare$religion,
                      income= welfare$income,
                      code_job= welfare$code_job,
                      code_region= welfare$code_region,
                      ID = c(1:length(welfare$income))
                      )
welfare

#1.분석주제
#성별에 따른 급여 차이 분석&시각화
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(income)

 

boxplot(income~sex, data=sex_income,
        
        ylab="급여",
        
        xlab="성별",
        
        main="성별에 따른 급여")


#나이에 따른 급여 차이 분석 시각화(나이대형성)
welfare$birth <- 2021 - welfare$birth

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(birth) %>%
  summarise(mean(income))

ggplot(data =age_income, aes(x=birth, y=`mean(income)`)) + geom_col()

#혼인 상태 분석(혼인 비율, 이혼율, ///), 혼인 상태와 급여 관계
#혼인비율
mar<- data.frame(table(welfare$marriage))
mar$Var1 <- c('18세 미만','기혼','사별','이혼','별거','미혼','기타')

bp <- ggplot(mar, aes(x='',y=Freq, fill=Var1))+
  geom_bar(width =1, stat = 'identity')
pie <- bp + coord_polar('y',start = 0)
pie
#
#
#
#혼인상태와  급여관계

in_mr <- welfare %>%
  filter(!is.na(income))%>%
  select(marriage,income)

table(in_mr$marriage)
# 각 혼인상태 마다 급여의 결측값을 제거후
# 혼인 상태별 급여 평균 측정

boxplot(income~marriage,
        data = in_mr, 
        main='혼인 상태와 급여 관계',
        xlab='혼인 상태',
        ylab='급여 평균',
        col='orange',
        border='brown')


#종교와 급여관계
Christ_or_not <- welfare %>% #전체 데이터에서 
  filter(!is.na(income))%>% #급여가 존재하는 것들 중에
  group_by(religion)%>% #종교가 있냐 없냐로 구분하여
  summarise(mean(income))#급여의 평균을 구함
Christ_or_not$religion<- c('신앙O','신앙X')
Christ_or_not


#종교와 이혼율 관계 분석
Christ_and_divorce <- welfare %>%
  filter(is.na(religion))
sum(Christ_and_divorce$religion)#종교 여부에 NA 없음
##########
Christ_and_divorce <- welfare %>%
  filter(marriage == 3)%>%#이혼 상태 
  group_by(religion)%>%
  summarise('divorce' = n())
Christ_and_divorce$religion <- c('신앙O','신앙X')
Christ_and_divorce$divorce <- (Christ_and_divorce$divorce/sum(Christ_and_divorce$divorce))*100 
Christ_and_divorce


#지역별 평균나이
region_ <- data.frame(table(welfare$code_region))
region_$Var1 <- c('서울','수도권(인천/경기)',
                     '울산/부산/경남','대구/경북',
                     '대전/충남','강원/충북','광주 등')
region_

age_region<- data.frame(region=region_$Var1,age_region=ceiling(2021-tapply(welfare$birth,  welfare$code_region , mean)))
age_region

#지역별 연령대 비율표
welfare$birth <- 2021-welfare$birth
welfare$birth

welfare$age_nick <- ifelse(welfare$birth <19, '청소년 이하',
                           ifelse(welfare$birth <= 19 & welfare$birth <40, '청년',
                                  ifelse(welfare$birth>=40 & welfare$birth <60,'불혹','노년')))


region_age <- data.frame(welfare$code_region,welfare$age_nick)
region_age$welfare.age_nick




ggplot(region_age, aes(x=welfare.code_region, fill=welfare.age_nick))+
  geom_bar(position='fill')




#직종코드는 있지만, 급여가 확인되지 않은 3000명 정도 NA가 존재
#급여가 NA인 사람의 직종코드를 참조하여, 동일한 직종 코드의 해당되는
#다른 사람들의 데이터를 찾아서 급여를 유추하여 결측치 대체

#코드 직종 있으나, 급여 없는 사람들.(이하 Case : A)
#코드 직종은 있으나 같은 직종의 모든 사람의 급여가 입력 되지 않은 경우 존재.(이하 Case : B) 

na_income_id <- c(welfare[is.na(welfare$income)&!is.na(welfare$code_job),]$ID)
na_income_id #Case A의 데이터 index를 벡터에 담아 리턴


mean_income<- welfare[-(na_income_id),] %>% #전체 데이터에서
  group_by(code_job)%>% #code_job으로 그룹화
  summarise(mean(income)) #각 직종(code_job)마다의 급여 평균값 리턴
mean_income$code_job
#


#welfare[na_income_id,]$income <- mean_income$`mean(income)`
#전체데이터는 2895개의 na이 있는데, 입력 값이 142개의 직종별 평균값만 존재하여 X
#데이터를 합쳐서 문제해결.

dvd <- left_join(welfare[na_income_id,],mean_income,by='code_job')
# Case A의데이터와 mean_income 데이터의 'code_job'을 기준으로 합친다.
# 왼쪽 데이터는 중복 포함 2895개의 code_job이 있고,
# 그것들을 매칭하여 mean_income의 직종과 급여가 합쳐진다.
dvd 

#이제 Case A 의 직종 순으로, 합쳐진 데이터의 평균 값을 뽑아내어 저장
welfare[na_income_id,]$income <- dvd$`mean(income)`
welfare[na_income_id,]$income

#[991] 168.24341 386.54176  92.00000  92.00000 319.95556 103.52643 163.90374  92.00000  92.00000  92.00000
#[ reached getOption("max.print") -- omitted 1895 entries ]

#Case B의 경우 존재 
welfare[!is.na(welfare$code_job)&is.na(welfare$income),]
#해당 직종 전체가 급여 값이 입력이 안되어 있다.