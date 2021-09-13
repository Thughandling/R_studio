my2sentence <- c("Learning R is so interesting","He is a fascinating singer")

length(my2sentence)
my2sentence



#install.packages('tidyverse')
library(tidyverse)
str_match('ing',my2sentence)

#ing로 끝나는 모든 단어 검색
mypattern<-gregexpr('[[:alpha:]]ing',my2sentence)
#알파벳문자 1개가 나와있고, ing로 끝나는 것을 찾아라
mypattern<-gregexpr('[[:alpha:]]+ing\\b',my2sentence)
#알파벳문자로 구성되어 있고, ing로 끝나는 것을 찾아라
# \\b 를쓰면 끝나는 구문을 표현.  
# => 알파벳 여러개+ing+끝

mypattern

regmatches(my2sentence,mypattern)
#python에는 endwith() => 특정문자열로 끝나는지 확인
#r 에서는?
#endsWith(my2sentence,'ing')
myexample <-"He (Obama) received the Nobel Prize"
mypattern<- gregexpr('\\([[:alpha:]]+\\)',myexample)

regmatches(myexample, mypattern)


myentences <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

rwikisent<-strsplit(myentences[[1]], split = "\\.")
rwikisent
gregexpr('[[:alpha:]]+ing\\b',rwikisent)
mypattern1 <- gregexpr('[[:alpha:]]+ing\\b',rwikisent)
mypattern1

library(stringr)
#text 분석에 많이 쓰이느 패키지중 하나

R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
#문자열 데이터를 추출할때 stringr 패키지내에 str_extract(),str_extract_all() 
str_extract(R_wiki,"software environment")
#여러군데 있으나 한개만 추출
str_extract_all(R_wiki,"software environment",simplify = TRUE)
#여러군데 있고 모든 것 추출, 행렬 타입 원하면 simplify=True

#첫글자가 대문자인 모든 단어들을 조사.
str_extract_all(R_wiki, '[[:upper:]]+') #대문자 알파벳만 나옴
myextract<-str_extract_all(R_wiki, '[[:upper:]]{1}[[:alpha:]]{0,}')
#[[:upper:]]{1}[[:alpha:]]{0,}' 대문자 시작하며 1개이고,
#뒤에 소문자로 이어 붙으며 0개이상
table(myextract)
###
str_match(R_wiki,"software environment")#행렬 형태로
str_match_all(R_wiki,"software environment")#행렬

#str_match(string = ,pattern = )
str_match(R_wiki,'[[:alpha:]]{1,}')
str_match_all(R_wiki,'[[:alpha:]]{1,}')
str_match_all(R_wiki,'[[:alpha:]]{1,} and [[:alpha:]]{1,}')


university_address <- c("연세대학교 주소는 서울시 서대문구 연세로 50번지다",
                        "서울대 주소: 서울시 관악구 관악로 1번지다",
                        "고려대는 서울시 성북구 안암로 145번지에 있다",
                        "카이스트 주소, 대전시 유성구 대학로 291번지",
                        "포항시 남구 청암로 77번지는 포항공과 대학교 주소임")
#시 구 로 모두 번지 출력
str_match_all(university_address,'[[가-힣]]{0,}시 [[가-힣]]{0,}구 [[가-힣]]{0,}로')
str_match_all(university_address,'[[:alpha:]]{0,}시 [[:alpha:]]{0,}구 [[:alpha:]]{0,}로 [[:digit:]]{1,}번지')

style_address <- '([[:alpha:]]{0,}시) ([[:alpha:]]{0,}구) ([[:alpha:]]{0,}로) ([[:digit:]]{1,}번지)'

my_address <- data.frame(str_match(university_address, style_address))

names(my_address) <- c("full_address","city","district","road","street")
my_address


rwikiPara<-str_split(R_wiki,'\n')
rwikiPara

rWikiSent<-str_split(rwikiPara[[1]],'\\. ')
rWikiSent

my2sentences <-unlist(rWikiSent)[c(4,7)]    #char 벡터
my2sentences

# 두 문장의 단어수를 각각 출력하시오.
#string_split
length(unlist(str_split(my2sentences[1],' ')))
length(unlist(str_split(my2sentences[2],' ')))

#
