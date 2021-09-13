
R_wiki

#문단단위로 분리
rwikiPara<-strsplit(R_wiki,split='\n')
class(rwikiPara)#list
class(rwikiPara[[1]])#character
#[[1]] 리스트의 길이는 1
#[1] 요소 1번째가 벡터
#[2] 요소 2번째가 벡터

#문장 단위로 분리
#strsplit(R_wiki, split = ".") 점을 인식못함
strsplit(R_wiki, split = "\\.") # \\ 두개 해줘야 함
# R_wiki 를 문장 단위로 분리
#strsplit(rwikiPara, split = "\\.") Error 발생 Type 리스트 여서
rwikisent<-strsplit(rwikiPara[[1]], split = "\\.")
length(rwikisent)

length(rwikisent[[1]])
length(rwikisent[[2]])
