#install.packages('fpc')
library(fpc)
library(ggplot2)
library(base)

#sample(1:10,n) #1부터 10가지의 수를 n개 생성하라
#replace 복원 추출


data<-data.frame(cbind(c(sample(1:10,40,replace = T),sample(20:30,20,replace = T)),
                       c(sample(1:10,40,replace = T),sample(20:30,20,replace = T))))

data

ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=X1, y=X2))+
  theme_classic()

#dbscan 클러스터링
db <- dbscan(data, eps=sqrt(18),MinPts = 2)
db

db$cluster
db$isseed#core인가 아닌가 T or F
#dbscan Pts=50 MinPts=4 eps=3.162278
#0  1  2         0 -> 노이즈 outlier 1,2 순번째 클러스터 
#border 1  0  4
#seed   0 30 15
#total  1 30 19

library(RColorBrewer)
ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=X1, y=X2,col=as.factor(db$cluster)))+
  geom_point(data=data[!db$isseed,],aes(X1,X2),shape=8)+
  theme_light()
  



data<-data.frame(cbind(c(sample(1:10,30,replace = T),sample(20:30,20,replace = T)),
                       c(sample(1:15,30,replace = T),sample(15:30,20,replace = T))))
data

ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=X1, y=X2))+
  theme_light()
disEps <- function(mydata, x, y){
#mydata : 기준점
#x,y : 나머지 점들의 x,y 좌표
  sqrt((mydata[1]-x)^2+(mydata[2]-y)^2)
}

#k를 지정해야함
#일반적으로 데이터의 변수개수+1 부터 시작
#정해진것은 없음.

k <- 3
#각 점과 거리를 구함
dis<-c()
for (i in 1:nrow(data)){#i는 1~50까지 값
    dis <- c(dis,sort(apply(data, 1, disEps,x<-data[i,1],y<-data[i,2]))[k+1])
}
#nrow(data) #range와 유사
print(dis)
  
ggplot()+
  #geom_hline(yintercept = 1:30, colour='grey')+
  #geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=1:length(dis), y=sort(dis,decreasing=T)))+
  theme_light()

#MinPts(k값 = 3), 반경 eps내에 점이 3개 있어야 한다.
#-> 자기 자신이 포함이 됨.
data

db<-dbscan(data,eps=3.5,MinPts=2)
ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(aes(x=X1,y=X2,col=as.factor(db$cluster))))+
  theme_light()
