myvector <-c(1:6,'a')
myvector

mylist <-list(1:6,'a')
mylist


obj1 <-1:4
obj2 <- 6:10

class(obj1)
obj3<-list(obj1,obj2)
obj3

my <- 'learning R is so interesting'

mywords <- strsplit(my, split = ' ')
class(mywords)
class(mywords[[1]][1])


lapply(x=my,strsplit(x, split=""))



출처: https://zeroad91.tistory.com/entry/R-기초-문자와-패턴 [zeroad]
lapply(mywords,FUN= strsplit(mywords,split=''))

mywords %>%
  strsplit(mywords[[1]][5],split='')
