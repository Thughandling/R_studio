# 1
for (i in month.name){
  cat('The month of',i,'\n')
}

# 2
e_o <- c(-5:5)
ch <-ifelse(e_o%%2==0,'짝수','홀수')
c(e_o,ch)

# 3
table(ch)

# 4
pie <- 3
x <- c(1:5)
ifelse(x >3,'TRUE','FALSE')

# 5
x <- c(2:99)
multi <- ifelse(x%%3==0,x,0)
sum(multi) #1683
length(which(multi>0))#33

#6
n <-readline('n>2')
x <- c(1:n)
f <- 1
for (i in x){
  f <- f*i
}
f

#7

for (i in 1:9){
  for (j in 1:9){
    cat(i,'*',j,'=',i*j,'\n')
  }
}


#8
n <- scan()

for (i in 1:n){
  if (i <n){
    for (j in 1:(n-i)){
      cat(' ')
    }
  }
  for(k in 1:((2*i)-1)){
    cat('*')
  }
  cat('\n')
}

