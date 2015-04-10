x<-seq(1:999)
sum<-0
for(i in 1:999) {if ((x[i]%%3==0)|(x[i]%%5==0))
sum=x[i]+sum
}
print(sum)