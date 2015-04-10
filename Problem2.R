options(max.print=10000000)
x<-matrix(100:999,1,900)
y<-matrix(100:999,900,1)
u<-matrix(0,900,900)
z<-y%*%x
v<-z
for(i in 1:900)
{
        for(j in 1:900)
        {
                while(z[i,j]%/%10!=0)
                {
                        u[i,j]<-10*(z[i,j]%%10+u[i,j])
                        
                        z[i,j]<-z[i,j]%/%10
                        
                        if(z[i,j]<10)
                        {
                                u[i,j]<-u[i,j]+z[i,j]
                        }
                        
                }
        }
}
w=u-v
for(i in 1:900){
        for(j in 1:900){
                if(w[i,j]==0)
                        w[i,j]<-v[i,j]
                else
                        w[i,j]<-0
                #     
        }
}
print(max(w))