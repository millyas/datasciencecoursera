corr <- function(directory, threshold = 0)
{
        f<-list.files(directory,full.names=TRUE)
        d<-data.frame()
        cr<-numeric()
        j<-1
        for(i in 1:332)
        {
                a<-d
                a<-read.csv(f[i])
                if (sum(complete.cases(a))>threshold)
                {
                        a<-a[complete.cases(a),]
                        d<-rbind(d,a)
                        cr[j]<-cor(d[,2],d[,3])
                        j<-j+1
                        d<-data.frame()
                }
                
        }
        return(cr)

}
