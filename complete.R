complete <- function(directory, id = 1:332)
{
        f<-list.files(directory,full.names=TRUE)
        d<-data.frame()
        for(i in 1:length(id))
        {
                d<-rbind(d,read.csv(f[id[i]]))
        }
                ID<-vector(mode="numeric",length(id))
                nobs<-vector(mode="numeric",length(id))
        for(i in 1:length(id))
        {
                nobs[i]=sum(complete.cases(subset(d,ID==id[i])))
                ID[i]=id[i]
        }
        e<-data.frame(ID,nobs)
        print(e)        
}