pollutantmean <- function(directory, pollutant, id = 1:332)
        {
                f<-list.files(directory,full.names=TRUE)
                d<-data.frame()
                for(i in id)
                {
                        d<-rbind(d,read.csv(f[i]))
                }
                mean(d[,pollutant], na.rm=TRUE)
        }