rankall <- function(outcome,num="best")
{       dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        x<-data.frame()
        {
                if((outcome!="heart attack") & (outcome!="heart failure") & (outcome!="pneumonia"))
                        stop("invalid outcome")
        }
        #2=hospital name,7=state
        # 11=HeartAttack,17=Heart Failure,23=Pneumonia
        if(outcome=="heart attack") 
        {
        x<-dat[,c(2,7,11)]
        } 


        if (outcome=="heart failure")
        {
        x<-dat[,c(2,7,17)]
                }  

        if(outcome=="pneumonia")
        {
        x<-dat[,c(2,7,23)]
        
        } 
        suppressWarnings(x[,3]<-as.numeric(x[,3]))
        x<-x[complete.cases(x),]
        x<-x[order(x[,1]),]
        x<-x[order(x[,3]),]
        y<- unique(x[,2])
        y<-sort(y)
        final<-data.frame()
        
        for(i in 1:length(y))
        {
                rum<-num
                u<-data.frame()
                v<-data.frame()
                u<-subset(x,x[,2]==y[i])
                v<-u[,1:2]
                if(rum=="best"){rum=1}
                if(rum=="worst"){rum=length(v[,1])}
                final<-rbind(final,v[rum,])
                
        }
        
        names(final)[1]<-"hospital"
        names(final)[2]<-"state"
        row.names(final)<-y
        final[,2]<-y
        return(final)

}