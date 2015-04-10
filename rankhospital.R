rankhospital <- function(state, outcome,num="best")
{
        dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        x<-data.frame()
        if(nrow(subset(dat,dat[,7]==state))==0)
        {stop("invalid state")} else 
        {if((outcome!="heart attack") & (outcome!="heart failure") & (outcome!="pneumonia"))
                stop("invalid outcome")
        }
        
        #2=hospital name,7=state
        # 11=HeartAttack,17=Heart Failure,23=Pneumonia
        if(outcome=="heart attack") 
        {
                x<-dat[,c(2,7,11)]
        } 
        else
        {
                if (outcome=="heart failure")
                {
                        x<-dat[,c(2,7,17)]
                        
                }  
                else
                {
                        if(outcome=="pneumonia")
                        {
                                x<-dat[,c(2,7,23)]
                        } 
                        
                }
                
                
        }
        
        suppressWarnings(x[,3]<-as.numeric(x[,3]))
        x<-x[complete.cases(x),]
        x<-subset(x,x[,2]==state)
        
        if(num=="best")
        {
                num=1       
        } 
        if(num=="worst")
        {
                num=nrow(x)
        }
        
        if(nrow(x)<num)
        {
                return(NA)
        }
        
        rank<-1:nrow(x)
        y<-data.frame()
        y<-x[,c(1,3)]
        y<-y[order(y[,1]),]
        y<-y[order(y[,2]),]
        y<-cbind(y,rank)
        print(y[num,1])
        
        
} 