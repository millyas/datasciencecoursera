best <- function(state, outcome) 
{ 
        dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        x<-data.frame()
        # 11=HeartAttack,17=Heart Failure,23=Pneumonia
        # 2=hospital name,7=state
        if(nrow(subset(dat,dat[,7]==state))==0)
        {
                stop("invalid state")
                
        }
        if((outcome!="heart attack")&(outcome!="heart failure")&(outcome!="pneumonia"))
        {
                stop("invalid outcome")
        }
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
x<-subset(x,x[,2]==state)
min.val<-min(as.numeric(x[,3]))
x<-subset(x,x[,3]==min.val)
if(nrow(x)==1)
{
        print(x[,1])
} else
{
        x<-order(x[,1])
        print(x[,1])
}     

}

