rankall<-function(outcome,num="best") {
        
        df<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        outc=0
        if(outcome=="heart attack") {outc<-11} else {
                if(outcome=="heart failure") {outc<-17} else {
                        if(outcome=="pneumonia") {outc<-23} else {stop("invalid outcome")}
                }
        }
        
        df[,outc]<-as.numeric(df[,outc])
        st<-df$State
        fst<-factor(st)
        lfst<-levels(fst)
        hn=0
        
        for(i in seq_len(length(lfst))) {
                state<-lfst[i] 
                df_state_g<-df$State==state
                df_state<-df[df_state_g,]
                state_outc_g<-complete.cases(df_state[,outc])
                state_outc<-df_state[state_outc_g,]
                num_hosp<-length(state_outc[,outc])
                hr=0
                options("show.error.messages"=FALSE)
                if(is.numeric(num)==TRUE&num<=num_hosp) {} else {if(is.numeric(num)==FALSE&(num=="best"|num=="worst")) {} else {(hn<-c(hn,NA))&next} 
                }
                
                options("show.error.messages"=FALSE)
                
                if(num=="best") {
                        for(i in seq_len(length(state_outc[,outc]))) {
                                if(state_outc[,outc][i]==min(state_outc[,outc])) 
                                {hr<-c(hr,i)} else {}
                        } 
                } else {}
                
                if(num=="worst") {
                        for(i in seq_len(length(state_outc[,outc]))) {
                                if(state_outc[,outc][i]==max(state_outc[,outc])) 
                                {hr<-c(hr,i)} else {}
                        }
                } else {}
                
                if(hr!=0) {hr<-hr[-1]} else {}
                
                outc_or_alph<-order(state_outc[,outc],state_outc$Hospital.Name)
                
                for(j in seq_len(num_hosp)){
                        if(num==j) {hr<-outc_or_alph[j]} else {}
                }
                hnm<-state_outc$Hospital.Name
                list_b<-hnm[hr]
                list_bt<-factor(list_b)
                list<-levels(list_bt)
                options(warn=-1)
                hn<-c(hn,list[1])
        }
        hn<-hn[-1]
        tab<-data.frame("hospital"=hn,"state"=lfst)
        row.name(tab)<-lfst
        print(tab)
}