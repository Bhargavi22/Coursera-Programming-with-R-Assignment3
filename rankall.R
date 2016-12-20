rankall<- function (outcome,num ="best") {
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        validoutcomes <- c("heart attack","heart failure","pneumonia")
                if(!(outcome %in% validoutcomes)){
                stop("invalid outcome")
        }
        #splitdata <- subset(data,State)
        
        
        if(outcome == "heart attack"){
                if (num == "worst"){
                        final<- worstoutput("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","worst")
                }
                else{
                
        final <- togetdata("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",num)
                }
        }
        else if(outcome == "heart failure"){
                if (num == "worst"){
                        final<-           worstoutput("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","worst")
                }
               else{
                       final<- togetdata("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",num)
               }
        }
        else if(outcome == "pneumonia"){
                if (num == "worst"){
                 final<-      worstoutput("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia","worst")
                }
                else{
                final<- togetdata("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",num)
                }
             
        }
       
       final
        
}
togetdata <- function(outcometype,num){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        result <- data.frame(hospital=character(),state=character())
        d1 <- data[,c("Hospital.Name","State",outcometype)] 
        d2<- split(d1,d1$State)
        
        if(num == "best"){
                num<- 1
        }
        for (i in 1:length(d2)){
                        d3<- subset(d2[[i]], outcometype !="Not Available")
                        d3[,3]<- as.numeric(d3[,3])
                        sorteddata<-d3[order(d3[3],d3$Hospital.Name),]
                        d4<- sorteddata[,c("Hospital.Name","State")]
                         if (num > nrow(d4)){
                                x<-unique(d4[,2])
                                y<- NA
                                list1<- list(y,x)
                                temp<-as.data.frame(list1)
                                names(temp)=names(result)
                                result <- rbind.data.frame(result,temp,deparse.level = 0)  
                         }
                        else{
                          element<- d4[num,]
                                names(element)=names(result)
                                result <- rbind.data.frame(result,element)
                        }
                }
        result
        
}
worstoutput <- function (outcometype,num){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        result <- data.frame(hospital=character(),state=character())
        d1 <- data[,c("Hospital.Name","State",outcometype)] 
        d2<- split(d1,d1$State)
        for (i in 1:length(d2)){
                
                        d3<- d2[[i]]
                        d4=subset(d3, d3[3]!="Not Available")
                        d4[,3]<- as.numeric(d4[,3])
                        sorteddata<-d4[order(d4[3],d4$Hospital.Name),]
                        d5<-tail(sorteddata,1)
                        d6<- d5[,c("Hospital.Name","State")]
                       # d5<- tail(d4,1)
                        names(d6)=names(result)
                        result <- rbind.data.frame(result,d6,deparse.level = 0) 
                        
        }
        result
}