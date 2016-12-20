rankhospital<- function (state,outcome,num) {
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        if (!(state %in% data$State)){      
                stop("invalid state")
        }
        validoutcomes <- c("heart attack","heart failure","pneumonia")
        
        if(!(outcome %in% validoutcomes)){
                stop("invalid outcome")
        }
        splitdata <- subset(data,State==state)
        pointer<-num
      
        if(outcome == "heart attack"){
                newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack !="Not Available")
                newdata[,11] <- as.numeric(newdata[,11])
                sorteddata<-newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,newdata$Hospital.Name),]
        }
        else if(outcome == "heart failure"){
                newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure !="Not Available")
                newdata[,17] <- as.numeric(newdata[,17])
                sorteddata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,newdata$Hospital.Name),]
        }
        else if(outcome == "pneumonia"){
                newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia !="Not Available")
                newdata[,23] <- as.numeric(newdata[,23])
                sorteddata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,newdata$Hospital.Name),]
        }
        if(num == "best"){
                pointer<- 1
        }
        else if(num == "worst"){
                pointer <- nrow(sorteddata)
        }
        else if(num > nrow(sorteddata)){
                pointer<- "NA"
        }
        hospital_name<- sorteddata[pointer,c("Hospital.Name")]
        hospital_name
        
}
