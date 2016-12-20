best<- function (state,outcome) {
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        if (!(state %in% data$State)){      
                stop("invalid state")
        }
        validoutcomes <- c("heart attack","heart failure","pneumonia")
        
        if(!(outcome %in% validoutcomes)){
                stop("invalid outcome")
        }
        splitdata <- subset(data,State==state)

        if(outcome == "heart attack"){
                 newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack !="Not Available")
                newdata[,11] <- as.numeric(newdata[,11])
                row_min <- min(newdata[,11])
                hospital_name <- newdata$Hospital.Name[newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack== row_min]
        }
        else if(outcome == "heart failure"){
                newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure !="Not Available")
                newdata[,17] <- as.numeric(newdata[,17])
                sorteddata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                hospital_name <- sorteddata[1,c("Hospital.Name")]
               #hospital_name <- newdata$Hospital.Name[newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure== row_min]
        }
        else if(outcome == "pneumonia"){
                newdata<- subset(splitdata, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia !="Not Available")
                newdata[,23] <- as.numeric(newdata[,23])
                row_min <- min(newdata[,23])
                hospital_name <- newdata$Hospital.Name[newdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == row_min]
        }
        print(state)
        print(row_min)
        hospital_name
        
}
