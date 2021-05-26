
if(any(Statselect == 1, Statselect == 2)){
    
    validfilterfreq <- function() {
        
        if(Statselect == 1) {
            cat("###########################################\n")
            cat("What low frequency limit?\n")
            whatfreq = readline(prompt = "Limit:")
            
        } else if (Statselect == 2){
            cat("###########################################\n")
            cat("What band frequency limits?\n")
            cat("Write the lower and the higher limits with a comma between them\n")
            cat("Ex: 45,15\n")
            whatfreq = readline(prompt = "Limits:")
            
        }
    }
    
    repeat{
        whatfreq <- validfilterfreq()
        
        if(Statselect == 1) {
            whatfreqselec = as.numeric(whatfreq)
            if(anyNA(whatfreqselec)) {
                cat("###########################################\n")
                print("Error!!! Write the which delimit the low-pass filter\n")
            } else if (length(whatfreqselec) == 0){
                print("Error!!! Write the which delimit the low-pass filter\n")
            } else if (any(duplicated(whatfreqselec))) {
                print("Error!!! Write the which delimit the low-pass filter\n")
            } else if (all(whatfreqselec >= 1) & length(whatfreqselec) > 0){
                hfreq1 <- whatfreqselec
                break
            } else{
                print("Error!!! Write the which delimit the low-pass filter\n")
            }
            
        } else if (Statselect == 2){
            whatfreqselec = as.numeric(unlist(strsplit(split=",", (whatfreq))))
            if(anyNA(whatfreqselec)) {
                cat("###########################################\n")
                print("Error!!! Write the lower and the higher limits with a comma between them\n")
                print("Ex: 45,15\n")
            } else if (length(whatfreqselec) == 0){
                print("Error!!! Write the lower and the higher limits with a comma between them\n")
                print("Ex: 45,15\n")
            } else if (length(whatfreqselec) != 2){
                print("Error!!! Write the lower and the higher limits with a comma between them\n")
                print("Ex: 45,15\n")
            } else if (any(duplicated(whatfreqselec))) {
                print("Error!!! Write the lower and the higher limits with a comma between them\n")
                print("Ex: 45,15\n") 
            } else if (all(whatfreqselec >= 1) & whatfreqselec[1] > whatfreqselec[2] & length(whatfreqselec) > 0){
                hfreq1 <- whatfreqselec[1]
                hfreq2 <- whatfreqselec[2]
                break
            } else{
                print("Error!!! Write the lower and the higher limits with a comma between them\n")
                print("Ex: 45,15\n")
            }
        }
        
        
    }
    
    
    
}


Groupofdata <- function(){
    cat("###########################################\n")
    cat("Choose with what group of data do you want to work.\n")
    print(paste(c(1,2), c("data1", "data2"), sep = ". "))
    
    whatgroup = readline(prompt = "Group: ")
} 
repeat{
    datagroup <- Groupofdata()
    datagroup <- as.numeric(datagroup)
    individualsname
    if (anyNA(datagroup)) {
        cat("###########################################\n")
        print("Invalid interval")
    } else if (datagroup == 1 | datagroup == 2) {
        if(datagroup == 1){
            whatdata <- "data1$"
            whatdata2 <- "data1"; whatdata2 <- parse(text = whatdata2)
            whatdata2 <- eval(whatdata2)
            thenames <- individualsname
            freq = freq1} 
        else {
            whatdata <- "data2$"
            whatdata2 <- "data2"; whatdata2 <- parse(text = whatdata2)
            whatdata2 <- eval(whatdata2)
            thenames <- sensorsname
            freq = freq2}
        break
    } else {
        cat("###########################################\n")
        print("Invalid group of data")
    }
}

#################################
#################################
if (yesornot == "y"){
###
### This loops allows to avoid the error when someone write wrong the data
validpinnaOne <- function(){
    cat("###########################################\n")
    cat("Choose what variables, separate the numbers by commas without spaces\n")
    if(datagroup == 1) {
        numbord <- allindividuals
        print(paste(allindividuals, individualsname, sep = "-"))
    } else {
        numbord <- allsensors
        print(paste(allsensors, sensorsname, sep = "."))
    }
    whatpinnas = readline(prompt = "Variable:")
} 
repeat{
    Pinna <- validpinnaOne()
    
    Pinnaselec = as.numeric(unlist(strsplit(split=",", (Pinna))))
    
    if(datagroup == 1) {
        numbord <- allindividuals
    } else {
        numbord <- allsensors
    }
    
    if(anyNA(Pinnaselec)) {
        cat("###########################################\n")
        print("Error0, try again")
    } else if (length(Pinnaselec) == 0){
        print("Error1, try again") 
    } else if (any(duplicated(Pinnaselec))) {
        print("Error2, try again") 
    } else if (all(Pinnaselec >= min(numbord)) & all(Pinnaselec <= max(numbord)) 
               & length(Pinnaselec) > 0){break
    } else{
        print("That doesn't seems like and individual")
    }
}

###


#################################
#################################
validintervalone <- function(){
    
    cat("###########################################\n")
    cat("Choose an interval.\n")
    cat("Select two julian days with a short dash between them, 35-67, (both days included).\n")
    cat("First day", minNday, "Last day", maxNday, "\n")
    #cat("Interval days: ", min(data1[,1]),"-", round_any(max(data1[,1]), 1, f = floor), 
    #    sep = "", "\n")
    whatinterval = readline(prompt = "Interval: ")
} 
repeat{
    interval <- validintervalone()
    intervaldays <- as.numeric(unlist(strsplit(split="-", (interval))))
    
    if (anyNA(intervaldays)) {
        cat("###########################################\n")
        print("Invalid interval")
    } else if (length(intervaldays == 2) & intervaldays[1] >= minNday
               & intervaldays[2] <= maxNday
               & intervaldays[2] > intervaldays [1]) {
        if (exp.days == TRUE){
            #intervaldays[1] <- intervaldays[1] + Rday[1]-1
            #intervaldays[2] <- intervaldays[2] + Rday[1]-1
            intervaldays[1] <- intervaldays[1]
            intervaldays[2] <- intervaldays[2]
            intervald <- intervaldays + Rday[1]-1

        }
        break
    } else {
        cat("###########################################\n")
        print("Invalid interval")
    }
}

###
###


#################################
#################################
} else {
    
    #################################
    #################################
    
    
    ### This loops allows to avoid the error when someone write wrong the data
    validpinnamultiple <- function(){
        cat("###########################################\n")
        cat("Choose what variables, separate the numbers by commas without spaces\n")
        if(datagroup == 1) {
            numbord <- allindividuals
            print(paste(allindividuals, individualsname, sep = "-"))
        } else {
            numbord <- allsensors
            print(paste(allsensors, sensorsname, sep = "."))
        }
        whatpinnas = readline(prompt = "Individuals:")
    } 
    repeat{
        Pinna <- validpinnamultiple()
        
        Pinnaselec = as.numeric(unlist(strsplit(split=",", (Pinna))))
        
        
        if(anyNA(Pinnaselec)) {
            cat("###########################################\n")
            print("Error0, try again")
        } else if (length(Pinnaselec) == 0){
            print("Error1, try again") 
            #} else if (any(duplicated(Pinnaselec))) {
            #    print("Error2, try again") 
        } else if (all(Pinnaselec >= min(allindividuals)) & all(Pinnaselec <= max(allindividuals))
                   & all(length(Pinnaselec) > 0)){break
        } else{
            print("That doesn't seems like and individual")
        }
    }
    
    ###
    
    
    ### This loops allows to avoid the error when someone write wrong the data
    validintervalmultiple <- function(){
        cat("Choose a julian day interval for each individual.\n")
        ##cat("###########################################\n")
        #cat("Put each interval with a short dash between them (both days included).\n")
        #cat("Interval days: ", min(dataindividuals$Julianday),"-", round_any(max(dataindividuals$Julianday), 1, f = floor), 
        #cat("And separate intervals with commas without spaces 34-45,34-60 .\n")
        #    sep = "", "\n")
        #whatinterval = readline(prompt = "Interval: ")
        
        cat("###########################################\n")
        cat("Choose a julian day interval for each individual.\n")
        cat("Select the intervals: 35-67,35-78 (both days included of each interval).\n")
        cat("First day", minNday, "Last day", maxNday, "\n")
        #cat("Interval days: ", min(data1[,1]),"-", round_any(max(data1[,1]), 1, f = floor), 
        #    sep = "", "\n")
        whatinterval = readline(prompt = "Interval: ")
    } 
    
    repeat{
        valid = FALSE
        whatinterval2 <- validintervalmultiple()
        interval <- unlist(strsplit(split=",", (whatinterval2)))
        
        if (length(interval) == length(Pinnaselec)){
            for (i in 1:length(interval)){
                intervaldays2 <- as.numeric(unlist(strsplit(split="-", (interval[i]))))
                
                if (anyNA(intervaldays2)) {
                    cat("###########################################\n")
                    print("Invalid interval 1")
                } else if (length(intervaldays2) == 2 & length(interval) == length(Pinnaselec) 
                           & intervaldays2[1] >= minNday
                           & intervaldays2[2] <= maxNday 
                           & intervaldays2[2] > intervaldays2 [1]) {
                    
                    valid = TRUE
                    break
                } else {
                    cat("###########################################\n")
                    print("Invalid interval 2")
                    
                }
            }
        } else {"Invalid interval 3"}
        if (valid == TRUE) {break} else {}
    }
    
    
    ###
    
}

