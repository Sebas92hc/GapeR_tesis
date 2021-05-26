### This loops allows to avoid the error when someone write wrong the data
validintervalmultiple <- function(){
    cat("###########################################\n")
    cat("Choose a julian day interval for each individual.\n")
    cat("Put each interval with a short dash between them (both days included).\n")
    cat("And separate intervals with commas without spaces 34-45,34-60 .\n")
    cat("Interval days: ", min(dataindividuals$Julianday),"-", round_any(max(dataindividuals$Julianday), 1, f = floor), 
        sep = "", "\n")
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
            } else if (length(intervaldays2) == 2 & intervaldays2[1] >= min(dataindividuals$Julianday)
                       & intervaldays2[2] <= max(dataindividuals$Julianday) 
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