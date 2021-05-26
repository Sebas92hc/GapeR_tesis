### This loops allows to avoid the error when someone write wrong the data
validintervalone <- function(){
    cat("###########################################\n")
    cat("Choose an interval.\n")
    cat("Select two julian days with a short dash between them, 35-67, (both days included).\n")
    cat("Interval days: ", min(dataindividuals$Julianday),"-", round_any(max(dataindividuals$Julianday), 1, f = floor), 
        sep = "", "\n")
    whatinterval = readline(prompt = "Interval: ")
} 


repeat{
    interval <- validintervalone()
    intervaldays <- as.numeric(unlist(strsplit(split="-", (interval))))
    
    if (anyNA(intervaldays)) {
        cat("###########################################\n")
        print("Invalid interval")
    } else if (length(intervaldays == 2) & intervaldays[1] >= min(dataindividuals$Julianday)
               & intervaldays[2] <= max(dataindividuals$Julianday) 
               & intervaldays[2] > intervaldays [1]) {
        break
    } else {
        cat("###########################################\n")
        print("Invalid interval")
    }
}

###
###