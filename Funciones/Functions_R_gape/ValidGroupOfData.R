### This loops allows to avoid the error when someone write wrong the data
Groupofdata <- function(){
    cat("###########################################\n")
    cat("Choose with what group of data do you want to work.\n")
    cat("Select 1 for group 1 or 2 for group 2\n")
    whatgroup = readline(prompt = "Group: ")
} 


repeat{
    datagroup <- Groupofdata()
    datagroup <- as.numeric(datagroup)
    
    if (anyNA(datagroup)) {
        cat("###########################################\n")
        print("Invalid interval")
    } else if (datagroup == 1 | datagroup == 2) {
        break
    } else {
        cat("###########################################\n")
        print("Invalid group of data")
    }
}

###
###