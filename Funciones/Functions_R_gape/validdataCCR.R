validdataCCR <- function(){
    cat("Select the two variables do you want to use")
    cat("Ex: data$Indv1,data2$Temp")
    whatdata = readline(prompt = "Group of data: ")
} 

repeat{
    variable <- validdataCCR()

    if(anyNA(unlist(strsplit(split=",", (variable))))) {
        cat("###########################################\n")
        print("Error0, try again")
    } else {
        variableselect <- unlist(strsplit(split=",", (variable)))
        dataselec1 = unlist(strsplit(split="$", (variableselect[1]), fixed = TRUE))
        dataselec2 = unlist(strsplit(split="$", (variableselect[2]), fixed = TRUE))
        if (exists(dataselec1[1]) & exists(dataselec2[1])) {
            break
        } else{
            print("There is no such variable")
        }
    } 
}
    