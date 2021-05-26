### This loops allows to avoid the error when someone write wrong the data
validpinnamultiple <- function(){
    cat("###########################################\n")
    cat("Choose all individuals you want, separated the numbers by commas without spaces\n")
    cat("Individuals allowed: ", min(allindividuals),"-", max(allindividuals), sep = "", "\n")
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
    } else if (Pinnaselec >= min(allindividuals) & Pinnaselec <= max(allindividuals)
               & length(Pinnaselec) > 0){break
    } else{
        print("That doesn't seems like and individual")
    }
}

###