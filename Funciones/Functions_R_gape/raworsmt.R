validresponse <- function(){
    cat("###########################################\n")
    cat("Do you want to use original data or smoothed data?\n")
    cat("Write -org- for original data or -smt- for smoothed data.\n")
    whatresponse = readline(prompt = "Response:")
} 
repeat{
    raworsmt <- validresponse()
    
    if(raworsmt == "org"){break
    } else if (raworsmt == "smt") {break
    } else {
        print("Come on it's easy, just only two options, try again")
    }
}