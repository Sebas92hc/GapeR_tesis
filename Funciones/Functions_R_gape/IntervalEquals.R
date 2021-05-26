IntervalEquals <- function(){
    cat("###########################################\n")
    cat("All your individuals have the same interval of days?\n")
    cat("Write -y- if yes or -n- if not.\n")
    whatresponse = readline(prompt = "Response:")
} 
repeat{
    yesornot <- IntervalEquals()
    
    if(yesornot == "y"){break
    } else if (yesornot == "n") {break
    } else {
        print("Come on, it's easy, try again")
    }
}