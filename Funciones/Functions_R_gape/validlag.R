validlag <- function(){
    cat("Choose what lag")
    whatlag = readline(prompt = "Lag: ")
} 

repeat{
    Lag <- validlag()
    Lagselect <- as.numeric(Lag)
    
    if(anyNA(Lagselect)) {
        cat("###########################################\n")
        print("Error0, try again")
    } else if(length(Lagselect) == 1 & Lagselect >= 1 & Lagselect <= 1000000){
        break
    } else{
        print("Error1, try again")
        
    }
}