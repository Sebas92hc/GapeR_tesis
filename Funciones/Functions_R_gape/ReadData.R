library(plyr)
library(RSEIS)
library(tidyr)
library(dtplyr)
library(dplyr)
library(data.table)

ReadData <- function(dataofIntfPinna, dataofprofilux){
    
    dataindividuals <- fread(dataofIntfPinna, 
                             sep = ",", 
                             dec = ".",
                             data.table = FALSE,
                             colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    
    datasensors <- fread(dataofprofilux, 
                         sep = ",", 
                         dec = ".",
                         data.table = FALSE,
                         colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    
    assign("datasensors", datasensors, envir = .GlobalEnv)
    
    assign("dataindividuals", dataindividuals, envir = .GlobalEnv)
    
}

