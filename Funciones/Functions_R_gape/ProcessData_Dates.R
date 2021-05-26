ProcessDataDates <- function(dataofIntfPinna, dataofprofilux){
    
    library(RSEIS)
    library(plyr)
    library(tidyr)
    library(dtplyr)
    library(dplyr)
    library(data.table)
    library(zoo)
    
    repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
        ind = which(!is.na(x))      # get positions of nonmissing values
        if(is.na(x[1]))             # if it begins with a missing, add the 
            ind = c(1,ind)        # first position to the indices
        rep(x[ind], times = diff(   # repeat the values at these indices
            c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
    }                               # they need to be repeated
    
    
    
    datosinterf <- fread("Datos/Pinna_Activity/Pinna_Interface/Datos/union_25_27_29.txt", 
                         sep = ",", 
                         dec = ".",
                         data.table = FALSE,
                         colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    
    datosrecup <- fread("Datos/Pinna_Activity/Pinna_Interface/Datos/datosrecuperados.txt", 
                        sep = ",", 
                        dec = ".",
                        data.table = FALSE,
                        colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    # Para juntar los datos con los datos que se perdieron
    #Convierto de los datos la columna con la fecha y hora en POSIXct
    datostt <- datosinterf$V1
    datosinterf$V1 <- as.POSIXct(datostt, tz = "GMT")
    
    # Convierto de los datos recuperados la columna con fecha y hora en POSIXct
    datosrt <- datosrecup$V1
    datosrecup$V1 <- as.POSIXct(datosrt, tz = "GMT")
    
    # Como los datos recuperados solo se toma dato cada vez que hay un cambio,
    ## creo una secuencia con un dato de fecha cada segundo, y hago un merge entre
    ### esa secuencia y la de datos recuperados. Luego con la función na.locf me
    #### completa todos los huecos que hay NA con el último dato tomado.
    alldatesrecup <- data.frame(seq(from = datosrecup$V1[1], to = datosrecup$V1[length(datosrecup$V1)], by = 1))
    colnames(alldatesrecup) <- c("V1")
    datosrecup <- merge.data.frame(x = datosrecup, y = alldatesrecup, 
                                   by = c("V1"), all.x = TRUE, all.y = TRUE)
    datosrecup <- na.locf(datosrecup)
    
    # Al hacer el na.locf me convierte todas las columnas en "character", así que
    ## hago un as.numeric para todas menos la primera (que es la fecha y hora)
    for (i in 2:length(datosrecup)) {
        datosrecup[,i] <- as.numeric(datosrecup[,i])
        
    }
    
    # Convierto la columna de fecha y hora en POSIXct. 
    datosrt <- datosrecup$V1
    datosrecup$V1 <- as.POSIXct(datosrt, tz = "GMT")
    
    # Hago un merge entre los datos y los datos recuperados ya procesados.
    datosinterf <- merge.data.frame(x = datosinterf, y = datosrecup, 
                                    by = c("V1", "V2", "V3", "V4", "V5", 
                                           "V6", "V7", "V8", "V9", "V10", 
                                           "V11", "V12", "V13", "V14"), 
                                    all.x = TRUE, all.y = TRUE)
    
    
    
    
    datost <- select(datosinterf, 1)
    datost2 <- separate(data = datost, col = V1, into = c("Date", "Hour"), sep = " ")
    
    tempsens <- select(datosinterf, 12, 13)
    
    
    datosinterf$V1 <- NULL; datosinterf$V6 <- NULL; datosinterf$V11 <- NULL
    datosinterf$V12 <- NULL; datosinterf$V13 <- NULL; datosinterf$V14 <- NULL
    colnames(datosinterf) <- c("Pinna.1", "Pinna.2", "Pinna.3", "Pinna.4", "Pinna.5", 
                               "Pinna.6", "Pinna.7", "Pinna.8")
    
    
    ###1,7seg
    # V6 Y V11 are not there because correspond to pinna.5A and pinna.5B. Rightn now
    #we only have until pinna.4. Apply aa to pinna.5
    aa <- function(x){x - 7}
    datosf2 <- sapply(datosinterf$Pinna.5, aa)
    
    
    
    
    #29,7seg a 10seg
    ##Convierto la columna de fecha en días julianos
    datostdate <- select(datost2, 1)
    x1 <- separate(data = datostdate, col = Date, into = c("Year", "Month", "Day"), sep = "-")
    d <- as.numeric(as.character(x1$Day))
    m <- as.numeric(as.character(x1$Month)) ## Factor hay que convertirlo primero a caracter y luego a número
    y <- as.numeric(as.character(x1$Year))
    jd <- getjul(year = y, month = m, day = d)
    
    
    #de 26seg a 7seg
    ##Convierto la columna de hora en días julianos  
    datosthour <- select(datost2, 2)
    x2 <- separate(data = datosthour, col = Hour, into = c("Hour", "Minute", "Second"), sep = ":")
    h <- as.numeric(as.character(x2$Hour))
    m <- as.numeric(as.character(x2$Minute)) 
    s <- as.numeric(as.character(x2$Second))
    
    
    
    julday <- jd + h/24 + m/(24*60) + s/(24*3600)
    Time <- h*3600 + m*60 + s
    
    
    
    
    
    ###################################
    ###################################
    ###################################
    
    foodtime <- cbind(as.matrix(julday), as.matrix(Time),rep(0, length(julday), replace = TRUE))
    
    for (i in 1:length(foodtime[,1])){
        
        if(foodtime[i,1] < 120){
            
            if(any(foodtime[i,2] == 1, foodtime[i,2] == 10801, foodtime[i,2] == 21601, foodtime[i,2] == 32401,
                   foodtime[i,2] == 43201, foodtime[i,2] == 54001, foodtime[i,2] == 64801, foodtime[i,2] == 75601)) {
                foodtime[i,3] <- 1} 
            else {foodtime[i,3] <- 0}
        }
        
        if(foodtime[i,1] >= 120){
            
            if(any(foodtime[i,2] == 1, foodtime[i,2] == 21601, foodtime[i,2] == 43201, foodtime[i,2] == 64801)) {
                foodtime[i,3] <- 1} 
            else {foodtime[i,3] <- 0}
        }
        
    }
    
    foodtime2 <- data.frame(foodtime)
    
    
    
    ###################################
    ###################################
    ###################################
    
    
    
    datalight3 <- as.matrix(julday)
    datalight <- cbind(datalight3,rep(0, length(datalight3[,1]), replace = TRUE))
    
    
    for (i in 1:length(datalight[,1])){
        
        #    if(datalight[i,1] == datalight[1,1]){
        
        #       datalight[i,2] <- 2 
        #   }
        
        #datalight[i,1] > datalight[1,1] & 
        if(datalight[i,1] <= 135.5){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(any(datalight[i,1] - jul <= 0.25, datalight[i,1] - jul >= 0.75)) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        if(datalight[i,1] > 135.5 & datalight[i,1] <= 137.5){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(any(datalight[i,1] - jul <= 0.25, datalight[i,1] - jul >= 0.8125)) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 137.5 & datalight[i,1] <= 139.5){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(any(datalight[i,1] - jul <= 0.25, datalight[i,1] - jul >= 0.875)) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 139.5 & datalight[i,1] <= 141.5){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(any(datalight[i,1] - jul <= 0.25, datalight[i,1] - jul >= 0.9583)) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 141.5 & datalight[i,1] <= 142.5){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.0417 & datalight[i,1] - jul <= 0.2500) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 142.5 & datalight[i,1] < 144){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.125 & datalight[i,1] - jul <= 0.2500) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] >= 144 & datalight[i,1] < 145){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.7917 & datalight[i,1] - jul <= 0.8750) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] >= 145 & datalight[i,1] < 147){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.7917 & datalight[i,1] - jul <= 0.9583) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        if(datalight[i,1] >= 147 & datalight[i,1] <= 175){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.8333 & datalight[i,1] - jul <= 0.99999) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 175 & datalight[i,1] <= 176){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.4896 & datalight[i,1] - jul <= 0.6389) {
                datalight[i,2] <- 1} 
            else if(datalight[i,1] - jul >= 0.8333 & datalight[i,1] - jul <= 0.99999) {
                datalight[i,2] <- 1} 
            else {
                datalight[i,2] <- 2}
        }
        
        
        if(datalight[i,1] > 176 & datalight[i,1] < 181){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.8333 & datalight[i,1] - jul <= 0.99999) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
        
        if(datalight[i,1] >= 181 & datalight[i,1] < 182){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            datalight[i,2] <- 2
            #  if(datalight[i,1] - jul >= 0.8333 & datalight[i,1] - jul <= 0.99999) {
            #     datalight[i,2] <- 1} 
            #else {datalight[i,2] <- 2}
        }
        
        if(datalight[i,1] > 182 & datalight[i,1] <= 188){
            
            jul <- round_any(datalight[i,1], 1, floor)
            # 1 is night, 2 is day
            if(datalight[i,1] - jul >= 0.8333 & datalight[i,1] - jul <= 0.99999) {
                datalight[i,2] <- 1} 
            else {datalight[i,2] <- 2}
        }
    }
    
    datalight2 <- as.data.frame(datalight)
    
    
    
    
    dataindividuals <- data.frame(julday, datost, datalight2$V2, foodtime2$X3, datosinterf$Pinna.1, datosinterf$Pinna.2, datosinterf$Pinna.3, datosinterf$Pinna.4,
                                  datosf2, datosinterf$Pinna.6, datosinterf$Pinna.7, datosinterf$Pinna.8)
    colnames(dataindividuals) <- c("Julianday", "Date", "Light", "Food", "Pinna.1A", "Pinna.2A", 
                                   "Pinna.3A", "Pinna.4A", "Pinna.1B", 
                                   "Pinna.2B", "Pinna.3B", "Pinna.4B")
    
    #dayslack <- data.frame(seq(from = min(dataindividuals$Julianday), to = max(dataindividuals$Julianday), by = dataindividuals$Julianday[2]-dataindividuals$Julianday[1]))
    #colnames(dayslack) <- c("Julianday")
    #dataindividuals <- merge.data.frame(x = dataindividuals, y = data.frame(dayslack), 
    #                              by = c("Julianday"), 
    #                             all.x = TRUE, all.y = TRUE)
    ################################
    ################################
    ################################
    ################################
    
    datos <- fread("Datos/Pinna_Activity/Profilux/Datos/Experimento_(220416)_2.txt", 
                   sep = ";", 
                   header = TRUE, 
                   dec = ",",
                   colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL"))
    
    
    datos$Time <- sub("^$", "0:00:00", datos$Time)
    library(RSEIS)
    
    # Para juntar los datos con los datos que se perdieron
    #Convierto de los datos la columna con la fecha y hora en POSIXct
    datostt <- paste(datos$Date, " ", datos$Time, sep = "")
    datos$Date <- as.POSIXct(datostt, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    
    datos$Time <- NULL
    
    
    
    
    datasensors2 <- data.frame(dataindividuals$Date, tempsens)
    colnames(datasensors2) <- c("Date", "Temp.AF", "Temp.BF")
    
    # Junta los datos del profilux (1 dato cada 5min) con los sensores de 
    ## temperatura de Miguel Angel (1 dato cada seg). De está forma solo coge
    ### aquellos datos que coincidan (tomados en el mismo instante) para poder
    #### formar un data.frame
    
    #datasensors <- merge.data.frame(x = datjd, y = datasensors2, by.x = "Julianday", by.y = "Julianday", all.x = TRUE, all.y = FALSE)
    datasensors <- merge.data.frame(x = datos, y = datasensors2, by = "Date", all.x = TRUE, all.y = TRUE)
    
    # El merge los junta todos añadiendo NA en aquellos momentos en los que no 
    ## hay datos de profilux. Con el na.omit eliminamos estás filas de datos.
    datasensors <- na.omit(datasensors)
    
    
    assign("datasensors", datasensors, envir = .GlobalEnv)
    colnames(datasensors) <- c("Date", "pH.A", "Temp.A", "Sal.A", "Ox.A", "pH.B", 
                               "Temp.B", "Sal.B", "Ox.B", "Temperature.AF", "Temperature.BF")
    
    
    
    # This allows to subsample the data.
    each2 <- c(TRUE,FALSE)
    each5 <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
    each10 <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
    each15 <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
    each20 <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
    each30 <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
    
    dataindividuals <- dataindividuals[each2,]
    
    dataindividuals$avA <- rowMeans(dataindividuals[,c(5,6,7)], na.rm = TRUE)
    dataindividuals$avB <- rowMeans(dataindividuals[,c(9,10,11)], na.rm = TRUE)
    dataindividuals$avAll <- rowMeans(dataindividuals[,c(5,6,7,9,10,11)], na.rm = TRUE)
    dataindividuals$av4AB <- rowMeans(dataindividuals[,c(8,12)], na.rm = TRUE)
    
    
    assign("dataindividuals", dataindividuals, envir = .GlobalEnv)
    
    
}









