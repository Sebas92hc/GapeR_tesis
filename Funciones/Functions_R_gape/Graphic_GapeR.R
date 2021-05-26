source("Funciones/Functions_R_gape/Colorfunction2.R")
source("Funciones/Functions_R_gape/AstroFunctions.R")
source("Funciones/Functions_R_gape/Stadistic.R")



source("Funciones/Functions_R_gape/Legend.R")

library(plyr)
library(SGAT)
library(devtools)
#library(dplyr)
library(tidyr)
#library(data.table)
library(dtplyr)
library(RSEIS)
library(RPMG)
library(fMultivar)
library(signal)
library(stats)
library(RColorBrewer)
library(randomcoloR)
#library(tripEstimation)
#x11()
# Number of the first device
#ActualDevice <- dev.cur()




Lightdata = TRUE; Fooddata = TRUE; subsetdata = FALSE
DataGroup1 = TRUE; DataGroup2 = TRUE
Selectbuttons <- c(DataGroup2, Lightdata, Fooddata)
PosButtons <- c("Sensors", "Light", "Food")
PosButtons2 <- PosButtons[Selectbuttons]


Light = TRUE; Food = FALSE; MOON = FALSE

LFM <- c(Light, Food)
numberLFM <- length(LFM)
LFMnames <- c("Light", "Food")

DoAstro = TRUE

lwdlegend <- 2  # Width of the legend lines, usually I use 2 (3 for jpeg)
ltylegend<- 1
cexlegend <- 0.7  # Text size of legend, usually I use 0.7 (2 for jpeg)
pnglwd <- 3 # Width of the line of High and Low freq png
pngaxistitle <- 1.2 # Size of the axis title of High and Low freq png
pngaxisnum <- 1.5 # Size of the axis numbers of High and Low freq png
lwdsensors <- 2.5 # Width of the lines, usually I use 2.5 (3 for jpeg)
lwdindv <- 3 # Width of the lines, usually I use 3 (3 for jpeg)
ltysensors <- 1
ltyindv <- 1
axisizeX <- 1 # Size of the text in axis X, usually I use 1 (2 for jpeg)
axisizeY <- 1 # Size of the text in axis Y, usually I use 1 (2.5 for jpeg)



# Program initiation, this probably must be in readinterface
#cat("With how many individuals are you working?")
#HowManyIndividuals = readline(prompt = "Number of individuals:")
numberindividuals <- 10 #This is the value that the user will choose (17 Nioz, 9 PINNA)
allindividuals <- seq(1, numberindividuals, by = 1)
## This  lines are created to, when click a button in the plot, change
## this parameters to FALSE and change the plot.
individualsname2 <- colnames(dataindividuals)

if (Lightdata != Fooddata) {
    individualsname <- individualsname2[-c(1,2,3)]
} else if (Lightdata == TRUE & Fooddata == TRUE) {
    individualsname <- individualsname2[-c(1,2,3,4)]
} else {    individualsname <- individualsname2[-c(1,2)]
}
# if light TRUE individualsname <- individualsname2[-c(1,2,3,length(individualsname2))]
# if light TRUE 

individuals <- rep(NA, length(allindividuals), replace = TRUE)
for(i in 1:length(allindividuals)){
    allindividuals2 <- paste(individualsname[i], sep = "")
    individuals[i] <- assign(allindividuals2, value = FALSE)
    
}

if (DataGroup2){
    #cat("How many sensors are you using?")
    #HowManySensors = readline(prompt = "Number of sensors:")
    numbersensors <- 10 #This is the value that the user will choose (9 Nioz, 10 PINNA)
    allsensors <- seq(1, numbersensors, by = 1)
    ## This  lines are created to, when click a button in the plot, change
    ## this parameters to FALSE and change the plot.
    sensorsname2 <- colnames(datasensors)
    sensorsname <- sensorsname2[-c(1,2,3)]
    sensors <- rep(NA, length(allsensors), replace = TRUE)
    for(i in 1:length(allsensors)){
        allsensors2 <- paste(sensorsname[i], sep = "")
        sensors[i] <- assign(allsensors2, value = FALSE)
    }
}


blackwhite = TRUE

if(blackwhite == FALSE){
    ## Select the colors
    indvcolors1 <- c("darkred", "deepskyblue", "darkseagreen", "darkgoldenrod1", "burlywood4", "aquamarine",
                     "darkorchid", "cyan", "lightblue", "darkslategray", "royalblue1", "maroon",
                     "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink", 
                     "lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                     "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                     "orange", "gray51", "indianred", "thistle3")
    indvcolors <- indvcolors1[allindividuals]
    
    senscolors1 <- c("lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                     "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                     "orange", "gray51", "indianred", "thistle3", "cyan", "deepskyblue", "darkred", "darkgoldenrod1", "burlywood4", "aquamarine",
                     "darkorchid", "darkseagreen", "lightblue", "darkslategray", "royalblue1", "maroon",
                     "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink")
    senscolors <- senscolors1[allsensors]
    
} else {
    indvcolors1 <- c("darkred", "deepskyblue", "darkseagreen", "darkgoldenrod1", "burlywood4", "aquamarine",
                     "darkorchid", "cyan", "lightblue", "darkslategray", "royalblue1", "maroon",
                     "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink", 
                     "lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                     "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                     "orange", "gray51", "indianred", "thistle3")
    indvcolors <- indvcolors1[allindividuals]
    
    senscolors1 <- c("lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                     "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                     "orange", "gray51", "indianred", "thistle3", "cyan", "deepskyblue", "darkred", "darkgoldenrod1", "burlywood4", "aquamarine",
                     "darkorchid", "darkseagreen", "lightblue", "darkslategray", "royalblue1", "maroon",
                     "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink")
    senscolors <- senscolors1[allsensors]
    
    
    
}
## Select the colors




## buttons that are going to be plotted in allbuttons
monthnames <- c("Ene", "Feb", "Mar","Abr", "May", "Jun", "Jul", "Ago", 
                "Sep", "Oct", "Nov", "Dic")
# Eliminated "D.Set" and Locator button. Is now useful right now.
basicbuttons = c("Refresh", "Dev.Off", "Text", "JPG", "Locator", "Next", "Prev", "GoTo", "SelecD", "NDay", "MOON")

sensorbuttons = sensorsname

# Button "lines" eliminated
pinnabuttons <- c("SelPinn", "Stadistic")

allbuttons = c(basicbuttons, PosButtons2, sensorbuttons, pinnabuttons)

## More buttons
FFTbuttons <- c("D.Set", "Text", "JPG")



## Created to make possible to do the legends
buttonslegend <- c(individualsname, sensorsname)
lightbuttons <- c("Light")
lightbuttons2 <- c("Day", "Night")



# Representar individuos

individuals <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)

NDay = 10
Rday <- seq(min(dataindividuals$Julianday), max(dataindividuals$Julianday), by = 1)
Inicialday <- 1
minJD <- Rday[Inicialday]
xday <- minJD + NDay



###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

jpng(paste("Graphic", sep = ""), P=c(14,9.8) ) 

# sebasplot <- function(dataindividuals) {
    
    if (DataGroup2){
        whatdaysens <- which(datasensors$Julianday>=minJD & datasensors$Julianday<=xday)
    }
    
    whatdaypinn <- which(dataindividuals$Julianday>=minJD & dataindividuals$Julianday<=xday)
    
    
    ## Para que la función pretty coja el año que toca, sino da dias equivocados.
    datayear <- strsplit(dataindividuals$Date[minJD], split = "-")
    year <- as.numeric(datayear[[1]][1])
    
    ## Esto serviría para que, una vez con los botones le digo que X individuos son TRUE, se actualice
    ## allindividuals con cuales son TRUE y cuales son FALSE. Útil para legend
    for(i in 1:length(allindividuals)){
        individuals2 <-  paste(individualsname[i], sep = "")
        individuals1 <- parse(text = individuals2)
        individuals[i] <- eval(individuals1)
    }
    
    for(i in 1:length(allsensors)){
        sensor2 <-  paste(sensorsname[i], sep = "")
        sensor1 <- parse(text = sensor2)
        sensors[i] <- eval(sensor1)
    }
    
    
    ##
    ##
    ## Axis Y
    TRUEindv2 <- c(individuals)
    TRUEindv1 <- c(individualsname)
    TRUEindv <- TRUEindv1[TRUEindv2]
    
    if (length(TRUEindv) > 0){
        dataaxisindv <- data.frame(matrix(ncol = length(TRUEindv), nrow = 
                                              length(dataindividuals$Julianday[whatdaypinn])))
        colnames(dataaxisindv) <- TRUEindv
        for(i in 1:length(TRUEindv)){
            dataaxis2 <-  paste("dataindividuals$", TRUEindv[i], sep = "")
            dataaxis1 <- parse(text = dataaxis2)
            dataaxisindv0 <- eval(dataaxis1)
            dataaxisindv[i] <- dataaxisindv0[whatdaypinn]
            
        }
    } else {dataaxisindv = vector("numeric")}
    
    ## Puede darse el caso en el que un sensor no tenga datos, justo en el tiempo en el que lo
    ## quiero representar (he tenido que hacer un data.frame con unos sensores que muestraban cada
    ## 5min y otro que lo hacía cada segundo). Entonces he hecho este if, para que detecte si justo
    ## esos datos han pasado los filtros anteriores, para quitarlos, porque entonces me daría error
    ## al poner los valores del eje Y.
    if(is.na(any(dataaxisindv >= 0))){
        dataaxisindv = vector("numeric")
    }
    
    
    if (length(dataaxisindv) > 0) {
        minLeftaxis <- min(dataaxisindv, na.rm = TRUE)
        maxLeftaxis <- max(dataaxisindv, na.rm = TRUE)
        minLeftaxis2 <- round_any(minLeftaxis, 1, f = floor)
        maxLeftaxis2 <- round_any(maxLeftaxis, 1, f = ceiling)
        
    } else if (length(dataaxisindv) == 0) {
        minLeftaxis2 <- 0
        maxLeftaxis2 <- 0
    } else {
        minLeftaxis2 <- 0
        maxLeftaxis2 <- 0
    }
    
    
    
    
    #############################################
    
    TRUEsens2 <- c(sensors)
    TRUEsens1 <- c(sensorsname)
    TRUEsens <- TRUEsens1[TRUEsens2]
    
    
    if (length(TRUEsens) > 0){
        dataaxissens <- data.frame(matrix(ncol = length(TRUEsens), nrow = 
                                              length(datasensors$Julianday[whatdaysens])))
        colnames(dataaxissens) <- TRUEsens
        for(i in 1:length(TRUEsens)){
            dataaxis2 <-  paste("datasensors$", TRUEsens[i], sep = "")
            dataaxis1 <- parse(text = dataaxis2)
            dataaxissens0 <- eval(dataaxis1)
            dataaxissens[i] <- dataaxissens0[whatdaysens]
        }
    } else {dataaxissens = vector("numeric")}
    
    
    
    ## Puede darse el caso en el que un sensor no tenga datos, justo en el tiempo en el que lo
    ## quiero representar (he tenido que hacer un data.frame con unos sensores que muestraban cada
    ## 5min y otro que lo hacía cada segundo). Entonces he hecho este if, para que detecte si justo
    ## esos datos han pasado los filtros anteriores, para quitarlos, porque entonces me daría error
    ## al poner los valores del eje Y.
    if(is.na(any(dataaxissens >= 0))){
        dataaxissens = vector("numeric")
    }
    
    
    if (length(dataaxissens) == 0) {
        minRightaxis2 <- 0
        maxRightaxis2 <- 0
        
    } else if (length(dataaxissens) > 0) {
        minRightaxis <- min(dataaxissens, na.rm = TRUE)
        maxRightaxis <- max(dataaxissens, na.rm = TRUE)
        minRightaxis2 <- round_any(minRightaxis, 1, f = floor)
        maxRightaxis2 <- round_any(maxRightaxis, 1, f = ceiling)
    } else {
        minRightaxis2 <- 0
        maxRightaxis2 <- 0
    }
    
    
    ## Axis Y
    ##
    ##
    minLeftaxis2 <- 0
    #maxLeftaxis2 <- 30
    
    
    if (DataGroup2){
        plotdaysens <- datasensors$Julianday[whatdaysens]
        
    }
    plotdaypinn <- dataindividuals$Julianday[whatdaypinn]
    
    
    Rplotday <- range(plotdaypinn, na.rm = TRUE)
    #addplotday = c(Rplotday[1]-0.06*diff(Rplotday), Rplotday[2])
    addplotday = c(Rplotday[1]-(diff(Rplotday)*0.03), Rplotday[2])
    Rsensor <- range(dataindividuals[4], dataindividuals[5])
    
    #Rplotday <- range(plotdaysens, na.rm = TRUE)
    #addplotday = c(Rplotday[1]-0.06*diff(Rplotday), Rplotday[2])
    #Rsensor <- range(datasensors[4], datasensors[5])
    
    
    
    
    ## In that last lines we are seleccting the julian days (axis X) that are going to
    ## be represented for the sensors and the pinna data.
    
    #X11()
    minjd <- min(addplotday) ## Minimum julian day value for the plot
    maxjd <- max(addplotday) ## Maximum julian day value for the plot
    par(mai = c(1,1,1,1))
    plot(addplotday, Rsensor, type = "n", axes = FALSE,
         xlab = "", ylab = "", ylim = c(minLeftaxis2, maxLeftaxis2), xlim = c(minjd, maxjd))
    
    #####################################
    #####################################
    #####################################
    #####################################
    
    if (Lightdata & Light){
        #Background light
        # Este if es porque si los primeros datos son de noche, hace falta que el
        ## primer dato sea diferente para que luego ese dato se seleccione y haya
        ### un primer dato desde el que dibujar el rectángulo.
        if (freq != 1) {# Aquí ira subsetdata == FALSE
            
            if(dataindividuals$Light[2] == 1) {
                dataindividuals$Light[1] <- 2
            } else {}
            
            int <- dataindividuals[c(diff(as.numeric(dataindividuals$Light)) == 1 | diff(as.numeric(dataindividuals$Light)) == -1, TRUE), ]
            int2 <- select(int, 1)
            
            n = 0
            for (i in 1:(length(int2$Julianday)/2)){
                
                if (i == 1){
                    rect(xleft = int2$Julianday[1], ybottom = minLeftaxis2-10, xright = int2$Julianday[2], ytop = maxLeftaxis2+10, lty = 0, col = "gray45")
                } else {
                    n = n+1
                    rect(xleft = int2$Julianday[i+n], ybottom = minLeftaxis2-10, xright = int2$Julianday[i+n+1], ytop = maxLeftaxis2+10, lty = 0, col = "gray45")
                }
            }
        } else {
            # De esta forma, si cambio los datos y submuestreo, puedo seguir teniendo
            ## la luz y en el lugar que corresponde, ya que en los nuevos datos
            ### submuestreados puede faltar justo el dato que contiene el cambio
            #### en la luz
            int <- fread("Datos/Pinna_Activity/Pinna_Interface/Datos/Light_0000.txt", 
                         sep = ",", 
                         dec = ".",
                         data.table = FALSE,
                         colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
            int2 <- select(int, 1)
            
            n = 0
            for (i in 1:(length(int2$Julianday)/2)){
                
                if (i == 1){
                    rect(xleft = int2$Julianday[1], ybottom = minLeftaxis2-10, xright = int2$Julianday[2], ytop = maxLeftaxis2+10, lty = 0, col = "gray45")
                    
                } else {
                    n = n+1
                    rect(xleft = int2$Julianday[i+n], ybottom = minLeftaxis2-10, xright = int2$Julianday[i+n+1], ytop = maxLeftaxis2+10, lty = 0, col = "gray45")
                }
                
            }
        }
        
    }
    #####################################
    #####################################
    #####################################
    #####################################
    
    #Background Food
    if (Fooddata & Food) {
        
        if (freq != 1) { # Aquí ira subsetdata == FALSE
            foodline <- subset.data.frame(x = dataindividuals, dataindividuals$Food == 1)
            
            
            for (i in 1:length(foodline$Julianday)){
                abline(v = foodline$Julianday[i], col = "darkgreen", lty = 2)
            }
            
        } else {
            foodline <- fread("Datos/Pinna_Activity/Pinna_Interface/Datos/Light_0000.txt", 
                              sep = ",", 
                              dec = ".",
                              data.table = FALSE,
                              colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
            for (i in 1:length(foodline$Julianday)){
                abline(v = foodline$Julianday[i], col = "darkgreen", lty = 2)
            }
        }
    }
    
    
    #####################################
    #####################################
    #####################################
    #####################################
    box()
    
    
    #axis(side = 2, col = axiscolor(Y1), las = 1, 
    #     at = seq(minLeftaxis2, maxLeftaxis2, by = ((maxLeftaxis2-minLeftaxis2))/10), cex.axis = axisizeY)
    
    axis(side = 2, col = axiscolor(Y1), las = 1, 
         at = seq(minLeftaxis2, maxLeftaxis2, by = 1), cex.axis = axisizeY)
    
    #####################################
    #####################################
    #####################################
    #####################################
    
    
    
    ###### Axis X.
    monthnames <- c("Ene", "Feb", "Mar","Abr", "May", "Jun", "Jul", "Ago", 
                    "Sep", "Oct", "Nov", "Dic")
    boxcoord <- par("usr")
    if (DataGroup2){
        posdaysaxisX <- 1
        
    } else {
        posdaysaxisX <- 1
    }
    
    
    if(NDay == 20){
        selecday20 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 2)
        getday20 <- getmoday(selecday20, year) 
        
        mymonths20 <- monthnames[getday20$mo]
        mydays20 <- getday20$dom
        mydate20 <- paste(sep=" ", mymonths20, mydays20)
        
        axis(1, at=selecday20, cex.axis = axisizeX, line = 0)
        axis(1, at=selecday20, labels= mydate20, lwd = 0, cex.axis = axisizeX, line = 1)
        
    } else if(NDay == 10) {
        selecday10 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 2)
        getday10 <- getmoday(selecday10, year) 
        
        mymonths10 <- monthnames[getday10$mo]
        mydays10 <- getday10$dom
        mydate10 <- paste(sep=" ", mymonths10, mydays10)
        
        axis(1, at=selecday10, cex.axis = axisizeX, line = 0)
        axis(1, at=selecday10, labels= mydate10, lwd = 0, cex.axis = axisizeX, line = 1)
        
        
        
    } else if(NDay == 30) {
        selecday30 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 3)
        getday30 <- getmoday(selecday30, year) 
        
        mymonths30 <- monthnames[getday30$mo]
        mydays30 <- getday30$dom
        mydate30 <- paste(sep=" ", mymonths30, mydays30)
        
        #axis(3, at=selecday30, pos=boxcoord[3])
        axis(1, at=selecday30, cex.axis = axisizeX, line = 0)
        axis(1, at=selecday30, labels=mydate30, lwd = 0, cex.axis = axisizeX, line = 1)
        
    } else if(NDay == 71) {
        selecday71 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 5)
        getday71 <- getmoday(selecday71, year) 
        
        mymonths30 <- monthnames[getday71$mo]
        mydays30 <- getday71$dom
        mydate30 <- paste(sep=" ", mymonths30, mydays30)
        
        #axis(3, at=selecday71, pos=boxcoord[3])
        axis(1, at=selecday71, cex.axis = axisizeX, line = 0)
        axis(1, at=selecday71, labels=mydate30, lwd = 0, cex.axis = axisizeX, line = 1)
        
    } else {
        selecday <- pretty(plotdaysens, n = 6)
        getday <- getmoday(round_any(selecday, 1, floor), year)
        ## He puesto un round_any porque el getmoday, si el dia juliano 56
        ## es 25 de febrero, el dia juliano 56.5 lo toma ya como 26 de febrero,
        ## cuando realmente sigue siendo 25. El round_any me redondea siempre 
        ## al número entero inferior, por lo que se soluciona el problema.
        mymonths <- monthnames[getday$mo]
        mydays <- getday$dom
        mydate <- paste(sep=" ", mymonths, mydays)
        
        #axis(3, at=selecday, pos=boxcoord[3])
        axis(1, at=selecday, cex.axis = axisizeX, line = 0)
        axis(1, at=selecday, labels=mydate, lwd = 0, cex.axis = axisizeX, line = 1)
        
    }
    
    mtext("Julian day", side = 1, line = 3, font = axisizeX)
    mtext("Date", side = 1, line = 3.8, font = axisizeX)
    
    mtext("Valve opening (mm)", side = 2, line = 3, font = axisizeX)
    
    #####################################
    #####################################
    #####################################
    #####################################
    
    
    ## Sensor legend
    
    
    lineindcolor <- allindividuals[individuals]
    
    leg <- FALSE
    if (any(individuals)) {
        
        indvlegend <- individualsname[which(individuals)]
        legindv <- legend("topleft", legend = indvlegend, lty = rep(ltylegend, numberindividuals, replace = TRUE),
                          col = indvcolors[lineindcolor], lwd = c(rep(lwdlegend, numberindividuals, replace = TRUE)), 
                          pt.cex = 0.85, cex = cexlegend, text.font = 2)
        leg <- TRUE
        
    } else {}
    
    if (leg){
        topleg <- legindv$rect$top
        botleg <- legindv$rect$top-legindv$rect$h
        leftleg <- legindv$rect$left  
    }
    
    ##############################################
    linesenscolor <- allsensors[sensors]
    leg2 <- FALSE
    
    
    if (any(sensors)) {
        sensorleg <- sensorsname[which(sensors)]
        
        if(leg){
            legsen <- legend(x = leftleg, y = botleg, legend = sensorleg, lty = rep(ltylegend, numbersensors, replace = TRUE),
                             col = senscolors[linesenscolor], lwd = c(rep(lwdlegend, numbersensors, replace = TRUE)), 
                             pt.cex = 0.78, cex = cexlegend, text.font = 2)
            
        } else{
            legsen <- legend("topleft", legend = sensorleg, lty = rep(ltylegend, numbersensors, replace = TRUE),
                             col = senscolors[linesenscolor], lwd = c(rep(lwdindv, numbersensors, replace = TRUE)), 
                             pt.cex = 0.85, cex = cexlegend, text.font = 2)
        }
        leg2 = TRUE
        
        
    } else {}
    
    if(leg2){
        topleg <- legsen$rect$top
        botleg <- legsen$rect$top-legsen$rect$h
        leftleg <- legsen$rect$left
    }
    
    
    #################################
    
    ## Add more legend
    leg3 <- FALSE
    
    if (any(Light)) {
        Lightleg <- c("Day", "Night")
        
        if(any(leg, leg2)){
            legLight <- legend(x = leftleg, y = botleg, legend = Lightleg, fill = c("white","gray90"), pt.cex = 0.78, cex = cexlegend, text.font = 2,
                               border = "black", box.cex = c(2,1))
            
        } else{
            legLight <- legend("topleft", legend = Lightleg, fill = c("white","gray90"), pt.cex = 0.78, cex = cexlegend, text.font = 2,
                               border = "black", box.cex = c(2,1))
        }
        leg3 = TRUE
        
    } else {}
    
    if(leg3){
        topleg <- legLight$rect$top
        botleg <- legLight$rect$top-legLight$rect$h
        leftleg <- legLight$rect$left
    }
    
    
    ## Add more legend
    leg4 <- FALSE
    
    if (any(Food)) {
        Foodleg <- "Food"
        
        if(any(leg, leg2, leg3)){
            legFood <- legend(x = leftleg, y = botleg, legend = Foodleg, col = "darkgreen", 
                              lty = 2, pt.cex = 0.78, cex = cexlegend, text.font = 2)
            
        } else{
            legFood <- legend("topleft", legend = Foodleg, col = "darkgreen", 
                              lty = 2, pt.cex = 0.78, cex = cexlegend, text.font = 2)
        }
        leg4 = TRUE
        
    } else {}
    
    if(leg4){
        topleg <- legFood$rect$top
        botleg <- legFood$rect$top-legFood$rect$h
        leftleg <- legFood$rect$left
    }
    
    
    ## Add more legend
    leg5 <- FALSE
    #####################################
    #####################################
    #####################################
    #####################################
    
    
    
    
    
    ##
    ##
    ## Lines individuals
    
    indvlines3 <- individualsname[individuals]
    if (length(indvlines3 > 0)) {
        
        for(i in 1:length(indvlines3)){
            #indvlines4 <- parse(text = indvlines5[i])
            #indvlines3 <- eval(indvlines4)
            indvlines2 <- paste("dataindividuals$", indvlines3[i], sep = "")
            indvlines1 <- parse(text = indvlines2)
            lines(plotdaypinn, eval(indvlines1)[whatdaypinn], type = "l", 
                  col = indvcolors[lineindcolor][i], lwd = lwdsensors)
        }
    }
    
    
    
    ## Lines individuals
    ##
    ## 
    
    
    if (any(sensors)){
    par(new = TRUE) ## Add a new Y axis in the right
    
    plot(addplotday, Rsensor, type = "n", axes = FALSE,
         xlab = "", ylab = "", ylim = c(minRightaxis2, maxRightaxis2), xlim = c(minjd, maxjd))
    
    axis(side = 4, col = axiscolor(Y1), las = 1, 
         at = seq(minRightaxis2, maxRightaxis2, by = ((maxRightaxis2-minRightaxis2))/10), cex.axis = axisizeY)
    }
    #######################################################
    #######################################################
    #######################################################
    ##
    ##
    ## Lines sensors
    
    if (DataGroup2){
        
        senslines3 <- sensorsname[sensors]
        if (length(senslines3 > 0)) {
            
            for(i in 1:length(senslines3)){
                #senslines4 <- parse(text = senslines5[1])
                #senslines3 <- eval(senslines4)
                senslines2 <- paste("datasensors$", senslines3[i], sep = "")
                senslines1 <- parse(text = senslines2)
                lines(plotdaysens, eval(senslines1)[whatdaysens], type = "l", lty = 2,  
                      col = senscolors[linesenscolor][i], lwd = lwdsensors)
            }
        }
    }
    
    
    
    ## Lines sensors
    ##
    ##
    #####################################
    #####################################
    #####################################
    #####################################
    
    
    
    
    
    
    {
        dev.off()
    }
    
