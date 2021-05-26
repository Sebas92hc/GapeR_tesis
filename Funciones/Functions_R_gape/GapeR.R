gapeR <- function(data1, data2 = NULL, lightdata = NULL, treatment = NULL,
                  coord = NULL, iday = c(2016, 04, 26, 0, 0, 0), 
                  fday = c(2016, 07, 05, 23, 59, 59), freq1 = 2, freq2 = 300, 
                  exp.days = FALSE, dates = TRUE, blackwhite = FALSE, fixLYaxis = NULL, fixRYaxis = NULL,
                  box = TRUE, bty = "o", mtexty = TRUE, legend = "vertical") {
    
    
    
    # bty = "n" sin box en legend. bty = "o" con box en legend.
    # mtexty = TRUE tenemos texto en el eje y. Si es FALSE, sin texto.
    # legend = vertical para leyenda vertical o legend = horizontal para leyenda horizontal
    iday = ISOdatetime(iday[1], iday[2], iday[3], iday[4], iday[5], iday[6], tz = "GMT" )
    
    fday = ISOdatetime(fday[1], fday[2], fday[3], fday[4], fday[5], fday[6], tz = "GMT" )
    
    Julianday <- colnames(data1)[1]
    
    
    source("Funciones/Functions_R_gape/Colorfunction2.R")
    source("Funciones/Functions_R_gape/AstroFunctions.R")
    source("Funciones/Functions_R_gape/Stadistic.R", local = TRUE)
    
    dir.create("Statistic_data/Low&HighF", showWarnings = FALSE)
    dir.create("Statistic_data/Low&HighF/Prints", showWarnings = FALSE)
    dir.create("Statistic_data/Low&HighF/Data", showWarnings = FALSE)
    
    dir.create("Statistic_data/FFT", showWarnings = FALSE)
    dir.create("Statistic_data/FFT/Prints", showWarnings = FALSE)
    dir.create("Statistic_data/FFT/Data", showWarnings = FALSE)
    dir.create("Statistic_data/FFT/Input_data", showWarnings = FALSE)
    
    dir.create("Statistic_data/ACF", showWarnings = FALSE)
    dir.create("Statistic_data/ACF/Data", showWarnings = FALSE)
    dir.create("Statistic_data/ACF/Prints", showWarnings = FALSE)
    dir.create("Statistic_data/ACF/Input_data", showWarnings = FALSE)
    
    
    dir.create("Statistic_data/CCR", showWarnings = FALSE)
    dir.create("Statistic_data/CCR/Data", showWarnings = FALSE)
    dir.create("Statistic_data/CCR/Prints", showWarnings = FALSE)
    
    
    
    source("Funciones/Functions_R_gape/Legend.R")
    
    library(fMultivar)
    library(plyr)
    library(sgat)
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
    library(RSEIS)
    library(plyr)
    library(tidyr)
    library(dtplyr)
    library(dplyr)
    library(data.table)
    
    #library(tripEstimation)
    #x11()
    quartz()
    # Number of the first device
    ActualDevice <- dev.cur()
    
    # Parámetros de estafística
    
    #hfreq1 <- 48
    #hfreq2 <- 15
    highdownlim <- -0.3
    highuplim <- 0.3
    lowdownlim <- 0
    lowuplim <- 1
    
    FFTy <- c(100, 6000000) # para la variación diara Pinna
    
    #FFTy <- c(1, 10000) 
    
    DoAstro = TRUE
    
   
    
    #stat
    lwdaxis2 <- 3.3 #width of the line of the axis
    mtextfont2 <- 2
    mtextcex2 <- 2.4
    cex_axis2 <- 2.4 # Size of the values in the axis, I use 1 (2.5 for jpeg)
    mai2 <- c(1,1.1,0.8,0.5) # Márgenes de la gráfica. Abajo, izquierda, arriba y derecha
    padj2 <- 0.2
    
    #graph
    lwdaxis <- 3.3 #width of the line of the axis
    mtextfont <- 2 # Kind of font for the axis titles (bolt, normal...)
    mtextcex <- 1.8 # Size of the title of the axis 1.8 (2.4)
    linemtextX <- c(3, 3.8) # Usually 3 and 3.8 (3.8, 4.5)
    linemtextY <- 3 # Usually 3 (3.9)
    cex_axis <- 1.4 # Size of the values in the axis, I use 1.4 (2.4 for jpeg)
    linevaluesX <- 0 # Position (up or down) of the line of the axis X
    linedatesX <- 1 # Position (up or down) of the second line of the axis X
    
    padj <- 0.5 # position of the values in axis X in reference to the line.
    
    cexlegend <- 0.7  # Text size of legend, usually I use 0.7 (1.9 for jpeg)
    
    mai <- c(1,1.1,0.8,1.1) # Márgenes de la gráfica. Abajo, izquierda, arriba y derecha
    
    
    lwd_data2 <- 2.5 # Width of the lines, usually I use 2.5 (3 for jpeg)
    lwd_data1 <- 3 # Width of the lines, usually I use 3 (3 for jpeg)
    lty_data1 <- 1
    
    ltysensors <- 1
    
    lwdlegend <- 3  # Width of the legend lines, usually I use 2 (3 for jpeg)
    ltylegend<- 1
    pnglwd <- 3 # Width of the line of High and Low freq png
    pngaxistitle <- 1.2 # Size of the axis title of High and Low freq png
    
    
    
    
    individualsname3 <- colnames(data1)
    ########################################################
    if (is.null(lightdata) & !is.null(treatment)) {
        whichcolumns <- seq(2, 1+treatment, by = 1)
        individualsname <- individualsname3[-c(1,whichcolumns)]
    } else if (!is.null(lightdata) & is.null(treatment)) {
        whichcolumns <- seq(2, 1+lightdata, by = 1)
        individualsname <- individualsname3[-c(1,whichcolumns)]
    } else if (is.null(lightdata) & is.null(treatment)) {
        individualsname <- individualsname3[-c(1)]
    } else {    
        whichcolumns <- seq(2, 1+lightdata+treatment, by = 1)
        individualsname <- individualsname3[-c(1,whichcolumns)]
    }
    #globalenv(individualsname)
    #individualsname <- rep(NA, length(individualsname2), replace = TRUE)
    #individualsname <- assign(individualsname, individualsname2)
#    if (lightdata != treatment) {
 #       individualsname <- individualsname2[-c(1,2,3)]
  #  } else if (lightdata == TRUE & treatment == TRUE) {
#        individualsname <- individualsname2[-c(1,2,3,4)]
 #   } else {    individualsname <- individualsname2[-c(1,2)]
  #  }
    ########################################################
    numberindividuals <- length(individualsname) #This is the value that the user will choose (17 Nioz, 10 PINNA)
    allindividuals <- seq(1, numberindividuals, by = 1)
    
    ## Create a value for each data, and turn it FALSE
    individuals <- rep(NA, length(allindividuals), replace = TRUE)
    for(i in 1:length(allindividuals)){
        allindividuals2 <- paste(individualsname[i], sep = "")
        individuals[i] <- assign(allindividuals2, value = FALSE)
        
    }
    
    
    if (!is.null(data2)){
        sensorsname2 <- colnames(data2)
        
        sensorsname <- sensorsname2[-c(1)]
        
        numbersensors <- length(sensorsname) #This is the value that the user will choose (9 Nioz, 10 PINNA)
        allsensors <- seq(1, numbersensors, by = 1)
        ## This  lines are created to, when click a button in the plot, change
        ## this parameters to FALSE and change the plot.
        
        ## Create an object for each data, and turn it FALSE
        sensors <- rep(NA, length(allsensors), replace = TRUE)
        for(i in 1:length(allsensors)){
            allsensors2 <- paste(sensorsname[i], sep = "")
            sensors[i] <- assign(allsensors2, value = FALSE)
        }
        
    }
    
    ## Select the colors
    if(blackwhite == FALSE){
        ## Select the colors
        indvcolors1 <- c("darkred", "deepskyblue", "darkseagreen", "darkgoldenrod1", "burlywood4", "aquamarine",
                         "darkorchid", "cyan", "lightblue", "darkslategray", "royalblue1", "maroon",
                         "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink", 
                         "lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                         "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                         "orange", "gray51", "indianred", "thistle3")
        indvcolors <- indvcolors1[allindividuals]
        
        lty_data11 <- rep(x = 1, length(individualsname))
        lty_data1 <- lty_data11[allindividuals]
        
        if (!is.null(data2)){
        senscolors1 <- c("lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                         "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                         "orange", "gray51", "indianred", "thistle3", "cyan", "deepskyblue", "darkred", "darkgoldenrod1", "burlywood4", "aquamarine",
                         "darkorchid", "darkseagreen", "lightblue", "darkslategray", "royalblue1", "maroon",
                         "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink")
        senscolors <- senscolors1[allsensors]
        }
    } else {
        indvcolors1 <- c("black", "black", "black", "black", "black", "black", "black", "black", 
                         "gray40", "black", "gray30", "black", "black", "black")
        indvcolors <- indvcolors1[allindividuals]
        
        lty_data11 <- c(1,1,1,1,1,1,1,1,1,1,2,1,1,1)
        lty_data1 <- lty_data11[allindividuals]
        
        if (!is.null(data2)){
        senscolors1 <- c("lightskyblue", "red", "darkturquoise", "forestgreen", "yellow3", "darkseagreen2",
                         "darkmagenta", "darkblue", "aquamarine4", "green", "blueviolet", "gold", "yellow",
                         "orange", "gray51", "indianred", "thistle3", "cyan", "deepskyblue", "darkred", "darkgoldenrod1", "burlywood4", "aquamarine",
                         "darkorchid", "darkseagreen", "lightblue", "darkslategray", "royalblue1", "maroon",
                         "lightgoldenrod", "darkolivegreen1", "chocolate1", "bisque4", "hotpink")
        senscolors <- senscolors1[allsensors]
        
        }
        
    }
    
    
    
    Light = FALSE; Food = FALSE; Moon = FALSE
    
    if(is.null(lightdata)) {
        ButtonLight = FALSE
    } else {
        ButtonLight = TRUE; Light = TRUE
        colorlight <- c("gray75", "gray45", "gray25")
    }
    
    if(is.null(treatment)) {
        ButtonFood = FALSE
    } else {
        ButtonFood = TRUE; Food = FALSE
        colortreatment <- c("magenta", "olivedrab1", "navajowhite4", "orange", "mediumblue")
    }
    
    if(is.null(coord)) {
        ButtonMOON = FALSE
    } else {ButtonMOON = TRUE; Moon = FALSE}
    
#    if(lightdata == FALSE) {
 #       ButtonLight = FALSE
  #  } else {ButtonLight = TRUE; Light = TRUE}
#    
 #   if(treatment == FALSE) {
  #      ButtonFood = FALSE
#    } else {ButtonFood = TRUE; Food = FALSE}
 #   
  #  if(coord == FALSE) {
#        ButtonMOON = FALSE
 #   } else {ButtonMOON = TRUE; MOON = FALSE}
    
    
    
    
    VariableButtons <- c(ButtonLight, ButtonFood, ButtonMOON)
    #PosButtons <- c("Light", "Food", "Moon")
    PosButtons <- c("Light", "Moon")
    
    PosButtons2 <- PosButtons[VariableButtons]
    

    
    ## buttons that are going to be plotted in allbuttons
    monthnames <- c("Ene", "Feb", "Mar","Abr", "May", "Jun", "Jul", "Ago", 
                    "Sep", "Oct", "Nov", "Dic")
    # Eliminated "D.Set" and Locator button. Is now useful right now.
    basicbuttons = c("Refresh", "Dev.Off", "Text", "JPG", "Next", "Prev", "GoTo", "SelecD", "NDay", "Stadistic")
    
    
    
    if (is.null(data2)) {
        
        if (length(individualsname) < 11){
            pinnabuttons <- individualsname
        } else {
            pinnabuttons <- "SelData.1"}
        
    } else {pinnabuttons <- "SelData.1"}
    
    if (!is.null(data2)){
        if(length(sensorsname) < 11){
            sensorbuttons = sensorsname
            
        } else {
            sensorbuttons = "SelData.2"
            
        }
    } else {sensorbuttons = character(0)}
    
    
    allbuttons = c(basicbuttons, PosButtons2, sensorbuttons, pinnabuttons)
    
    
    
    
    
    NDay = 10
    Rday <- seq(min(data1[,1]), max(data1[,1]), by = 1)
    Inicialday <- 1
    minJD <- Rday[Inicialday]
    xday <- minJD + NDay
    
    
    if (exp.days == FALSE){
        maxNday <- round_any(max(data1[,1]), 1, floor)
        minNday <- round_any(min(data1[,1]), 1, floor)
    } else {
        maxNday <- round_any(max(data1[,1]), 1, floor) - Rday[1]+1
        minNday <- round_any(min(data1[,1]), 1, floor) - Rday[1]+1
    }
    
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    
    
    sebasplot <- function(data1) {
        
        if (!is.null(data2)){
            whatdaysens <- which(data2[,1]>=minJD & data2[,1]<=xday)
        }
        
        whatdaypinn <- which(data1[,1]>=minJD & data1[,1]<=xday)
        
        
        ## Para que la función pretty coja el año que toca, sino da dias equivocados.
        #datayear <- strsplit(data1$Date[minJD], split = "-")
        #year <- as.numeric(datayear[[1]][1])
        year <- as.numeric(format(iday, "%Y"))
        
        ## Esto serviría para que, una vez con los botones le digo que X individuos son TRUE, se actualice
        ## allindividuals con cuales son TRUE y cuales son FALSE. Útil para legend
        for(i in 1:length(allindividuals)){
            individuals2 <-  paste(individualsname[i], sep = "")
            individuals1 <- parse(text = individuals2)
            individuals[i] <- eval(individuals1)
        }
        
        if (!is.null(data2)){
            for(i in 1:length(allsensors)){
                sensor2 <-  paste(sensorsname[i], sep = "")
                sensor1 <- parse(text = sensor2)
                sensors[i] <- eval(sensor1)
            }
        }
        
        ##
        ##
        ## Axis Y
        TRUEindv2 <- c(individuals)
        TRUEindv1 <- c(individualsname)
        TRUEindv <- TRUEindv1[TRUEindv2]
        
        if (length(TRUEindv) > 0){
            dataaxisindv <- data.frame(matrix(ncol = length(TRUEindv), nrow = 
                                                  length(data1[,1][whatdaypinn])))
            colnames(dataaxisindv) <- TRUEindv
            for(i in 1:length(TRUEindv)){
                dataaxis2 <-  paste("data1$", TRUEindv[i], sep = "")
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
        if (!is.null(data2)){
            
            TRUEsens2 <- c(sensors)
            TRUEsens1 <- c(sensorsname)
            TRUEsens <- TRUEsens1[TRUEsens2]
            
            
            if (length(TRUEsens) > 0){
                dataaxissens <- data.frame(matrix(ncol = length(TRUEsens), nrow = 
                                                      length(data2[,1][whatdaysens])))
                colnames(dataaxissens) <- TRUEsens
                for(i in 1:length(TRUEsens)){
                    dataaxis2 <-  paste("data2$", TRUEsens[i], sep = "")
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
            
        }
        
        if (!is.null(fixLYaxis)) {
            minLeftaxis2 <- fixLYaxis[1]
            maxLeftaxis2 <- fixLYaxis[2]
        } else {}
        
        if (!is.null(fixRYaxis)) {
            minRightaxis2 <- fixRYaxis[1]
            maxRightaxis2 <- fixRYaxis[2]
        } else {}
        ## Axis Y
        ##
        ##
        
        
        if (!is.null(data2)){
            plotdaysens <- data2[,1][whatdaysens]
            
        }
        plotdaypinn <- data1[,1][whatdaypinn]
        lineindcolor <- allindividuals[individuals]
        
        
        Rplotday <- range(plotdaypinn, na.rm = TRUE)
        
        addplotday = c(Rplotday[1]-(diff(Rplotday)*0.03), Rplotday[2])
        #addplotday = c(Rplotday[1], Rplotday[2])
        #addplotday = c(Rplotday[1], Rplotday[2])
        Rsensor <- range(data1[4], data1[5], na.rm = TRUE)
        
        #Rplotday <- range(plotdaysens, na.rm = TRUE)
        #addplotday = c(Rplotday[1]-0.06*diff(Rplotday), Rplotday[2])
        #Rsensor <- range(data2[4], data2[5])
        
        
        
        
        ## In that last lines we are seleccting the julian days (axis X) that are going to
        ## be represented for the sensors and the pinna data.
        
        
        minjd <- min(addplotday) ## Minimum julian day value for the plot
        maxjd <- max(addplotday) ## Maximum julian day value for the plot
        

        par(mai = c(mai[1],mai[2],mai[3],mai[4]))
        plot(addplotday, Rsensor, type = "n", axes = FALSE,
             xlab = "", ylab = "", ylim = c(minLeftaxis2, maxLeftaxis2), xlim = c(minjd, maxjd))
        
        #####################################
        #####################################
        #####################################
        #####################################
        
        if (!is.null(lightdata) & Light){
            #Background light
            # Este if es porque si los primeros datos son de noche, hace falta que el
            ## primer dato sea diferente para que luego ese dato se seleccione y haya
            ### un primer dato desde el que dibujar el rectángulo.
           
            for (i in 2:(1+lightdata)){
                
                datalight <- data1[whatdaypinn[1]:tail(whatdaypinn, 1),]

                if(datalight[,i][2] == 1){
                    datalight[,i][1] <- 2
                }
                
                int <- datalight[c(diff(as.numeric(datalight[,i])) == 1 | diff(as.numeric(datalight[,i])) == -1, TRUE), ]
                int2 <- select(int, 1)
                whatcolor <- colorlight[i-1]
                n = 0
                for (i in 1:(length(int2[,1])/2)){
                    
                    if (i == 1){
                        rect(xleft = int2[,1][1], ybottom = minLeftaxis2, xright = int2[,1][2], ytop = maxLeftaxis2, lty = 0, col = whatcolor)
                    } else {
                        n = n+1
                        rect(xleft = int2[,1][i+n], ybottom = minLeftaxis2, xright = int2[,1][i+n+1], ytop = maxLeftaxis2, lty = 0, col = whatcolor)
                    }
                }
                
            }
            
        }
        #####################################
        #####################################
        #####################################
        #####################################
        
        #Background Food
        if (!is.null(treatment) & Food & !is.null(lightdata)) {
            
            for (i in 1+lightdata:1+lightdata+treatment){
                foodline <- subset.data.frame(x = data1, data1[,i] == 1)
                whatcolor <- colortreatment[i-lightdata]
                for (i in 1:length(foodline[,1])){
                    abline(v = foodline[,1][i], col = whatcolor, lty = 2)
                }
            }
        } else {
            if (!is.null(treatment) & Food & is.null(lightdata)) {
                
                for (i in 2:1+treatment){
                    foodline <- subset.data.frame(x = data1, data1[,i] == 1)
                    whatcolor <- colortreatment[i-1]
                    for (i in 1:length(foodline[,1])){
                        abline(v = foodline[,1][i], col = whatcolor, lty = 2)
                    }
                }
            }
        }
        
        
        #####################################
        #####################################
        #####################################
        #####################################
        if(box == TRUE){
            box()
        }
        
        if(any(individuals)){
            if (maxLeftaxis2 > 2){
                axis(side = 2, col = axiscolor(Y1), las = 1, 
                     at = seq(minLeftaxis2, maxLeftaxis2, by = round_any((maxLeftaxis2-minLeftaxis2)/10, 1, f = floor)), 
                     cex.axis = cex_axis, lwd = lwdaxis)
                
            } else {
                axis(side = 2, col = axiscolor(Y1), las = 1, 
                     at = seq(minLeftaxis2, maxLeftaxis2, by = round_any((maxLeftaxis2-minLeftaxis2)/10, 0.1, f = floor)), 
                     cex.axis = cex_axis, lwd = lwdaxis)
                
            }
            
        }
        
        #####################################
        #####################################
        #####################################
        #####################################
        
        ####
        #####
        ###### Axis X.
        monthnames <- c("Ene", "Feb", "Mar","Abr", "May", "Jun", "Jul", "Ago", 
                        "Sep", "Oct", "Nov", "Dic")
        boxcoord <- par("usr")
        if (!is.null(data2)){
            posdaysaxisX <- 1
            
        } else {
            posdaysaxisX <- 1
        }
        
        
        if(NDay == 20){
            selecday20 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 2)
            getday20 <- getmoday(selecday20-0.75, year) #Añado el -0,75 porque el getmoday cuenta los días
            # a partir de las 6 de la mañana. Entonces si le resto 0,75 contará de 00:00 a 23:59 como el mismo día.
            
            mymonths20 <- monthnames[getday20$mo]
            mydays20 <- getday20$dom
            mydate20 <- paste(sep=" ", mymonths20, mydays20)
            
            if (exp.days == FALSE){
                axis(1, at=selecday20, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday20, labels = round_any(selecday20 - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            
            if (dates == TRUE){
                axis(1, at=selecday20, labels= mydate20, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}

        } else if(NDay == 10) {
            selecday10 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 2)
            getday10 <- getmoday(selecday10-0.75, year) 
            
            mymonths10 <- monthnames[getday10$mo]
            mydays10 <- getday10$dom
            mydate10 <- paste(sep=" ", mymonths10, mydays10)
            
            if (exp.days == FALSE){
                axis(1, at=selecday10, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday10, labels = round_any(selecday10 - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            if (dates == TRUE){
            axis(1, at=selecday10, labels= mydate10, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}
        
        } else if(NDay == 12) {
            selecday12 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 2)
            getday12 <- getmoday(selecday12-0.75, year) 
            
            mymonths12 <- monthnames[getday12$mo]
            mydays12 <- getday12$dom
            mydate12 <- paste(sep=" ", mymonths12, mydays12)
            
            if (exp.days == FALSE){
                axis(1, at=selecday12, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday12, labels = round_any(selecday12 - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            if (dates == TRUE){
            axis(1, at=selecday12, labels=mydate12, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}
            
        } else if(NDay == 30) {
            selecday30 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 3)
            getday30 <- getmoday(selecday30-0.75, year) 
            
            mymonths30 <- monthnames[getday30$mo]
            mydays30 <- getday30$dom
            mydate30 <- paste(sep=" ", mymonths30, mydays30)
            
            #axis(3, at=selecday30, pos=boxcoord[3])
            if (exp.days == FALSE){
                axis(1, at=selecday30, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday30, labels = round_any(selecday30 - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            if (dates == TRUE){
            axis(1, at=selecday30, labels=mydate30, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}
            
        } else if(NDay == 71) {
            selecday71 <- seq(round_any(minJD, 1, f =floor), round_any(xday, 1, f = ceiling), by = 5)
            getday71 <- getmoday(selecday71-0.75, year) 
            
            mymonths71 <- monthnames[getday71$mo]
            mydays71 <- getday71$dom
            mydate71 <- paste(sep=" ", mymonths71, mydays71)
            
            #axis(3, at=selecday71, pos=boxcoord[3])
            if (exp.days == FALSE){
                axis(1, at=selecday71, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday71, round_any(labels = selecday71 - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            if (dates == TRUE){
            axis(1, at=selecday71, labels=mydate71, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}
            
        } else {
            selecday <- pretty(plotdaypinn, n = 6)
            getday <- getmoday(round_any(selecday, 0.1, ceiling)-0.75, year)
            ## He puesto un round_any porque el getmoday, si el dia juliano 56
            ## es 25 de febrero, el dia juliano 56.5 lo toma ya como 26 de febrero,
            ## cuando realmente sigue siendo 25. El round_any me redondea siempre 
            ## al número entero inferior, por lo que se soluciona el problema.
            mymonths <- monthnames[getday$mo]
            mydays <- getday$dom
            mydate <- paste(sep=" ", mymonths, mydays)
            
            if (exp.days == FALSE){
                axis(1, at=selecday, cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            } else {
                axis(1, at=selecday, labels = round_any(selecday - Rday[1]+1, 0.1), cex.axis = cex_axis, line = linevaluesX, lwd = lwdaxis, padj = padj)
            }
            if (dates == TRUE){
            axis(1, at=selecday, labels=mydate, lwd = 0, cex.axis = cex_axis, line = linedatesX)
            } else{}
        } 
        
        if (exp.days == TRUE){
            mtext("Experimental days", side = 1, line = linemtextX[1], font = mtextfont, cex = mtextcex)
        } else {
            mtext("Julian days", side = 1, line = linemtextX[1], font = mtextfont, cex = mtextcex)
        }
        #mtext("Date", side = 1, line = linemtextX[2], font = mtextfont, cex = mtextcex)
        
        
        # Text axis Y
        
        if(mtexty == TRUE){
            if(length(individualsname[individuals]) > 0){
                mtext(paste(individualsname[individuals], "(mm)", collapse = ", "), side = 2, line = linemtextY, font = mtextfont, cex = mtextcex)
            }
            if (!is.null(data2)){
                
                legtextsens <- paste(sensorsname[sensors])
                if(length(sensorsname[sensors]) > 0){
                    mtext(paste(sensorsname[sensors], collapse = ", "), side = 4, line = linemtextY, font = mtextfont, cex = mtextcex)
                }
            }
            
        }
        
        #####################################
        #####################################
        #####################################
        #####################################
        

        if (Moon) {
            
            #  Dspain = ISOdatetime(2010,2,5, 0, 0, 0, tz = "GMT" )
            iday = ISOdatetime(2014,02,07, 0, 0, 0, tz = "GMT" )
            ##  iday- Dspain
            fday = ISOdatetime(2016,09,23, 0, 0, 0, tz = "GMT" )
            
            
            u1 = par("usr")
            
            #ASTRO = getSunMoon(iday,fday, PINNAlat=(38+10/60),  PINNAlon= -(0+28/60))
            #ASTRO = getSunMoon(iday,fday, PINNAlat=71.0626,  PINNAlon= 24.12)
            ASTRO = getSunMoon(iday, fday, PINNAlat= coord[1], PINNAlon = coord[2])
            
            
            
            if(DoAstro)
            {  ###   astronomical information
                
                SMperc = 0
                SMperc2 = 0.1
                
                ###################### I HAVE MODIFIED tselastro 
                #####################(the original is the first line)        
                
                #days = as.numeric( round((fday+365*(yr-2010))-iday) )
                days = as.numeric( round((fday+365*(2016-2016))-iday) )
                days2=days[1]
                
                #tselastro  =  ASTRO$jatm>=i1 & ASTRO$jatm<=i2
                
                # tselastro  =  ASTRO$jatm<=i1+days2-1
                tselastro  =  ASTRO$jatm<=minJD+days2-1
                astrotim = ASTRO$jatm[tselastro] 
                SUNelv =ASTRO$Sun$theta[tselastro]
                
                ##  SUNelv[SUNelv<0] = NA
                
                Fazer = ASTRO$Phase[tselastro]
                
                
                RangeSUN = range(SUNelv, na.rm = FALSE)
                
                Sunr = RESCALE(SUNelv, u1[3]+SMperc*abs(u1[4]-u1[3]) , u1[4],  RangeSUN[1], RangeSUN[2] )
                
                Sunr[SUNelv<0] = NA
                
                LUNAelv = ASTRO$Moon$theta[tselastro]
                
                ##  LUNAelv[LUNAelv<0] = NA
                RangeMOON = range(LUNAelv, na.rm = FALSE)
                print(u1)
                
                Moonr = RESCALE(LUNAelv, u1[3]+SMperc*abs(u1[4]-u1[3]) , u1[4]-SMperc2*abs(u1[4]-u1[3]) ,  RangeMOON[1], RangeMOON[2] )
                
                Moonr[LUNAelv<0] = NA
                
                ##  plot(astrotim, Sunr, type='n')
                u1 = par("usr")
                astro.names = c("Sun", "Moon")
                astro.colors = c("gold", rgb(.75,.75, 1)  )
                astro.lty  = c(1,2 )
                
                
                lines(astrotim , Sunr, col=astro.colors[1]  , lwd=3, lty=astro.lty[1]) 
                
                lines(astrotim , Moonr, col=astro.colors[2]  , lwd=3, lty=astro.lty[2]) 
                
                
                
                
                peakmoons = which( peaks(Moonr, span = 3, do.pad = TRUE) )
                
                ##   abline(v=astrotim[peakmoons])
                u = par("usr")
                
                up = par("usr")
                ui = par("pin")
                
                ratx = (up[2]-up[1])/ui[1]
                raty=  (up[4]-up[3])/ui[2]
                
                
                ##  uinch =   (u[4]-u[3])/pin[2]
                
                
                ##  myasp = (u1[4]-u1[3])/(u1[2]-u1[1])
                
                rad1 =   .2 * ratx
                rad2 =   .2  * raty
                
                ##  ik = peakmoons
                ##  rect(astrotim[ik]-rad1 ,Moonr[ik]-rad2, astrotim[ik]+rad1 ,Moonr[ik]+rad2)
                
                
                for(i in 1:length(peakmoons))
                {
                    ik = peakmoons[i]
                    moonglyph(astrotim[ik] ,Moonr[ik],rad1, rad2, Fazer[ik], xpd=FALSE  )
                }
                
            }
            
            
        }
        
        
        ##
        ##
        ## Lines individuals
        
        indvlines3 <- individualsname[individuals]
        if (length(indvlines3 > 0)) {
            
            for(i in 1:length(indvlines3)){
                #indvlines4 <- parse(text = indvlines5[i])
                #indvlines3 <- eval(indvlines4)
                indvlines2 <- paste("data1$", indvlines3[i], sep = "")
                indvlines1 <- parse(text = indvlines2)
                lines(plotdaypinn, eval(indvlines1)[whatdaypinn], type = "l", 
                      col = indvcolors[lineindcolor][i], lwd = lwd_data1, lty = lty_data1[lineindcolor][i])
            }
        }
        
        
        
        ## Lines individuals
        ##
        ## 
        
        
        if (!is.null(data2)){
            
            par(new = TRUE) ## Add a new Y axis in the right
            
                plot(addplotday, Rsensor, type = "n", axes = FALSE,
                     xlab = "", ylab = "", ylim = c(minRightaxis2, maxRightaxis2), xlim = c(minjd, maxjd))
                
            
            if(any(sensors)){
                axis(side = 4, col = axiscolor(Y1), las = 1, 
                     at = seq(minRightaxis2, maxRightaxis2, by = ((maxRightaxis2-minRightaxis2))/10), 
                     cex.axis = cex_axis, lwd = lwdaxis)
            }
            
            #######################################################
            #######################################################
            #######################################################
            ##
            ##
            ## Lines sensors
            
            linesenscolor <- allsensors[sensors]
                
            
            senslines3 <- sensorsname[sensors]
            if (length(senslines3 > 0)) {
                
                for(i in 1:length(senslines3)){
                    #senslines4 <- parse(text = senslines5[1])
                    #senslines3 <- eval(senslines4)
                    senslines2 <- paste("data2$", senslines3[i], sep = "")
                    senslines1 <- parse(text = senslines2)
                    lines(plotdaysens, eval(senslines1)[whatdaysens], type = "l", lty = 2,  
                          col = senscolors[linesenscolor][i], lwd = lwd_data2)
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
        
        ## Sensor legend
        
        leg <- FALSE
        if (any(individuals)) {
            
            indvlegend <- individualsname[which(individuals)]
            legindv <- legend("topleft", legend = indvlegend, lty = lty_data1[which(individuals)],
                              col = indvcolors[lineindcolor], lwd = c(rep(lwdlegend, numberindividuals, replace = TRUE)), 
                              pt.cex = 0.85, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
            leg <- TRUE
            
        } else {}
        
        if (leg){
            topleg <- legindv$rect$top
            botleg <- legindv$rect$top-legindv$rect$h
            leftleg <- legindv$rect$left
            horzleg <- legindv$rect$left + legindv$rect$w
        }
        
        ##############################################
        leg2 <- FALSE
        if (!is.null(data2)){
            
            linesenscolor <- allsensors[sensors]
            
            
            if (any(sensors)) {
                sensorleg <- sensorsname[which(sensors)]
                
                if(leg){
                    if (legend == "horizontal"){
                        legsen <- legend(x = horzleg, y = topleg, legend = sensorleg, lty = rep(ltylegend, numbersensors, replace = TRUE),
                                         col = senscolors[linesenscolor], lwd = c(rep(lwdlegend, numbersensors, replace = TRUE)), 
                                         pt.cex = 0.78, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
                        
                    } else {
                        legsen <- legend(x = leftleg, y = botleg, legend = sensorleg, lty = rep(ltylegend, numbersensors, replace = TRUE),
                                         col = senscolors[linesenscolor], lwd = c(rep(lwdlegend, numbersensors, replace = TRUE)), 
                                         pt.cex = 0.78, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
                        
                    }
                    
                } else{
                    legsen <- legend("topleft", legend = sensorleg, lty = rep(ltylegend, numbersensors, replace = TRUE),
                                     col = senscolors[linesenscolor], lwd = c(rep(lwd_data1, numbersensors, replace = TRUE)), 
                                     pt.cex = 0.85, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
                }
                leg2 = TRUE
                
                
            } else {}
        }
        if(leg2){
            topleg <- legsen$rect$top
            botleg <- legsen$rect$top-legsen$rect$h
            leftleg <- legsen$rect$left
            horzleg <- legsen$rect$left + legsen$rect$w
            
        }
        
        
        #################################
        
        ## Add more legend
        leg3 <- FALSE
        
        if (!is.null(lightdata) & Light) {
            Lightleg <- c("Day", "Night")
            
            if(any(leg, leg2)){
                if (legend == "horizontal"){
                    legLight <- legend(x = horzleg, y = topleg, legend = Lightleg, fill = c("white","gray45"), pt.cex = 0.78, cex = cexlegend, text.font = 2,
                                       border = "black", box.cex = c(2,1), bg = "white", bty = bty)
                    
                } else {
                    legLight <- legend(x = leftleg, y = botleg, legend = Lightleg, fill = c("white","gray45"), pt.cex = 0.78, cex = cexlegend, text.font = 2,
                                       border = "black", box.cex = c(2,1), bg = "white", bty = bty)
                    
                }
                
            } else{
                legLight <- legend("topleft", legend = Lightleg, fill = c("white","gray45"), pt.cex = 0.78, cex = cexlegend, text.font = 2,
                                   border = "black", box.cex = c(2,1), bg = "white", bty = bty)
            }
            leg3 = TRUE
            
        } else {}
        
        if(leg3){
            topleg <- legLight$rect$top
            botleg <- legLight$rect$top-legLight$rect$h
            leftleg <- legLight$rect$left
            horzleg <- legLight$rect$left + legLight$rect$w
            
        }
        
        
        ## Add more legend
        leg4 <- FALSE
        
        if (!is.null(treatment) & Food) {
            Foodleg <- colnames(treatment)[2]
            
            if(any(leg, leg2, leg3)){
                if (legend == "horizontal"){
                    legFood <- legend(x = horzleg, y = topleg, legend = Foodleg, col = "darkgreen", 
                                      lty = 2, pt.cex = 0.78, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
                    
                } else{
                    legFood <- legend(x = leftleg, y = botleg, legend = Foodleg, col = "darkgreen", 
                                      lty = 2, pt.cex = 0.78, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
                    
                }
                
            } else{
                legFood <- legend("topleft", legend = Foodleg, col = "darkgreen", 
                                  lty = 2, pt.cex = 0.78, cex = cexlegend, text.font = 2, bg = "white", bty = bty)
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
        
        
    }
    
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    
    
    
    buttscolours <- length(allbuttons)
    for(i in 1:buttscolours){
        buttscolours[i] <- linecolor(allbuttons[i])
    }
    
    sensorbuttsimbol <- c(rep(16, length(sensorbuttons), replace = TRUE))
    basicbuttsimbol <- c(rep(4, length(basicbuttons), replace = TRUE))
    pinnabuttsimbol <- c(rep(4, length(pinnabuttons), replace = TRUE))
    allsimbol <- c(basicbuttsimbol, sensorbuttsimbol, pinnabuttsimbol)                     
    
    dosebas = sebasplot(data1)
    buttonsize = 0.7
    thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
    NLABS = length(allbuttons)
    NOLAB = NLABS +1000
    iloc = locator(1, type='p')
    zloc = iloc
    Nclick = length(iloc$x)
    if(is.null(zloc$x)) { return(NULL) }
    K =  whichbutt(zloc , thebutts)
    sloc = zloc
    
    esc = FALSE
    
    
    
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    ###############################################################################################################
    
    
    
    while(TRUE) {  ######  main while loop morebuttons y labs
        
        
        if(K[Nclick] == match("Dev.Off", allbuttons, nomatch = NOLAB))
        {
            
            dev.off(ActualDevice)
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
            
        }
        
        if(K[Nclick] == match("Refresh", allbuttons, nomatch = NOLAB))
        {
            
            print("Refresh")
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
            
        }
        
        if(K[Nclick] == match("Moon", allbuttons, nomatch = NOLAB))
        {
            
            Moon = !Moon
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        if(K[Nclick] == match("Food", allbuttons, nomatch = NOLAB))
        {
            
            Food = !Food
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        if(K[Nclick] == match("Light", allbuttons, nomatch = NOLAB))
        {
            
            Light = !Light
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        
        if(K[Nclick] == match("Text", allbuttons, nomatch = NOLAB))
        {
            locators <- function(){
                cat("How many locators do you want to do")
                whatstat = readline(prompt = "Choose a number: ")
            } 
            
            repeat{
                loc <- locators()
                nlocators <- as.numeric(loc)
                
                if(anyNA(nlocators)) {
                    cat("###########################################\n")
                    print("Error0, try again")
                } else if (length(nlocators) == 0) {
                    print("Error1, try again")
                } else if(length(nlocators) == 1 & nlocators >= 1){
                    break
                } else{
                    print("There is no such statistic")
                    print("Try again or choose -esc-")
                    
                }
            }
            nlocators <- rep(1, nlocators)
            for(i in 1:length(nlocators)){
                print("Select value to locate")
                first <- locator(nlocators[i])
                print("Select where do you want to plot the text")
                second <- locator(nlocators[i])
                if (exp.days == FALSE){
                    text(x = second$x, y = second$y, labels = paste("x =", round_any(first$x, accuracy = 0.01),
                                                                    "y =", round_any(first$y, accuracy = 0.01)))
                    print(paste("x =", round_any(first$x, accuracy = 0.01)))
                    print(paste("y =", round_any(first$y, accuracy = 0.01)))
                } else {
                    text(x = second$x, y = second$y, labels = paste("x =", round_any(first$x-Rday[1]+1, accuracy = 0.01),
                                                                    "y =", round_any(first$y, accuracy = 0.01)))
                    print(paste("x =", round_any(first$x-Rday[1]+1, accuracy = 0.01)))
                    print(paste("y =", round_any(first$y, accuracy = 0.01)))
                }
                
            }
            
            
            
            #dosebas = sebasplot(data1)
            #thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            #zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        if(K[Nclick] == match("NDay", allbuttons, nomatch = NOLAB))
        {
            
            maxNday2 <- round_any(max(data1[,1])-min(data1[,1]), 1, ceiling)
            
            cat("###########################################\n")
            cat("Change the Number of days. Max:", maxNday2)
            newNday = readline(prompt = "Type in the Number of days: ")
            
            NDay = as.numeric(newNday)
            
            if(NDay > maxNday2) {
                NDay = maxNday2
                cat("No so many days, NDay = max NDay:", maxNday2)
            } else{
                NDay = as.numeric(newNday)
            }
            
            
            #Rday <- seq(min(data2[,1]), max(data2[,1]), by = 1)
            Rday <- seq(min(data1[,1]), max(data1[,1]), by = 1)
            
            
            
            Inicialday <- 1
            minJD <- minJD
            xday <- minJD + NDay
            
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        if(K[Nclick] == match("Next", allbuttons, nomatch = NOLAB))
        {
            
            if(Inicialday + NDay > length(Rday))
            {
                Inicialday = Inicialday
                print("End")
                
            } else {
                minJD <- minJD + NDay
            }
            
            
            
            minJD <- minJD
            xday <- minJD + NDay
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            
            ###  do what ever button 2 is supposed to do
        }
        
        if(K[Nclick] == match("Prev", allbuttons, nomatch = NOLAB))
        {
            
            if(Inicialday<1)
            {
                Inicialday = 1
                print("Start")
                
            } else {
                minJD <- minJD - NDay
                
            }
            
            minJD <- minJD
            xday <- minJD + NDay
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        
        if(K[Nclick] == match("JPG", allbuttons, nomatch = NOLAB))
        {
            
            print(paste(sep=' ' ,"Start JPG  PLOT"))
            jdev = dev.cur()
            plfname = local.file("GapeR","jpg")
            
            
            jpeg(file=plfname ,
                 width = 14, height = 9.8, units = "in",
                 pointsize = 12,    bg = "transparent", res=500 )
            ## width 56 for print big plots, 14 normal
            ## height 9.8
            
            print(paste(sep=' ' ,"Doing jpg", plfname))
            dosebas = sebasplot(data1)
            
            print(paste(sep=' ' ,"Done creating jpg  file: ", plfname))
            dev.off()
            dev.set(jdev)
            zloc = list(x=NULL, y=NULL)
            
        }
        
        if(K[Nclick] == match("SelecD", allbuttons, nomatch = NOLAB))
        {
            print("Select first")
            first <- locator(1)
            print("Select second")
            second <- locator(1)
            
            firstday <- round_any(first$x, 1, f = floor)
            secondday <- round_any(second$x, 1, f = ceiling)
            
            Rday2 <- round_any(seq(min(data1[,1]), max(data1[,1]), by = 1), 1, floor)
            
            Inicialday <- which(Rday2 == firstday)
            NDay <- secondday - firstday
            minJD <- firstday
            xday <- minJD + NDay
            
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
            
        }
        
        if(K[Nclick] == match("Sensors", allbuttons, nomatch = NOLAB))
        {
            
            for(i in 1:length(sensorsname)){
                Sensorselec <- paste(sensorsname[i], sep = "")
                assign(Sensorselec,value = FALSE)
            }
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
            
        }
        
        
        if(K[Nclick] == match("GoTo", allbuttons, nomatch = NOLAB))
        {
            
            
            cat("###########################################\n")
            cat("To what day do you want to go?:\n")
            cat("First day", minNday, "Last day", maxNday)
            newGoTo = readline(prompt = "Type the day: ")
            
            if (exp.days == FALSE){
                GoTo = as.numeric(newGoTo)
            } else {
                GoTo = as.numeric(newGoTo) + Rday[1]-1
                
            }
            
            
            
            theday <- length(which(Rday == GoTo))
            Inicialday <- which(Rday == GoTo)
            if (theday>0){
                minJD <- GoTo
                xday <- minJD + NDay
            } else{
                cat("No such day")
                
            }
            
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        
        if (is.null(data2)){
            
            if(length(individualsname) < 11){
                
                for(i in 1:length(individualsname)){
                    
                    #whatclick <- match(eval(individualsname[i]), allbuttons, nomatch = NOLAB)
                    if(K[Nclick] == match(individualsname[i], allbuttons, nomatch = NOLAB))
                    {
                        print("working sensors")
                        print("push refresh after select sensors")
                        assign(individualsname[i], value = !eval(parse(text = individualsname[i])))
                    }
                }
                
            } else {
                
                if(K[Nclick] == match("SelData.1", allbuttons, nomatch = NOLAB))
                {
                    
                    
                    ### This loops allows to avoid the error when someone write wrong the data
                    validpinna <- function(){
                        cat("###########################################\n")
                        cat("Choose what data. Put the numbers between commas without spaces. Data available:\n")
                        print(paste(allindividuals, individualsname, sep = "-"))
                        cat("Or push ENTER to deselected all the data.\n")
                        whatpinnas = readline(prompt = "Individuals:")
                    } 
                    repeat{
                        Pinna <- validpinna()
                        
                        if(Pinna == ""){
                            Pinnaselec <- vector("numeric")
                            break
                        } else {
                            Pinnaselec = as.numeric(unlist(strsplit(split=",", (Pinna))))
                        }
                        
                        if(anyNA(Pinnaselec)) {
                            cat("###########################################\n")
                            print("Error0, try again")
                        } else if (length(Pinnaselec) == 0){
                            print("Error1, try again") 
                        } else if (any(duplicated(Pinnaselec))) {
                            print("Error2, try again") 
                        } else if (Pinnaselec >= min(allindividuals) & Pinnaselec <= max(allindividuals)
                                   & length(Pinnaselec) > 0){break
                        } else{
                            print("That doesn't seems like and individual")
                        }
                    }
                    
                    
                    if (length(Pinnaselec) > 0){ # Al pedirle que sea numeric, si el usuario pone
                        # "", es decir, pulsa ENTER, el codigo le dirige al final.
                        indtrue <- allindividuals[Pinnaselec]
                        indfalse <- allindividuals[-Pinnaselec]
                        
                        for(i in 1:length(indtrue)){
                            Pinnaselec2 <- paste(individualsname[indtrue][i], sep = "")
                            assign(Pinnaselec2,value = TRUE)
                        }
                        
                        if (length(indfalse) >=1 ){
                            for(i in 1:length(indfalse)){
                                Pinnaselec2 <- paste(individualsname[indfalse][i], sep = "")
                                assign(Pinnaselec2,value = FALSE)
                            }  
                        }
                        
                    } else {
                        for(i in 1:length(allindividuals)){
                            Pinnaselec2 <- paste(individualsname[i], sep = "")
                            assign(Pinnaselec2,value = FALSE)
                        }
                    }
                    
                    
                    dosebas = sebasplot(data1)
                    thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
                    zloc = list(x=NULL, y=NULL)
                    ###  do what ever button 2 is supposed to do
                }
            }
        }
        
        if (!is.null(data2)){
            
            if(K[Nclick] == match("SelData.1", allbuttons, nomatch = NOLAB))
            {
                ### This loops allows to avoid the error when someone write wrong the data
                validpinna <- function(){
                    cat("###########################################\n")
                    cat("Choose what data. Put the numbers between commas without spaces. Data available:\n")
                    print(paste(allindividuals, individualsname, sep = "-"))
                    #cat("Write -whole- to select all the individuals:\n")
                    cat("Or push ENTER to deselected all the data.\n")
                    whatpinnas = readline(prompt = "Individuals:")
                } 
                repeat{
                    Pinna <- validpinna()
                    
                    if(Pinna == ""){
                        Pinnaselec <- vector("numeric")
                        break
                    } else {
                        Pinnaselec = as.numeric(unlist(strsplit(split=",", (Pinna))))
                    }
                    
                    if(anyNA(Pinnaselec)) {
                        cat("###########################################\n")
                        print("Error0, try again")
                    } else if (length(Pinnaselec) == 0){
                        print("Error1, try again") 
                    } else if (any(duplicated(Pinnaselec))) {
                        print("Error2, try again") 
                    } else if (Pinnaselec >= min(allindividuals) & Pinnaselec <= max(allindividuals)
                               & length(Pinnaselec) > 0){break
                    } else{
                        print("That doesn't seems like and individual")
                    }
                }
                
                
                if (length(Pinnaselec) > 0){ # Al pedirle que sea numeric, si el usuario pone
                    # "", es decir, pulsa ENTER, el codigo le dirige al final.
                    indtrue <- allindividuals[Pinnaselec]
                    indfalse <- allindividuals[-Pinnaselec]
                    
                    for(i in 1:length(indtrue)){
                        Pinnaselec2 <- paste(individualsname[indtrue][i], sep = "")
                        assign(Pinnaselec2,value = TRUE)
                    }
                    
                    if (length(indfalse) >=1 ){
                        for(i in 1:length(indfalse)){
                            Pinnaselec2 <- paste(individualsname[indfalse][i], sep = "")
                            assign(Pinnaselec2,value = FALSE)
                        }  
                    }
                    
                } else {
                    for(i in 1:length(allindividuals)){
                        Pinnaselec2 <- paste(individualsname[i], sep = "")
                        assign(Pinnaselec2,value = FALSE)
                    }
                }
                
                
                dosebas = sebasplot(data1)
                thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
                zloc = list(x=NULL, y=NULL)
                ###  do what ever button 2 is supposed to do
            }
            
            
            
            if (length(sensorsname) < 11){
                
                for(i in 1:length(sensorsname)){
                    
                    #whatclick <- match(eval(sensorsname[i]), allbuttons, nomatch = NOLAB)
                    if(K[Nclick] == match(sensorsname[i], allbuttons, nomatch = NOLAB))
                    {
                        print("working sensors")
                        print("push refresh after select sensors")
                        assign(sensorsname[i], value = !eval(parse(text = sensorsname[i])))
                        #sensorsname[i] = !sensorsname[1]
                        #dosebas = sebasplot(data1)
                        #thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
                        #zloc = list(x=NULL, y=NULL)
                        ###  do what ever button 2 is supposed to do
                    }
                }
            } else {
                
                if(K[Nclick] == match("SelData.2", allbuttons, nomatch = NOLAB))
                {
                    
                    
                    ### This loops allows to avoid the error when someone write wrong the data
                    validpinna <- function(){
                        cat("###########################################\n")
                        cat("Choose what data. Put the numbers between commas without spaces. Data available:\n")
                        #cat(paste(allsensors, "-",sensorsname, sep = ""), sep = ", ", "\n")
                        print(paste(allsensors,sensorsname, sep = "-"))
                        cat("Or push ENTER to deselected all the data:\n")
                        whatpinnas = readline(prompt = "Individuals:")
                    } 
                    repeat{
                        Pinna <- validpinna()
                        
                        if(Pinna == ""){
                            Pinnaselec <- vector("numeric")
                            break
                        } else {
                            Pinnaselec = as.numeric(unlist(strsplit(split=",", (Pinna))))
                        }
                        
                        if(anyNA(Pinnaselec)) {
                            cat("###########################################\n")
                            print("Error0, try again")
                        } else if (length(Pinnaselec) == 0){
                            print("Error1, try again") 
                        } else if (any(duplicated(Pinnaselec))) {
                            print("Error2, try again") 
                        } else if (Pinnaselec >= min(allsensors) & Pinnaselec <= max(allsensors)
                                   & length(Pinnaselec) > 0){break
                        } else{
                            print("That doesn't seems like and individual")
                        }
                    }
                    
                    
                    if (length(Pinnaselec) > 0){ # Al pedirle que sea numeric, si el usuario pone
                        # "", es decir, pulsa ENTER, el codigo le dirige al final.
                        indtrue <- allsensors[Pinnaselec]
                        indfalse <- allsensors[-Pinnaselec]
                        
                        for(i in 1:length(indtrue)){
                            Pinnaselec2 <- paste(sensorsname[indtrue][i], sep = "")
                            assign(Pinnaselec2,value = TRUE)
                        }
                        
                        if (length(indfalse) >=1 ){
                            for(i in 1:length(indfalse)){
                                Pinnaselec2 <- paste(sensorsname[indfalse][i], sep = "")
                                assign(Pinnaselec2,value = FALSE)
                            }  
                        }
                        
                    } else {
                        for(i in 1:length(allsensors)){
                            Pinnaselec2 <- paste(sensorsname[i], sep = "")
                            assign(Pinnaselec2,value = FALSE)
                        }
                    }
                    
                    
                    dosebas = sebasplot(data1)
                    thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
                    zloc = list(x=NULL, y=NULL)
                    ###  do what ever button 2 is supposed to do
                }
            }
            
        }
        
        
        
        if(K[Nclick] == match("Stadistic", allbuttons, nomatch = NOLAB))
        {
            cat("###########################################\n")
            cat("Choose stadistic\n")
            print("1. Low frequenci")
            print("2. High frequenci")
            print("3. FFT")
            print("4. Auto Correlation")
            print("5. Cross Correlation (RAW)")
            print("6. Cross Correlation (RAW & LowF)")
            print("7. Cross Correlation (RAW & LowF & HighF)")
            print("8. Cross Correlation (RAW & HighF)")
            print("9. Exit from Statistic menu")
            
            dir.create("Statistic_data", showWarnings = FALSE)
            
            ###
            ### This loops allows to avoid the error when someone write wrong the interval of days
            validstatistic <- function(){
                cat("Select an statistic")
                whatstat = readline(prompt = "What do you want to do: ")
            } 
            
            repeat{
                Stat <- validstatistic()
                Statselect <- as.numeric(Stat)
                
                if(anyNA(Statselect)) {
                    cat("###########################################\n")
                    print("Error0, try again")
                } else if (length(Statselect) == 0) {
                    print("Error1, try again")
                } else if(length(Statselect) == 1 & Statselect >= 1 & Statselect <= 9){
                    break
                } else if (Statselect == 9){
                    esc <- TRUE
                    break
                } else{
                    print("There is no such statistic")
                    print("Try again or choose -esc-")
                    
                }
            }
            
            
            if (Statselect == 1){
                cat("###########################################\n")
                cat("Low frequency selected\n")
                namestat <- "LowF_"
            } else if(Statselect == 2) {
                cat("###########################################\n")
                cat("High frequency selected\n")
                namestat <- paste("HighF_")
            } else if(Statselect == 3) {
                cat("###########################################\n")
                cat("Fast Fourier Transform selected\n")
                namestat <- "FFT_"
            } else if(Statselect == 4) {
                cat("###########################################\n")
                cat("Auto correlation function selected\n")
                namestat <- "ACF_"
            } else if(Statselect == 5) {
                cat("###########################################\n")
                cat("Cross Correlation (RAW) selected\n")
                namestat <- "CCR_"
            } else if(Statselect == 6) {
                cat("###########################################\n")
                cat("Cross Correlation (RAW & LowF) selected\n")
                namestat <- "CCR_"
            } else if(Statselect == 7) {
                cat("###########################################\n")
                cat("Cross Correlation (RAW, LowF & HighF) selected\n")
                namestat <- "CCR_"
            } else if(Statselect == 8) {
                cat("###########################################\n")
                cat("Cross Correlation (RAW & HighF) selected\n")
                namestat <- "CCR_"
            } else {"Error statselect"}
            
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            if (any(Statselect == 1, Statselect == 2)){
                
                source("Funciones/Functions_R_gape/IntervalEquals.R", local = TRUE)
                
                
                #if (Statselect == 1){ ylim1 = c(lowdownlim, lowuplim)
                #} else if (Statselect == 2) { ylim1 = c(highdownlim, highuplim)
                # } else {print("error")}
                
                if (yesornot == "y") {
                    
                   
                    source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE)
                    
                    
                    for(i in 1:length(Pinnaselec)){
                        
                        source("Funciones/Functions_R_gape/StatisticData.R", local = TRUE)
                        
                        
                        if (Statselect == 1){
                            namepinna <- paste(namestat, thenames[Pinnaselec][i], sep = "")
                            namefiles <- paste(namestat, thenames[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                            
                            DataHL <- Lowfreq(as.matrix(Pinnaselec7[,1]),as.matrix(Pinnaselec7[,2]),freq/(24*3600),1/freq,1/(hfreq1*60*60))
                        } else if (Statselect == 2) { 
                            namepinna <- paste(namestat, thenames[Pinnaselec][i], sep = "")
                            namefiles <- paste(namestat, thenames[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2],"_", hfreq1, "_", hfreq2, sep = "")
                            
                            DataHL <- Highfreq(as.matrix(Pinnaselec7[,1]),as.matrix(Pinnaselec7[,2]),freq/(24*3600),1/freq,1/(hfreq1*60*60),1/(hfreq2*60*60))
                        } else {print("error")}
                        
                        # Write.table
                        colnames(DataHL) <- c("Julianday", namepinna)
                        write.table(x = DataHL, file = local.file(paste("Statistic_data/Low&HighF/Data/",namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                        print(paste("DataHL saved:", namefiles))
                        # Creat the JPEG
                        
                        jpng(paste("Statistic_data/Low&HighF/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 
                        
                        ylim1 = c(round_any(min(DataHL[,2]), 1, floor), c(round_any(max(DataHL[,2]), 1, ceiling)))
                        xlim1 = c(round_any(min(DataHL[,1]), 1, floor), round_any(max(DataHL[,1]), 1, ceiling))
                        

                        par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                        plot(DataHL[,1], DataHL[,2], xlab = "", ylab = "", type = "l", lwd = pnglwd, 
                             cex.axis = cex_axis2, axes = FALSE, ylim = c(ylim1[1], ylim1[2]),
                             xlim = c(xlim1[1], xlim1[2]))
                        
                        mtext("Julian day", side = 1, line = 3, font = mtextfont2, cex = mtextcex2)
                        mtext(namepinna, side = 2, line = 4, font = mtextfont2, cex = mtextcex2)
                        box()
                        axis(side = 2, cex.axis = cex_axis2, at = seq(ylim1[1], ylim1[2], by = (ylim1[2]-ylim1[1])/5), lwd = lwdaxis)
                        
                        if (exp.days == TRUE) {
                            axis(side = 1, cex.axis = cex_axis2, at = seq(xlim1[1], xlim1[2], by = round_any((xlim1[2]-xlim1[1])/5, 1)), 
                                 lwd = lwdaxis, labels = seq(0, round_any(max(DataHL[,1])- min(DataHL[,1]), 10, ceiling), by = round_any((xlim1[2]-xlim1[1])/5, 1)), padj = padj2)
                        } else {
                            axis(side = 1, cex.axis = cex_axis2, at = seq(xlim1[1], xlim1[2], by = round_any((xlim1[2]-xlim1[1])/5, 1)), lwd = lwdaxis, padj = padj2)
                            
                        }

                        {
                            dev.off()
                        }
                        print(paste("img done:", namefiles))
                        
                        # Actice the first device, is inactive like x11()
                        # dev.set(ActualDevice)
                    }
                    print("############################################")
                    print("work done")
                    
                    
                    ##############################################################
                    ##############################################################
                    ##############################################################
                    ##############################################################
                    
                } else if (yesornot == "n") {
                    
                    
                    ###
                    ### This loops allows to avoid the error when someone write wrong the data
                    source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE)
                    
                    ###
                    ###
                    
                   for(i in 1:length(Pinnaselec)){
                    
                    source("Funciones/Functions_R_gape/StatisticData.R", local = TRUE)
                    
                       
                       if (Statselect == 1){
                           namepinna <- paste(namestat, individualsname[Pinnaselec][i], sep = "")
                           namefiles <- paste(namestat, individualsname[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                           DataHL <- Lowfreq(as.matrix(Pinnaselec7[,1]),as.matrix(Pinnaselec7[,2]),freq/(24*3600),1/freq,1/(2*24*60*60))
                           
                       } else if (Statselect == 2) { 
                           namepinna <- paste(namestat, individualsname[Pinnaselec][i], sep = "")
                           namefiles <- paste(namestat, individualsname[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2],"_", hfreq1, "_", hfreq2, sep = "")
                           DataHL <- Highfreq(as.matrix(Pinnaselec7[,1]),as.matrix(Pinnaselec7[,2]),freq/(24*3600),1/freq,1/(hfreq1*60*60),1/(hfreq2*60*60))
                           
                       } else {print("error")}
                       
                        # Write.table
                        colnames(DataHL) <- c("Julianday", namepinna)
                        write.table(x = DataHL, file = local.file(paste("Statistic_data/Low&HighF/Data/",namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                        print(paste("DataHL saved:", namefiles))
                        # Creat the JPEG
                        
                        
                        jpng(paste("Statistic_data/Low&HighF/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 
                        
                        ylim1 = c(round_any(min(DataHL[,2]), 1, floor), c(round_any(max(DataHL[,2]), 1, ceiling)))
                        xlim1 = c(round_any(min(DataHL[,1]), 1, floor), round_any(max(DataHL[,1]), 1, ceiling))
                        
                        par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                        plot(DataHL[,1], DataHL[,2], xlab = "", ylab = "", type = "l", lwd = pnglwd, 
                             cex.axis = cex_axis2, axes = FALSE, ylim = c(min(DataHL[,2]), max(DataHL[,2])))
                        mtext("Julian day", side = 1, line = 3, font = mtextfont2, cex = mtextcex2)
                        mtext(namepinna, side = 2, line = 4, font = mtextfont2, cex = mtextcex2)
                        box()
                        axis(side = 2, cex.axis = cex_axis2, at = seq(ylim1[1], ylim1[2], by = (ylim1[2]-ylim1[1])/5), lwd = lwdaxis)
                        
                        if (exp.days == TRUE) {
                            axis(side = 1, cex.axis = cex_axis2, at = seq(xlim1[1], xlim1[2], by = round_any((xlim1[2]-xlim1[1])/5, 1)), 
                                 lwd = lwdaxis, labels = seq(0, round_any(max(DataHL[,1])- min(DataHL[,1]), 10, ceiling), by = round_any((xlim1[2]-xlim1[1])/5, 1)), padj = padj2)
                        } else {
                            axis(side = 1, cex.axis = cex_axis2, at = seq(xlim1[1], xlim1[2], by = round_any((xlim1[2]-xlim1[1])/5, 1)), lwd = lwdaxis, padj = padj2)
                            
                        }
                        
                        {
                            dev.off()
                        }
                        print(paste("img done:", namefiles))
                        
                        # Actice the first device, is inactive like x11()
                        # dev.set(ActualDevice)
                    }
                    print("############################################")
                    print("work done")
                    
                } else {print("Big error")}
            }
            
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            
            if(Statselect == 3) {
                
                
                source("Funciones/Functions_R_gape/raworsmt.R", local = TRUE)
                
                if (raworsmt == "org") {
                    
                    
                    source("Funciones/Functions_R_gape/IntervalEquals.R", local = TRUE)
                    
                    
                    
                    if (yesornot == "y") {
                        
                        ###
                        ### This loops allows to avoid the error when someone write wrong the data
                        #source("Funciones/Functions_R_gape/validpinna_one.R")
                        #source("Funciones/Functions_R_gape/validinterval_one.R")
                        ###
                        ###
                        source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE)
                        
                        
                        # To pick the individual selected by the user from the data
                        
                        for (i in 1:length(Pinnaselec)){
                            
                            
                            source("Funciones/Functions_R_gape/StatisticData.R", local = TRUE)
                            
                            
                            jpng(paste("Statistic_data/FFT/Prints/", namefiles, sep = ""), P=c(14,9.8) )
                            
                            par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                            FFT <- fftwin2(Pinnaselec7[,2], freq, FFTy)
                            
                            
                            {
                                dev.off()
                            }
                            print(paste("img done:", namefiles))
                            
                            #Para guardar los datos en un txt:
                            colnames(FFT) <- c("Julianday", namepinna)
                            write.table(x = FFT, file = local.file(paste("Statistic_data/FFT/Data/", namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                            print(paste("DataFFT saved:", namefiles))
                            print("############################################")
                            
                        }
                        print("############################################")
                        print("work done")
                        
                        
                        ###
                        ###
                        
                        
                    } else if (yesornot == "n") {
                        
                        ###
                        ### This loops allows to avoid the error when someone write wrong the data
                        #source("Funciones/Functions_R_gape/validpinna_multiple.R", local = TRUE)
                        #source("Funciones/Functions_R_gape/validinterval_multiple.R", local = TRUE)
                        source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE)
                        
                        ###
                        ###
                        
                        for (i in 1:length(Pinnaselec)){
                            
                            source("Funciones/Functions_R_gape/StatisticData.R", local = TRUE)
                            
                            jpng(paste("Statistic_data/FFT/Prints/", namefiles, sep = ""), P=c(14,9.8) )
                            
                            par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                            FFT <- fftwin2(Pinnaselec7[,2], freq, FFTy)
                            mtext("Amplitude", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
                            mtext("Frequency", side = 1, line = 2.8, font = mtextfont2, cex = mtextcex2)
                            axis(2, cex.axis = cex_axis2, line = -0.8, lwd = lwdaxis)
                            axis(1, cex.axis = cex_axis2, line = 0, lwd = lwdaxis)
                            
                            {
                                dev.off()
                            }
                            print(paste("img done:", namefiles))
                            
                            #Para guardar los datos en un txt:
                            colnames(FFT) <- c("Julianday", namepinna)
                            write.table(x = FFT, file = local.file(paste("Statistic_data/FFT/Data/", namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                            print(paste("DataFFT saved:", namefiles))
                            print("############################################")
                            
                        }
                        print("############################################")
                        print("work done")
                    } else {print("Big error")}
                    
                    
                } else if (raworsmt == "smt") {
                    
                    files <- list.files("Statistic_data/FFT/Input_data/", full.names = TRUE)
                    filesnames <- list.files("Statistic_data/FFT/Input_data/", full.names = FALSE)
                    
                    for (i in 1:length(files)){
                        
                        namefiles <- paste("FFT_",filesnames[i], sep = "")
                        
                        datt2 <- fread(files[i], 
                                       sep = ",",
                                       header = TRUE,
                                       dec = ".",
                                       colClasses = c("numeric", "numeric"))
                        
                        datt <- as.matrix(datt2)
                        
                        
                        jpng(paste("Statistic_data/FFT/Prints/", namefiles, sep = ""), P=c(14,9.8) )
                        print(paste("Calculating DataFFT",filesnames[i], sep = " "))
                        
                        par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                        FFT <- fftwin2(datt, min(datt[,1]), max(datt[,1]), freq, FFTy)
                        # Añado más uno porque si alguien selecciona el día 35, quiero que esté incluido.
                        mtext("Amplitude", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
                        mtext("Frequency", side = 1, line = 2.8, font = mtextfont2, cex = mtextcex2)
                        axis(2, cex.axis = cex_axis2, line = -0.8, lwd = lwdaxis)
                        axis(1, cex.axis = cex_axis2, line = 0, lwd = lwdaxis)
                        
                        
                        {
                            dev.off()
                        }
                        
                        print(paste("img done:", namefiles))
                        
                        write.table(x = FFT, file = local.file(paste("Statistic_data/FFT/Data/", namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                        print(paste("DataFFT saved:", namefiles))
                        print("############################################")
                        
                    }
                    print("work done")
                    
                    
                    # Aquí tengo que añadir el código que lea los datos de low and high
                    
                    
                } else {print("Error in FFT raworsmt")}
                
                
            }
            
            
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            
            if(Statselect == 4) {
                
                
                source("Funciones/Functions_R_gape/raworsmt.R", local = TRUE)
                
                
                
                
                ##########################################################
                ##########################################################
                if (raworsmt == "org") {
                    
                    source("Funciones/Functions_R_gape/IntervalEquals.R", local = TRUE)
                    

                    if (yesornot == "y") {
                        
                        ###
                        ### This loops allows to avoid the error when someone write wrong the data                    
                        #source("Funciones/Functions_R_gape/validpinna_one.R")
                        #source("Funciones/Functions_R_gape/validinterval_one.R")
                        
                        source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE) # Sustityte validpinna_one.R & validinterval_one.R
                        source("Funciones/Functions_R_gape/validlag.R", local = TRUE)
                        #source("Funciones/Functions_R_gape/datapinnainterval.R", local = TRUE)
                        
                        ###
                        ###
                        
                        
                        for(i in 1:length(Pinnaselec)){
                            
                            source("Funciones/Functions_R_gape/StatisticData.R", local = TRUE)
                            
                            namepinna <- paste(namestat, thenames[Pinnaselec][i], sep = "")
                            namefiles <- paste(namestat, Lagselect, "_", thenames[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                            
                            #namepinna <- paste(namestat, individualsname[Pinnaselec][i], sep = "")
                            #namefiles <- paste(namestat, Lagselect, "_", individualsname[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                            
                            jpng(paste("Statistic_data/ACF/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 
                            print(paste("Calculating DataACF", individualsname[Pinnaselec][i], sep = " "))
                            
                            par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                            DataACF <- acf(Pinnaselec7[,2], Lagselect, main = individualsname[Pinnaselec][i], 
                                           cex.main =3.2, axes = FALSE, xlab = "", ylab = "")
                            
                            mtext("Autocorrelation", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
                            mtext("Time lag (1/2sec)", side = 1, line = linemtextX[1], font = mtextfont2, cex = mtextcex2)
                            axis(2, cex.axis = cex_axis2, line = -0.8, lwd = lwdaxis)
                            axis(1, cex.axis = cex_axis2, line = 0, lwd = lwdaxis, padj = padj2)
                            

                            
                            # Extract maxim correlation over 1 day
                            ACFoutput <- list(DataACF$lag, DataACF$acf) # Extraer las partes de la lista que me interesan
                            ACFoutput2 <- matrix(c(unlist(ACFoutput[[1]]), unlist(ACFoutput[[2]])), ncol = 2, nrow = length(ACFoutput[[1]]))
                            ACFoutput3 <- subset.matrix(ACFoutput2, ACFoutput2[, 1] > Lagselect/1.3)
                            ACFoutput4 <- max(ACFoutput3[, 2])
                            ACFoutput5 <- subset.matrix(ACFoutput3, ACFoutput3[,2] == ACFoutput4)
                            #rect(xleft = ACFoutput5[1,1], ybottom = -1, xright = ACFoutput5[1,1]+1, ytop = ACFoutput5[1,2]+0.04, lty = 0, col = "cyan1", border = "cyan1")
                            text(x = ACFoutput5[1,1], y = ACFoutput5[1,2] + 0.05, labels = round_any(ACFoutput5[1,2], 0.001), cex = cex_axis2)
                            {
                                dev.off()
                            }
                            print(paste("img done:", namefiles))
                            
                            # Write.table
                            individual <- rep(individualsname[Pinnaselec][i], length(DataACF$acf))
                            DataACF <- data.frame(individual, DataACF$lag,DataACF$acf )
                            colnames(DataACF) <- c("Indv", "Lag", "ACF")
                            write.table(x = DataACF, file = local.file(paste("Statistic_data/ACF/Data/", namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                            
                            print("DataACF saved")
                            
                        }
                        print("work done")
                        
                        
                        ##############################################################
                        ##############################################################
                        ##############################################################
                        
                    } else if (yesornot == "n") {
                        
                        ###
                        ### This loops allows to avoid the error when someone write wrong the data
                        #source("Funciones/Functions_R_gape/validpinna_multiple.R", local = TRUE)
                        #source("Funciones/Functions_R_gape/validinterval_multiple.R", local = TRUE)
                        source("Funciones/Functions_R_gape/StatisticMenu.R", local = TRUE) # Sustityte validpinna_one.R & validinterval_one.R
                        source("Funciones/Functions_R_gape/validlag.R", local = TRUE)
                        ###
                        ###
                        
                        Pinnaselec2 <- rep(NA,length = length(Pinnaselec))
                        namecol <- rep(NA,length = length(Pinnaselec))
                        
                        for(i in 1:length(Pinnaselec)){
                            Pinnaselec2[i] <- paste("data1$", individualsname[Pinnaselec][i], sep = "")
                        }
                        Pinnaselec3 <- parse(text = Pinnaselec2)
                        Pinnaselec4 <- data.frame(numeric(length(data1[,1])))
                        
                        for(i in 1:length(Pinnaselec)){
                            Pinnaselec4[i] <- eval(Pinnaselec3[i])
                        }
                        
                        
                        for(i in 1:length(Pinnaselec)){
                            intervaldays <- as.numeric(unlist(strsplit(split="-", (interval[i]))))
                            intervald <- intervaldays + Rday[1]-1
                            interval2 <- matrix(data1[,1] >= intervald[1] &  data1[,1] <= intervald[2]+1)
                            Pinnaselec5 <- subset.data.frame(Pinnaselec4, subset = interval2)
                            
                            #namepinna <- paste(namestat, individualsname[Pinnaselec][i], sep = "")
                            #namefiles <- paste(namestat, Lagselect, "_", individualsname[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                            namepinna <- paste(namestat, thenames[Pinnaselec][i], sep = "")
                            namefiles <- paste(namestat, Lagselect, "_", thenames[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
                            
                            jpng(paste("Statistic_data/ACF/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 
                            print(paste("Calculating DataACF", individualsname[Pinnaselec][i], sep = " "))
                            
                            par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                            DataACF <- acf(Pinnaselec5[,i], Lagselect, main = individualsname[Pinnaselec][i], 
                                           cex.main =3.2, axes = FALSE, xlab = "", ylab = "")
                            
                            mtext("Autocorrelation", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
                            mtext("Time lag (1/2sec)", side = 1, line = linemtextX[1], font = mtextfont2, cex = mtextcex2)
                            
                            axis(2, cex.axis = cex_axis2, line = -0.8, lwd = lwdaxis)
                            axis(1, cex.axis = cex_axis2, line = 0, lwd = lwdaxis, padj = padj2)
                            
                            
                            # Extract maxim correlation over 1 day
                            ACFoutput <- list(DataACF$lag, DataACF$acf) # Extraer las partes de la lista que me interesan
                            ACFoutput2 <- matrix(c(unlist(ACFoutput[[1]]), unlist(ACFoutput[[2]])), ncol = 2, nrow = length(ACFoutput[[1]]))
                            ACFoutput3 <- subset.matrix(ACFoutput2, ACFoutput2[, 1] > Lagselect/1.3)
                            ACFoutput4 <- max(ACFoutput3[, 2])
                            ACFoutput5 <- subset.matrix(ACFoutput3, ACFoutput3[,2] == ACFoutput4)
                            #rect(xleft = ACFoutput5[1,1], ybottom = -1, xright = ACFoutput5[1,1]+1, ytop = ACFoutput5[1,2]+0.04, lty = 0, col = "cyan1", border = "cyan1")
                            text(x = ACFoutput5[1,1], y = ACFoutput5[1,2] + 0.05, labels = round_any(ACFoutput5[1,2], 0.001), cex = cex_axis2)
                            {
                                dev.off()
                            }
                            print(paste("img done:", namefiles))
                            
                            # Write.table
                            individual <- rep(individualsname[Pinnaselec][i], length(DataACF$acf))
                            DataACF <- data.frame(individual, DataACF$lag,DataACF$acf )
                            colnames(DataACF) <- c("Indv", "Lag", "ACF")
                            write.table(x = DataACF, file = local.file(paste("Statistic_data/ACF/Data/", namefiles, sep = ""),"txt"), sep=",", row.names = FALSE)
                            
                            print("DataACF saved")
                            
                            
                        }
                        print("work done")
                        
                        
                        # Aquí introduzco el código donde puedo seleccionar un intervalo por individuo
                        
                    } else {print("Error in ACF yesornot")}
                    
                    
                    #########################################################
                    #########################################################
                    #########################################################
                    
                } else if (raworsmt == "smt") {
                    
                    ###
                    ### This loops allows to avoid the error when someone write wrong the data
                    source("Funciones/Functions_R_gape/validlag.R", local = TRUE)
                    ###
                    ###
                    
                    
                    files <- list.files("Statistic_data/ACF/Input_data/", full.names = TRUE)
                    filesnames <- list.files("Statistic_data/ACF/Input_data/", full.names = FALSE)
                    
                    for (i in 1:length(files)){
                        
                        namefiles <- paste(namestat,Lagselect,filesnames[i], sep = "")
                        
                        datt2 <- fread(files[i], 
                                       sep = ",",
                                       header = TRUE,
                                       dec = ".",
                                       colClasses = c("numeric", "numeric"))
                        
                        datt <- as.matrix(datt2)
                        
                        jpng(paste("Statistic_data/ACF/Prints/",namefiles, sep = ""), P=c(14,9.8) ) 
                        print(paste("Calculating DataACF",filesnames[i], sep = " "))
                        
                        par(mai = c(mai2[1],mai2[2],mai2[3],mai2[4]))
                        DataACF <- acf(datt[,2], Lagselect, main = filesnames[i], 
                                       cex.main =3.2, axes = FALSE, xlab = "", ylab = "")
                        
                        mtext("Autocorrelation", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
                        mtext("Time lag (1/2sec)", side = 1, line = linemtextX[1], font = mtextfont2, cex = mtextcex2)
                        
                        axis(2, cex.axis = 2.2, line = -0.8, lwd = lwdaxis)
                        axis(1, cex.axis = 2.2, line = 0, lwd = lwdaxis, padj = padj2)
                        
                        
                        #Extract maxim correlation over 1 day
                        ACFoutput <- list(DataACF$lag, DataACF$acf) # Extraer las partes de la lista que me interesan
                        ACFoutput2 <- matrix(c(unlist(ACFoutput[[1]]), unlist(ACFoutput[[2]])), ncol = 2, nrow = length(ACFoutput[[1]]))
                        ACFoutput3 <- subset.matrix(ACFoutput2, ACFoutput2[, 1] > Lagselect/1.3)
                        ACFoutput4 <- max(ACFoutput3[, 2])
                        ACFoutput5 <- subset.matrix(ACFoutput3, ACFoutput3[,2] == ACFoutput4)
                        #rect(xleft = ACFoutput5[1,1], ybottom = -1, xright = ACFoutput5[1,1]+1, ytop = ACFoutput5[1,2]+0.04, lty = 0, col = "cyan1", border = "cyan1")
                        text(x = ACFoutput5[1,1], y = ACFoutput5[1,2] + 0.05, labels = round_any(ACFoutput5[1,2], 0.001), cex = cex_axis2)
                        {
                            dev.off()
                        }
                        print(paste("img done:", namefiles))
                        
                    }
                    print("work done")
                    
                    # Aquí tengo que añadir el código que lea los datos de low and high
                    
                    
                } else {print("Error in ACF raworsmt")}
                
                
            }
            
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            
            
            if (any(Statselect == 5, Statselect == 6, Statselect == 7, Statselect == 8)){
                
                
                
                cat("###########################################\n")
                cat("Cross Correlation selected\n")
                
                ###
                ### This loops allows to avoid the error when someone write wrong the data
                #source("Funciones/Functions_R_gape/validpinna_two.R")
                #source("Funciones/Functions_R_gape/datapinnainterval.R")
                
                source("Funciones/Functions_R_gape/validdataCCR.R", local = TRUE)
                source("Funciones/Functions_R_gape/validinterval_one.R", local = TRUE)
                intervaldays <- as.numeric(unlist(strsplit(split="-", (interval))))
                
                source("Funciones/Functions_R_gape/validlag.R", local = TRUE)
                
                whatdaysA1 <- paste(dataselec1[1], "$", Julianday, sep = "")
                whatdaysA2 <- parse(text = whatdaysA1)
                whatdaysA3 <- data.frame(eval(whatdaysA2))
                intervalA2 <- matrix(whatdaysA3 >= intervaldays[1] &  whatdaysA3 <= intervaldays[2]+1)
                ##
                variableA <- variableselect[1]
                variableA2 <- parse(text = variableselect[1])
                ##
                ##
                whatdaysB1 <- paste(dataselec2[1], "$", Julianday, sep = "")
                whatdaysB2 <- parse(text = whatdaysB1)
                whatdaysB3 <- data.frame(eval(whatdaysB2))
                intervalB2 <- matrix(whatdaysB3 >= intervaldays[1] &  whatdaysB3 <= intervaldays[2]+1)
                ##
                variableB <- variableselect[2]
                variableB2 <- parse(text = variableselect[2])
                
                
                ###
                ###
                
                
                #namepinna <- paste(namestat, "Indv.", Pinnaselec[i], "freq.", freq, sep = "")
                namefiles <- paste(namestat, Lagselect, "_", dataselec1[2], "_", dataselec2[2], "_", "freq.", freq, "_", intervaldays[1], "_", intervaldays[2], sep = "")
                
                
                cat("Write the path to two datas, put each one betwen quotes and a coma betwen them\n")
                PinnaselecLowf = readline(prompt = "Select two Pinnas Lowfrequency data:")
                cat("###########################################\n")
                cat("Write the path to two datas, put each one betwen quotes and a coma betwen them\n")
                PinnaselecHighf = readline(prompt = "Select two Pinnas Highfrequency data:")
                
                raw1 <- subset.data.frame(data.frame(whatdaysA3, eval(variableA2)), subset = intervalA2)
                raw2 <- subset.data.frame(data.frame(whatdaysB3, eval(variableB2)), subset = intervalB2)
                
                
                if (Statselect == 6 | Statselect == 7){
                    ## Elimino las comillas creadas en el redline, lo separo por datos para poder seleccionar
                    ## cada archivo
                    removequotesLowf <- gsub("\"", "", PinnaselecLowf)
                    removequotesLowf2 <- unlist(strsplit(split=",", (removequotesLowf)))
                    
                    Lowf11 <- read.table(removequotesLowf2[1], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)
                    Lowf1 <- data.frame(Lowf11[,2])
                    Lowf22 <- read.table(removequotesLowf2[2], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)
                    Lowf2 <- data.frame(Lowf22[,2])
                } else if (Statselect == 7 | Statselect == 8){
                    
                    ## Elimino las comillas creadas en el redline, lo separo por datos para poder seleccionar
                    ## cada archivo
                    removequotesHighf <- gsub("\"", "", PinnaselecHighf)
                    removequotesHighf2 <- unlist(strsplit(split=",", (removequotesHighf)))
                    
                    Highf11 <- read.table(removequotesHighf2[1], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)
                    Highf1 <- data.frame(Highf11[,2])
                    Highf22 <- read.table(removequotesHighf2[2], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)
                    Highf2 <- data.frame(Highf22[,2])
                    
                    
                }
                
                
                jpng(paste("Statistic_data/CCR/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 
                
                if (Statselect == 5){
                    crosscorrelation(raw1[,2], raw2[,2], Lowf1[,1], Lowf2[,1], Highf1[,1], Highf2[,1], Lagselect)
                } else{
                    crosscorrelation2(raw1[,2], raw2[,2], Highf1[,1], Highf2[,1], Lagselect)
                }
                

                {
                    dev.off()
                }
                print(paste("img done:", namefiles))
                
                
            }
            
            
            
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            #################################################################
            
            
            
            if (esc == TRUE){
                dosebas = sebasplot(data1)
            }
            
            dosebas = sebasplot(data1)
            thebutts = rowBUTTONS(allbuttons, col = c(buttscolours), pch = c(allsimbol), cex = buttonsize)
            zloc = list(x=NULL, y=NULL)
            ###  do what ever button 2 is supposed to do
        }
        
        
        
        
        iloc = locator(1,type='p')
        ##### print(iloc)
        zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
        Nclick = length(iloc$x)
        if(is.null(zloc$x)) { return(sloc) }
        K =  whichbutt(iloc , thebutts)
        
    }
    
}