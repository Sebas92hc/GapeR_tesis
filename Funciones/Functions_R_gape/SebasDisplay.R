
source("Funciones/Functions_R_gape/Stadistic.R")
source("Funciones/Functions_R_gape/Colorfunction2.R")
source("Funciones/Functions_R_gape/ReadData.R")
ReadData("Datos/Pinna_Activity/Dataindividuals_2s_AVG.txt", "Datos/Pinna_Activity/Datasensors_0001.txt")


source("Funciones/Functions_R_gape/GapeR.R")

gapeR(data1 = dataindividuals, data2 = datasensors, lightdata = 1, 
      treatment = 1, blackwhite = TRUE, exp.days = TRUE, fixLYaxis = c(0, 24),
      box = FALSE, dates = FALSE, bty = "n", mtexty = FALSE, legend = "horizontal")

####
# Una vez abierto el programa, los botones quedan habilitados para su uso. El uso de cada botón
# requerirá de seguir las instrucciones que aparezcan en la consola de R o pulsar el botón de 
# refresh para actualizar la gráfica.
###