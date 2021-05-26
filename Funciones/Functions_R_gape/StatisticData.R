
if (yesornot == "y"){
    Pinnaselec2 <- paste(whatdata, thenames[Pinnaselec[i]], sep = "")
    Pinnaselec3 <- parse(text = Pinnaselec2) ## parse convierte el texto en expression
    Pinnaselec4 <- Pinnaselec3[[1]] ## Esto saca el texto del expression sin comillas
    Pinnaselec5 <- data.frame(whatdata2[,1], eval(Pinnaselec4)) ## Eval permite utilizar la clase "call"
    Pinnaselec6 <- as.matrix(Pinnaselec5)
    #intervaldays <- as.numeric(unlist(strsplit(split="-", (interval))))
    interval2 <- matrix(whatdata2[,1] >= intervald[1] &  whatdata2[,1] <= intervald[2]+1)
    Pinnaselec7 <- subset.matrix(Pinnaselec6, subset = interval2)
    
    #añadir un eliminar NA de la matriz, y que te avise de que los hay
    
    namepinna <- paste(namestat, thenames[Pinnaselec][i], sep = "")
    namefiles <- paste(namestat, thenames[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
    
    
} else {
    Pinnaselec2 <- paste(whatdata, sensorsname[Pinnaselec[i]], sep = "")
    Pinnaselec3 <- parse(text = Pinnaselec2) ## parse convierte el texto en expression
    Pinnaselec4 <- Pinnaselec3[[1]] ## Esto saca el texto del expression sin comillas
    Pinnaselec5 <- data.frame(whatdata2[,1], eval(Pinnaselec4)) ## Eval permite utilizar la clase "call"
    Pinnaselec6 <- as.matrix(Pinnaselec5)
    #intervaldays <- as.numeric(unlist(strsplit(split="-", (interval[i]))))
    interval2 <- matrix(whatdata2[,1] >= intervald[1] &  whatdata2[,1] <= intervald[2]+1)
    Pinnaselec7 <- subset.matrix(Pinnaselec6, subset = interval2)
    
    namepinna <- paste(namestat, sensorsname[Pinnaselec][i], "_", sep = "")
    namefiles <- paste(namestat, sensorsname[Pinnaselec][i], "_", intervaldays[1], "_", intervaldays[2], sep = "")
    
    
}
    