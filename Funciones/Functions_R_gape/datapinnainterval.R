
Pinnaselec2 <- rep(NA,length = length(Pinnaselec))
namecol <- rep(NA,length = length(Pinnaselec))

for(i in 1:length(Pinnaselec)){
    Pinnaselec2[i] <- paste("dataindividuals$", individualsname[Pinnaselec][i], sep = "")
}
Pinnaselec3 <- parse(text = Pinnaselec2)
Pinnaselec4 <- data.frame(numeric(length(dataindividuals$Julianday)))

for(i in 1:length(Pinnaselec)){
    Pinnaselec4[i] <- eval(Pinnaselec3[i])
}

intervaldays <- as.numeric(unlist(strsplit(split="-", (interval))))
interval2 <- matrix(dataindividuals$Julianday >= intervaldays[1] &  dataindividuals$Julianday <= intervaldays[2]+1)
Pinnaselec5 <- subset.data.frame(Pinnaselec4, subset = interval2)
