1A, 1A, 2A, 1B, 1B, 2B

2A, 3A, 3A, 2B, 3B, 3B

high11 <- c("Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.1A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.1A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.2A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.1B_freq.2_147_173_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.1B_freq.2_147_173_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.2B_freq.2_147_173_48_15_0000.txt")

high22 <- c("Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.2A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.3A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.3A_freq.2_161_186_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.2B_freq.2_147_173_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.3B_freq.2_147_173_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Summer/HighF_48_15_Pinna.3B_freq.2_147_173_48_15_0000.txt")
intervaldays1 <- c(161,161,161,147,147,147)
intervaldays2 <- c(186,186,186,173,173,173)
for (i in 1:6){
    

high11 <- c("Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.1A_freq.2_117_158_48_15_0000.txt",
           "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.1A_freq.2_117_158_48_15_0000.txt",
           "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.2A_freq.2_117_158_48_15_0000.txt",
           "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.1B_freq.2_117_132_48_15_0000.txt",
           "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.1B_freq.2_117_132_48_15_0000.txt",
           "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.2B_freq.2_117_132_48_15_0000.txt")

high22 <- c("Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.2A_freq.2_117_158_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.3A_freq.2_117_158_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.3A_freq.2_117_158_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.2B_freq.2_117_132_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.3B_freq.2_117_132_48_15_0000.txt",
            "Statistic_data/Low&HighF/Data/High/Winter/HighF_48_15_Pinna.3B_freq.2_117_132_48_15_0000.txt")

intervaldays1 <- c(117,117,117,117,117,117)
intervaldays2 <- c(158,158,158,132,132,132)
pinnas1 <- c("Pinna.1A", "Pinna.1A", "Pinna.2A", "Pinna.1B", "Pinna.1B", "Pinna.2B")
pinnas2 <- c("Pinna.2A", "Pinna.3A", "Pinna.3A", "Pinna.2B", "Pinna.3B", "Pinna.3B")


pinna1 <- parse(text = paste("dataindividuals$", pinnas1[i], sep = ""))
pinna2 <- parse(text = paste("dataindividuals$", pinnas2[i], sep = ""))


high1 <- read.table(high11[i], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)
high2 <- read.table(high22[i], sep = ",", dec = ".", skipNul = TRUE, fill = TRUE, header = TRUE)

high1 <- high1[,2]
high2 <- high2[,2]


lags <- 50000



intervalA2 <- matrix(dataindividuals$Julianday >= intervaldays1[i] &  dataindividuals$Julianday <= intervaldays2[i]+1)

raw1 <- subset.data.frame(data.frame(dataindividuals$Julianday, eval(pinna1)), subset = intervalA2)
raw2 <- subset.data.frame(data.frame(dataindividuals$Julianday, eval(pinna2)), subset = intervalA2)
raw1 <- raw1[,2]
raw2 <- raw2[,2]

namefiles <- paste("CCR_", lags, "_", pinnas1[i], "_", pinnas2[i], "_", "freq.", 2, "_", intervaldays1[i], "_", intervaldays2[i], sep = "")

jpng(paste("Statistic_data/CCR/Prints/", namefiles, sep = ""), P=c(14,9.8) ) 


#crosscorrelation2=function(raw1,raw2,high1,high2,lags) ##plotting correlations
    
#{
    par(mfrow=c(2,2))
    #par(mai=c(0.6,0.6,0.4,0.3))
    dataCCFraw <- ccf(raw1,raw2,lags)
    #text(x = 15000, y = max(dataCCFraw$acf), labels = round_any(max(dataCCFraw$acf), 0.001))
    legend(5000, max(dataCCFraw$acf), paste("CCFraw = ", round_any(max(dataCCFraw$acf), 0.001)), bty="n")
    
    print(paste("ccf1 done:", namefiles))
    
    dataCCFhigh <- ccf(high1,high2,lags)
    #text(x = 15000, y = max(dataCCFhigh$ccf), labels = round_any(max(dataCCFhigh$ccf), 0.001))
    legend(5000, max(dataCCFhigh$acf), paste("CCFhigh = ", round_any(max(dataCCFhigh$acf), 0.001)), bty="n")
    
    
    print(paste("ccf2 done:", namefiles))
    
    
    #par(mai=c(0.6,0.6,0.4,0.3))
    #par(mfrow=c(2,2))
    
    plot(raw1-mean(raw1),ylim=c(-25,20),type="n")
    lines(raw1-mean(raw1),col="red")
    lines(raw2-mean(raw2),col="blue")
    
    cc=cor(raw1,raw2)
    
    cc2=round(cc,digits=2)
    
    legend(30,-15,paste("raw R = ", cc2),bty="n")
    #legend(min(raw1[,1]), 25,paste("raw R = ", cc2),bty="n")
    
    plot(high1-mean(high1),ylim=c(-35,30),type="n")
    lines(high1-mean(high1),col="red")
    lines(high2-mean(high2),col="blue")
    
    
    dd=cor(high1,high2)
    
    cc3=round(dd,digits=2)
    
    #legend(30,-15,paste("lowfreq R = ", cc3),bty="n")
    legend(min(high1), 25,paste("highfreq R = ", cc3),bty="n")
    
    
    
#}
    
    {
        dev.off()
    }
    print(paste("img done:", namefiles))
    
}
