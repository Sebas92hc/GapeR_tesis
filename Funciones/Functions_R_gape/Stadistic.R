library(signal)

library(fMultivar)
library(RSEIS)

library(stats)

library(RPMG)

Lowfreq <- function(vec1,vec2,byh,sps,flow) {
    
    #vec1 = Pinnaselec7[,1]
    #vec2 = Pinnaselec7[,2]
    #Highfreq(Pinnaselec7[,1],Pinnaselec7[,2],freq/(24*3600),1/freq,1/(hfreq1*60*60),1/(hfreq2*60*60))
    
    expcev2 <- function(vec1,vec2,byh) {
        ## vec1 and vec2 are the vectors to be expanded 
        ## and byh=600/(24*3600) in this case
        
        jdadd1=seq(min(vec1, na.rm = TRUE)-100,min(vec1, na.rm = TRUE)-byh,by=byh)
        jdadd2=seq(max(vec1, na.rm = TRUE)+byh,max(vec1, na.rm = TRUE)+100,by=byh)
        
        jdadd1=as.matrix(jdadd1)
        jdadd2=as.matrix(jdadd2)
        
        en=rep(mean(vec2, na.rm = TRUE),length(jdadd2))
        st=rep(mean(vec2, na.rm = TRUE),length(jdadd1))
        
        en=as.matrix(en)
        st=as.matrix(st)
        
        ex1=rbind(jdadd1,vec1)
        ex2=rbind(ex1,jdadd2)
        
        ex3=rbind(st,vec2)
        ex4=rbind(ex3,en)
        
        cbind (ex2,ex4)
    }
    
    Rafilt3=function(nacra,ord,freq) {
        
        mant1=butter(ord, freq,type = "low")
        pet=filtfilt(mant1, nacra)
        return(pet)
    }
    
    f1 = flow*2/sps ###in nyquist units, which is equal to 1/2 *sampling frequency or 1/2*1/dt, where dt is the sampling interval.
    ###so, it doesn't matter if in the function we input time interval or freqency, provided we calculate sampling
    ###units correctly. Also, the dt or frequency have to be in the same units than the input files.
    freqs = f1
    
    tete=expcev2(vec1,vec2,byh)
    teteSp2=Rafilt3(tete[,2],2,freqs)
    
    ll=cbind(tete[,1],teteSp2)
    
    ty=window(ll,length(seq(min(tete[,1]),min(vec1),by=byh))+100,length(seq(min(tete[,1]),max(vec1),by=byh))-100)
    
    ty=as.matrix(ty)
    
    return(ty)
    
    
}


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################



pruebahigh <- function(vec1,vec2,byh,sps,fhigh) {
    
    expcev2 <- function(vec1,vec2,byh) {
        ## vec1 and vec2 are the vectors to be expanded 
        ## and byh=600/(24*3600) in this case
        
        jdadd1=seq(min(vec1)-50,min(vec1)-byh,by=byh)
        jdadd2=seq(max(vec1)+byh,max(vec1)+50,by=byh)
        
        jdadd1=as.matrix(jdadd1)
        jdadd2=as.matrix(jdadd2)
        
        en=rep(mean(vec2),length(jdadd2))
        st=rep(mean(vec2),length(jdadd1))
        
        en=as.matrix(en)
        st=as.matrix(st)
        
        ex1=rbind(jdadd1,vec1)
        ex2=rbind(ex1,jdadd2)
        
        ex3=rbind(st,vec2)
        ex4=rbind(ex3,en)
        
        cbind (ex2,ex4)
    }
    
    Rafilt3=function(nacra,ord,freq) {
        
        mant1=butter(ord, freq,type = "high")
        pet=filtfilt(mant1, nacra)
        return(pet)
    }
    
    f1 = fhigh*2/sps ###in nyquist units, which is equal to 1/2 *sampling frequency or 1/2*1/dt, where dt is the sampling interval.
    ###so, it doesn't matter if in the function we input time interval or freqency, provided we calculate sampling
    ###units correctly. Also, the dt or frequency have to be in the same units than the input files.
    freqs = f1
    
    tete=expcev2(vec1,vec2,byh)
    teteSp2=Rafilt3(tete[,2],2,freqs)
    
    ll=cbind(tete[,1],teteSp2)
    
    ty=window(ll,length(seq(min(tete[,1]),min(vec1),by=byh))+100,length(seq(min(tete[,1]),max(vec1),by=byh))-100)
    
    ty=as.matrix(ty)
    
    return(ty)
    
    
}


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

Highfreq <- function(vec1,vec2,byh,sps,flow,fhi)
    
{
    # sps es la frecuencia de los datos en segundos
    # en flow 1/(2*24*60*60) todo el ruido que sea mayor de 48 horas es eliminado
    # en fhi 1/(15*60*60) todo el ruido que sea menor de 15 horas es eliminado
    
    expcev2=function(vec1,vec2,byh) ## vec1 and vec2 are the vectors to be expanded 
        ## and byh=600/(24*3600) in this case
    {
        jdadd1=seq(min(vec1)-200,min(vec1)-byh,by=byh)
        jdadd2=seq(max(vec1)+byh,max(vec1)+200,by=byh)
        
        jdadd1=as.matrix(jdadd1)
        jdadd2=as.matrix(jdadd2)
        
        en=rep(mean(vec2),length(jdadd2))
        st=rep(mean(vec2),length(jdadd1))
        
        en=as.matrix(en)
        st=as.matrix(st)
        
        ex1=rbind(jdadd1,vec1)
        ex2=rbind(ex1,jdadd2)
        
        ex3=rbind(st,vec2)
        ex4=rbind(ex3,en)
        
        cbind (ex2,ex4)
        
    }
    
    
    Rafilt2=function(nacra,ord,freq)
    {
        mant1=butter(ord, freq,type = "pass")
        pet=filtfilt(mant1, nacra)
        return(pet)
    }
    
    
    f1 = flow*2/sps ###in nyquist units, which is equal to 1/2 *sampling frequency or 1/2*1/dt, where dt is the sampling interval.
    ###so, it doesn't matter if int eh function we input time interval or freqency, provided we calculate sampling
    ###units correctly. Also, the dt or frequency have to be in the same units than the input files.
    f2 = fhi*2/sps
    freqs = c(f1,f2)
    
    tete=expcev2(vec1,vec2,byh)
    teteSp2=Rafilt2(tete[,2],2,freqs)
    
    ll=cbind(tete[,1],teteSp2)
    
    ty=window(ll,length(seq(min(tete[,1]),min(vec1),by=byh))+200,length(seq(min(tete[,1]),max(vec1),by=byh))-200)
    
    ty=as.matrix(ty)
    
    return(ty)
    
}




########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

#225, 305, 40, c(1,600000)

#fil = as.matrix(datosrafa)
#start = 225
#end = 305
#dt = 20
#limy = c(1,600000)

#fftwin2=function(fil,start,end,dt,limy)
fftwin2=function(fil,dt,limy)
{
    
    #aa = which(fil[,1] > start & fil [,1] < end)
    
    #filquin3=window(fil,min(aa),max(aa))
    
    #tsig=filquin3[,2]-mean(filquin3[,2])
    tsig = fil
    
    fsig=fft(tsig)
    
    dt=dt 
    
    df=1/(length(tsig)*dt)
    
    ff2=seq(from=1,by=1,length=length(tsig))*df 
    
    mm=cbind(ff2,Mod(fsig)[1:length(ff2)])
    
    #rr=window(mm,1/(2000000/dt)*length(mm[,1]),1/(1000/dt)*length(mm[,1])) # Para la variación diaria
    #rr=window(mm,1/(400000/dt)*length(mm[,1]),1/100*length(mm[,1])) # Para la variación diaria
    
    rr=window(mm,1/(120000/dt)*length(mm[,1]),1/100*length(mm[,1]))
    
    #plot(rr,type="n",log="xy",ylim=limy,xlab="Frequency",ylab="Amplitude",cex.axis=1.2,cex.lab=1.4,las=1)
    plot(rr, type="n", log="xy", ylim=limy, xlab = "", ylab = "", 
         cex.main =3.2, axes = FALSE)
    
    #gg <- expression(s^{-1} )
    gg <- bquote("Frequency (s"^"-1"*")")
    mtext("Amplitude", side = 2, line = 2.2, font = mtextfont2, cex = mtextcex2)
    mtext(gg, side = 1, line = linemtextX[1], font = mtextfont2, cex = mtextcex2)
    axis(2, cex.axis = cex_axis2, line = -0.8, lwd = lwdaxis)
    axis(1, cex.axis = cex_axis2, line = 0, lwd = lwdaxis, padj = padj2)
    
    
    lines(rr,lwd=2)
    
    FFToutput <- subset.matrix(rr, rr[,1] == rr[,1][rr[,2]==max(rr[,2])])
    
    print(rr[,1][rr[,2]==max(rr[,2])])
    aa=1/(rr[,1][rr[,2]==max(rr[,2])])
    bb=aa/3600
    
    text(x = FFToutput[1], y = FFToutput[2] + ((limy[2]-limy[1])/10) + 1000000, labels = round_any(bb, 0.01), cex = cex_axis2)
    
    
    
    print(aa)
    print(bb)
    return(mm)
    
    
}



########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################



crosscorrelation=function(f1,f2,f3,f4,f5,f6,lags) ##plotting correlations
    
{
    par(mfrow=c(2,3))
    par(mai=c(0.6,0.6,0.4,0.3))
    
    ccf(f1,f2,lags)
    
    ccf(f3,f4,lags)
    
    
    ccf(f5,f6,lags)
    
    
    par(mai=c(0.6,0.6,0.4,0.3))
    
    plot(f1-mean(f1),ylim=c(-35,30),type="n")
    lines(f1-mean(f1),col="red")
    lines(f2-mean(f2),col="blue")
    
    cc=cor(f1,f2)
    
    cc2=round(cc,digits=2)
    
    legend(30,-15,paste("raw R = ", cc2),bty="n")
    #legend(min(f1[,1]), 25,paste("raw R = ", cc2),bty="n")
    
    plot(f3-mean(f3),ylim=c(-35,30),type="n")
    lines(f3-mean(f3),col="red")
    lines(f4-mean(f4),col="blue")
    
    
    dd=cor(f3,f4)
    
    cc3=round(dd,digits=2)
    
    #legend(30,-15,paste("lowfreq R = ", cc3),bty="n")
    legend(min(f3), 25,paste("lowfreq R = ", cc3),bty="n")
    
    plot(f5-mean(f5),ylim=c(-35,30),type="n")
    lines(f5-mean(f5),col="red")
    lines(f6-mean(f6),col="blue")
    
    
    ee=cor(f5,f6)
    
    cc4=round(ee,digits=2)
    
    #legend(30,-15,paste("highfreq R = ", cc4),bty="n")
    legend(min(f5), 25,paste("highfreq R = ", cc4),bty="n")
    
    
    
}



crosscorrelation2=function(f1,f2,f3,f4,lags) ##plotting correlations
    
{
    par(mfrow=c(2,2))
    #par(mai=c(0.6,0.6,0.4,0.3))
    
    dataCCFraw <- ccf(f1,f2,lags)
    legend(5000, max(dataCCFraw$acf), paste("CCFraw = ", round_any(max(dataCCFraw$acf), 0.001)), bty="n")
    
    dataCCFhigh <- ccf(f3,f4,lags)
    legend(5000, max(dataCCFhigh$acf), paste("CCFhigh = ", round_any(max(dataCCFhigh$acf), 0.001)), bty="n")
    
    

    
    #par(mai=c(0.6,0.6,0.4,0.3))
    #par(mfrow=c(2,2))
    
    plot(f1-mean(f1),ylim=c(-25,25),type="n")
    lines(f1-mean(f1),col="red")
    lines(f2-mean(f2),col="blue")
    
    cc=cor(f1,f2)
    
    cc2=round(cc,digits=2)
    
    legend(30,-15,paste("raw R = ", cc2),bty="n")
    #legend(min(f1[,1]), 25,paste("raw R = ", cc2),bty="n")
    
    plot(f3-mean(f3),ylim=c(-35,35),type="n")
    lines(f3-mean(f3),col="red")
    lines(f4-mean(f4),col="blue")
    
    
    dd=cor(f3,f4)
    
    cc3=round(dd,digits=2)
    
    #legend(30,-15,paste("lowfreq R = ", cc3),bty="n")
    legend(min(f3), 25,paste("highfreq R = ", cc3),bty="n")
    
    
    
}
