
moonglyph<-function(x,y,rx, ry=rx, perc=30, ...)
{
    #### plot(c(-1,1)  , c(-1,1), type='n' , asp=1 )
    if(missing(ry)) ry=rx
    
    if(missing(perc)) perc=30
    
    if(perc>100) perc = 100
    if(perc<0) perc = 0
    
    c1 = circle(1)
    polygon(rx*c1$x+x, ry*c1$y+y, col='black', border=grey(0.85), ... )
    
    
    lam0 = pi/2
    
    phi = seq(from=-90, to=90, by=5)*pi/180
    
    
    i = pi * seq(from = 90, to = 90+180, by = 1)/180
    
    
    cx = cos(i)
    cy = sin(i)
    
    j = 180*perc/100
    
    lam = j*pi/180
    R = sqrt(2)/2
    kp = sqrt(2/(1+cos(phi)*cos(lam-lam0)))
    px = R*kp*cos(phi)*sin(lam-lam0)
    py = R * kp*sin(phi)
    ##    lines(x,y, col='black' , lwd=1)
    
    polygon(rx*c(cx, px)+x ,ry*c(cy, py)+y, col='white', border=grey(0.85), ...)
    
    
    
}

moonphases<-function( tm )
{
    require(tripEstimation)
    
    N = length(tm)
    PLOTIT = FALSE
    
    pi2=2*pi
    piO2 = pi/2
    tauSUN = 8.32/(1440.0 * 36525.0)  ##   8.32 minutes for sun to hit earth
    ##############   this is subtracted from the Sun's time to account for the
    ##                 travel of light to the moon
    sun = mini.sun(tm-tauSUN)
    moon = lunar(tm)
    
    D =  (15*moon$RA -   15*sun$RA)*pi/180
    
    ##################    get phase of the moon, page147 in Peter Duffet-Smith
    ########   Practical Astronomy with your calculator
    Faze = 100* 0.5 * (1-cos(D))
    
    if(PLOTIT)
    {
        
        philune = 15*moon$RA*pi/180
        phisun = 15*sun$RA*pi/180
        
        mx = cos(philune)
        my = sin(philune)
        
        sx = cos(phisun)
        sy = sin(phisun)
        
        mr = seq(from=.8, by=(.2)/length(moon$RA), length=length(moon$RA))
        pal = tomo.colors(100)
        plot(mr*mx, mr*my , asp=1, col=pal[ trunc(1+ 100*(1:length(mx))/(length(mx)+1) )    ] )
        points(0,0, pch=2)
        
        points(sx, sy, pch=4, col=pal[ trunc(1+ 100*(1:length(sx))/(length(sx)+1) )    ]  )
        
        ##  text(mr*mx, mr*my, labels=format(trunc(p) ), col=pal[ trunc(1+ 100*(1:length(sx))/(length(sx)+1) )    ] )
        ##      ###   ang = acos(  mx*sx + my*sy )
        ##      ###   p = ang*180/pi
        ##      ###   pfaze  = 100*p/180
        
        for(i in seq(from=1, to=length(mx), by=20) )
        {
            moonglyph(mr[i]*mx[i],  mr[i]*my[i],  .05  , perc=Faze[i])
        }
    }
    
    return(Faze)
    
}



getSunMoon<-function( Dstart ,  Dend  , PINNAlat= 38.346041,  PINNAlon= -0.484756, TIMESHIFT=0 )
{ #PINNAlat=(38+10/60) ; PINNAlon= -(0+28/60)
    ##########   time shift is seconds to GMT, 1 hour=3600 
    if(missing(TIMESHIFT)) { TIMESHIFT=0  } 
    
    require(tripEstimation)
    require(RSEIS)
    
    days = as.numeric( round(Dend-Dstart) )
    
    #days = as.numeric( round((Dend+365*(yr-2010))-Dstart) )
    #days2=days[1]
    
    ##   tm <- Dstart + seq(by = 300, length = 15*24*days2)
    
    tm <- Dstart + seq(by = 300, length = 15*24*days)
    
    ### tm2 <- Dstart + seq(by = 3600, length = 24*days2)
    
    sun <- mini.sun(tm)
    
    ###   sun2 <- mini.sun(tm2)
    ###  24*3600*( sun$MJD -   sun2$MJD )
    
    rtp <- astro(PINNAlon, PINNAlat, sun)
    atm = as.POSIXlt(tm)
    
    ###  atm2 = as.POSIXlt(tm2)
    ###   cbind(atm$yday, atm$hour, atm$min, atm$sec,atm2$yday, atm2$hour, atm2$min, atm2$sec) 
    
    origyear = min(atm$year)
    EDAY = EPOCHday(atm$year, jd=atm$yday, origyr=origyear)
    
    jatm = EDAY$jday+atm$hour/24+atm$min/(24*60)+atm$sec/(24*3600)
    
    moonpos = lunar(tm)
    ktp =  astro(PINNAlon, PINNAlat, moonpos )
    
    p = moonphases(tm)
    
    return(list(Moon=ktp, Sun=rtp, Phase=p , jatm=jatm+TIMESHIFT/(24*3600), atm=atm+TIMESHIFT))
    
}
