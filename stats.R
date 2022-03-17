#this file is for computing statistics based
#on the time series for the fires in protected
#natural areas in Mexico.

#First the time series is plotted.
#A few comments on the plot are given
#second, the autocorrelation function (ACF) is computed
#for comparison purposes.
#A few comments on the ACF are given
#clusters are measured for the periodicity in ACF
#Then the power spectra is computed with function
#pwrspc (this function was written by hand and is
#also provided)
#the power spectra is plotted
#a few comments are given
#the largest peaks are selected with function
#tpeaks (this function was written by hand and
#is also provided)
#the points are plotted together with the
#periodogram

##1  ### balaan
plot(f.a.anp$balaan$date,f.a.anp$balaan$area,type="h")
#before 2009 there are few fires and very little
#after 2009 fires make distinguisible clusters and spikes are high
hist(as.numeric(fire.dist$balaan),breaks=20)
#overall few large fires, lots of small fires
cogr<-acf(f.a.anp$balaan$area,lag.max = 1000,ci.type="ma")
#there are clusters, at 0, 400 and 700
which(cogr$acf==max(cogr$acf[350:450]))
#period is 400
## now with fft
pgm<-pwrspc(f.a.anp$balaan$area,1)
plot(pgm,type="l")
#there are thee peaks almost the same heigth and slightly larger
#than random noise
tpeaks(pgm,3)#182.0000, 373.5789, 709.8000
points(pgm[c(6,11,16,20),])
## median weibull
#################
library(MASS)
dist<-as.numeric(fire.dist$balaan)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#7.996138
#2  ## calakmul
plot(f.a.anp$calakmul$date,f.a.anp$calakmul$area,type="h")
#fires are very sprea and area increases in time
cogr<-acf(f.a.anp$calakmul$area,lag.max = 1200,ylim=c(0,0.4),ci.type="ma")
#one small cluster at 400 one large cluster at 800. This could mean
#that there are weak yearly pattern and strong bienial pattern
which(cogr$acf==max(cogr$acf[300:500]))
#361 not significant
which(cogr$acf==max(cogr$acf[700:900]))
#783 very significant
## now with fft
pgm<-pwrspc(f.a.anp$calakmul$area,1)
#the power spectrum has one larger peak, the second largest is probably nothing
#the third largest matches the acf. So there are two periods, but the annual
#period is not significant, and the biennial is significant. This is almost
#conflicting with the fft
plot(pgm,type="l")
tpeaks(pgm,3) #374.8421, 712.2000
points(pgm[c(6,11,20),])
#############weibull median 
dist<-as.numeric(fire.dist$calakmul)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#3.65432
#3  ## Carmen
plot(f.a.anp$carmen$date,f.a.anp$carmen$area,type="h")
#very few fires, spikes are larger in 2011
cogr<-acf(f.a.anp$carmen$area,lag.max = 1500)
#one cluster relatively well defined, no yearly clusters
which(cogr$acf==max(cogr$acf[1000:1500]))
#1062
##now with fft
pgm<-pwrspc(f.a.anp$carmen$area,1)
plot(pgm,type="l") #all peaks are the same. The peak for 1000 is slightly shorter
#is the eigth in the position of the peaks, but in th height are comparable
tpeaks(pgm,10)
points(pgm[c(8,14,20),])
pgm[c(8,14,20),]
#############weibull median 
dist<-as.numeric(fire.dist$carmen)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#3.65432
#failed
#4  Centla
plot(f.a.anp$centla$date,f.a.anp$centla$area,type="h")
#many fires, seasonality is almost yearly, area is high
cogr<-acf(f.a.anp$centla$area,lag.max=1000,ylim=c(-0.05,0.2))
#very well defined oscillatory pattern with around 400 d period
which(cogr$acf==max(cogr$acf[300:500]))
#372->400 if you look at the shape
## now with fft
pgm<-pwrspc(f.a.anp$centla$area,1)
plot(pgm,type="l") #one peak slightly larger
tpeaks(pgm,2) #427.6111, 769.7000
points(pgm[c(11,19),])
#############weibull median 
dist<-as.numeric(fire.dist$centla)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#0.6891294
#5  ### chamela
plot(f.a.anp$chamela$date,f.a.anp$chamela$area,type="h")
#one single fire
#nothing to do
#6  ## chankin
plot(f.a.anp$chankin$date,f.a.anp$chankin$area,type="h")
#few fires seasonality is weak are is variable
cogr<-acf(f.a.anp$chankin$area,lag.max = 1000)
#two clusters, one at 400 other at 700
which(cogr$acf==max(cogr$acf[300:500]))
#376
which(cogr$acf==max(cogr$acf[600:800]))
#716
### now with fft
pgm<-pwrspc(f.a.anp$chankin$area,1)
plot(pgm,type="l")
#the second most largest peak is 373.42
tpeaks(pgm,3)
points(pgm[c(20,14,11,8,5),])
#############weibull median 
dist<-as.numeric(fire.dist$chankin)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#7  ##chichinautin
plot(f.a.anp$chichinautzin$date,f.a.anp$chichinautzin$area,type="h")
#relatively small number of fires, seasonality is weak
cogr<-acf(f.a.anp$chichinautzin$area,lag.max = 1000)
#two weak clusters, 400 and 700
which(cogr$acf==max(cogr$acf[300:500]))
#359
which(cogr$acf==max(cogr$acf[600:800]))
#720
### now with fft
pgm<-pwrspc(f.a.anp$chichinautzin$area,1)
plot(pgm,type="l")
tpeaks(pgm,3) #374.0
points(pgm[c(4,10,13,20),])
#############weibull median 
dist<-as.numeric(fire.dist$chichinautzin)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#8  ## chuchujaqui
plot(f.a.anp$chuchujaqui$date,f.a.anp$chuchujaqui$area,type="h")
#number of fires is relatively high, area is high, seasonality is weak
cogr<-acf(f.a.anp$chuchujaqui$area,lag.max = 1000)
#two weak clusters, 400 and 700
which(cogr$acf==max(cogr$acf[300:500]))
#430
which(cogr$acf==max(cogr$acf[600:800]))
#733
### now with fft
pgm<-pwrspc(f.a.anp$chuchujaqui$area,1)
plot(pgm,type="l")
tpeaks(pgm,2)#373.6316
points(pgm[c(20,15,12,9,7,5),])
#############weibull median 
dist<-as.numeric(fire.dist$chuchujaqui)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#9 ## cirios
plot(f.a.anp$cirios$date,f.a.anp$cirios$area,type="h")
#few fires, seasonality is bad, area is low
cogr<-acf(f.a.anp$cirios$area,lag.max = 1000)
#one peak at 700 very weak at 400 
which(cogr$acf==max(cogr$acf[700:900]))
#730
which(cogr$acf==max(cogr$acf[200:400]))
#351
### now with fft
pgm<-pwrspc(f.a.anp$cirios$area,1)
plot(pgm,type="l")
tpeaks(pgm,10)#peak rank 21 is 354.55 other large periods are 644.6364 and 787.8889
points(pgm[c(3,8,10,12,21),])
#############weibull median 
dist<-as.numeric(fire.dist$cirios)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#10  ##cuatocienegas
plot(f.a.anp$cuatrocienegas$date,f.a.anp$cuatrocienegas$area,type="h")
#two fires, nothing to do
#11 ## encrucijada
plot(f.a.anp$encrucijada$date,f.a.anp$encrucijada$area,type="h")
#lots of fires, strong seadonality, high area
cogr<-acf(f.a.anp$encrucijada$area,lag.max = 1000)
#two clusters at 400 and 700
which(cogr$acf==max(cogr$acf[300:500]))
#392
which(cogr$acf==max(cogr$acf[600:800]))
#728
### now with fft
pgm<-pwrspc(f.a.anp$encrucijada$area,1)
plot(pgm,type="l")#two well defined peaks
tpeaks(pgm,3)#375.4211. smaller harmonics are probably noise
#they do not match with the pattern of times series or acf
points(pgm[c(20,40),])
#############weibull median 
dist<-as.numeric(fire.dist$encrucijada)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#12 ## gorda
plot(f.a.anp$gorda$date,f.a.anp$gorda$area,type="h")
#few fires, seasonality is weak
cogr<-acf(f.a.anp$gorda$area,lag.max = 1500,ylim=c(-0.05,0.5))
#one strong peak at 1100, other weak at 400, 700 
which(cogr$acf==max(cogr$acf[1000:1200]))
#1128
which(cogr$acf==max(cogr$acf[250:500]))
#291
which(cogr$acf==max(cogr$acf[500:1000]))
#725
### now with fft
pgm<-pwrspc(f.a.anp$gorda$area,1)
plot(pgm,type="l")
#there are three peaks that may be considered, 1182, 591 and 373
points(pgm[c(7,13,20),])
#############weibull median 
dist<-as.numeric(fire.dist$gorda)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#13 ## Guanajuato
plot(f.a.anp$guanajuato$date,f.a.anp$guanajuato$area,type="h")
#one fire, nothing to do
#14  ## huautla
plot(f.a.anp$huautla$date,f.a.anp$huautla$area,type="h")
#three fires
cogr<-acf(f.a.anp$huautla$area,lag.max = 3000)
#one peak
which(cogr$acf==max(cogr$acf[2500:3000]))
#2915
#15  ## janos
plot(f.a.anp$janos$date,f.a.anp$janos$area,type="h")
#moderate number of fires, area very variable
cogr<-acf(f.a.anp$janos$area,lag.max = 1800)
#large peak at 400
which(cogr$acf==max(cogr$acf[300:500]))
#385
which(cogr$acf==max(cogr$acf[500:1000]))
#781
## now with fft
pgm<-pwrspc(f.a.anp$janos$area,1)
plot(pgm,type="l") #418 confirmed, 1185 new 1778 new
points(pgm[c(5,7,20),])
#############weibull median 
dist<-as.numeric(fire.dist$janos)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#16  ## lacantun
plot(f.a.anp$lcantun$date,f.a.anp$lcantun$area,type="h")
#few fires, seasonality is weak
cogr<-acf(f.a.anp$lcantun$area,lag.max = 1500, ylim=c(-0.05,0.2))
#large peak at 1100, two smaller peaks in between
which(cogr$acf==max(cogr$acf[300:500]))
#370
which(cogr$acf==max(cogr$acf[1000:1200]))
#1104
which(cogr$acf==max(cogr$acf[500:1000]))
#734
### now with fft
pgm<-pwrspc(f.a.anp$lcantun$area,1)
plot(pgm,type="l") #three peaks are important 1015, 711 and 374
#only 1015 is significant
points(pgm[c(8,11,20),])
#############weibull median 
dist<-as.numeric(fire.dist$lcantun)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#17  ## madre
plot(f.a.anp$madre$date,f.a.anp$madre$area,type="h")
#many fires, seasonality is weak, fires are in all dates, area is large
cogr<-acf(f.a.anp$madre$area,lag.max = 1500)
#one small peak at 400
which(cogr$acf==max(cogr$acf[300:500]))
#374
## now with fft
pgm<-pwrspc(f.a.anp$madre$area,1)
plot(pgm,type="l",ylim=c(0,10^5))
points(pgm[c(18,15),])
#there is a lot of noise in the acf. the fft shows one larger peak at 423
#############weibull median 
dist<-as.numeric(fire.dist$madre)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#18  ## manantlan
plot(f.a.anp$manantlan$date,f.a.anp$manantlan$area,type="h")
#moderate number of fires, seasonality is good, area had a peak
cogr<-acf(f.a.anp$manantlan$area,lag.max = 2000)
#one peak at 400
which(cogr$acf==max(cogr$acf[300:500]))
#368
## now with fft
pgm<-pwrspc(f.a.anp$manantlan$area,1)
plot(pgm,type="l")#one significant peak with harmonic larger than one year
#the peak with period shorter than one year is considered noise
#423 at postion 18
tpeaks(pgm,2)
points(pgm[c(6,14,18),])
#############weibull median 
dist<-as.numeric(fire.dist$manantlan)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#19  ## mapimi
plot(f.a.anp$mapimi$date,f.a.anp$mapimi$area,type="h")
#one fire, nothing to do
#20  ## micilia
plot(f.a.anp$michilia$date,f.a.anp$michilia$area,type="h")
#one fire, nothing to do
#21  ## monarca
plot(f.a.anp$monarca$date,f.a.anp$monarca$area,type="h")
#number of fires is moderate, seasonality is weak
cogr<-acf(f.a.anp$monarca$area,lag.max = 2000)
#one cluster at 800, a small one at 400
which(cogr$acf==max(cogr$acf[300:500]))
#410
which(cogr$acf==max(cogr$acf[500:1000]))
#749
## now with fft
pgm<-pwrspc(f.a.anp$monarca$area,1)
plot(pgm,type="l")
points(pgm[c(4,8,11,19),])
#taking into account all peaks with harmonics larger than 1 year
#2363.7, 1013.0000, 709.1000, 393.9444, other are not significant
#22 montes azules
plot(f.a.anp$montesazules$date,f.a.anp$montesazules$area,type="h")
#moderate number of fires, seasonality is ok, areas are very variable
cogr<-acf(f.a.anp$montesazules$area,lag.max = 1500)
#peak at 400
which(cogr$acf==max(cogr$acf[300:500]))
#372
## now with fft
pgm<-pwrspc(f.a.anp$montesazules$area,1)
plot(pgm,type="l")#
points(pgm[c(3,6,9,11,19),])
#there are several peaks, the largest may be confounded. 
#399.3333, 718.80, 1437.6. 898.5000, 3594.0 is not evident in the acf
#############weibull median 
dist<-as.numeric(fire.dist$montesazules)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#23  ## nayarit
plot(f.a.anp$nayarit$date,f.a.anp$nayarit$area,type="h")
#small number of fires, seasonality is ok, area is variable
cogr<-acf(f.a.anp$nayarit$area,lag.max = 1100)
#one peak at 700 no peaks in the middle
which(cogr$acf==max(cogr$acf[600:800]))
#725
## now with fft
pgm<-pwrspc(f.a.anp$nayarit$area,1)
plot(pgm,type="l")
points(pgm[c(4,8,13,20),])#373.2632NS, 591.0NS, 1013.1429 2364.0
#############weibull median 
dist<-as.numeric(fire.dist$nayarit)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#24  ## ocampo
plot(f.a.anp$ocampo$date,f.a.anp$ocampo$area,type="h")
#one large fire, few others, seasonality is bad, maybe noth
#enough to compute a periodicity because it misses another large fire
cogr<-acf(f.a.anp$ocampo$area,lag.max = 1000)
#one weak peak at ; 400 
which(cogr$acf==max(cogr$acf[300:500]))
#340
#now with fft
pgm<-pwrspc(f.a.anp$ocampo$area,1)
plot(pgm,type="l")
points(pgm[c(5,16,20),])
#############weibull median 
dist<-as.numeric(fire.dist$ocampo)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#25  #ocote
plot(f.a.anp$ocote$date,f.a.anp$ocote$area,type="h")
#number of fires is ok, seasonality is good, area is large
cogr<-acf(f.a.anp$ocote$area,lag.max = 1800)
#one weak at 400 and 800, seems ok
which(cogr$acf==max(cogr$acf[300:500]))
#358
which(cogr$acf==max(cogr$acf[600:800]))
#743
##now with fft
pgm<-pwrspc(f.a.anp$ocote$area,1)
plot(pgm,type="l")
points(pgm[c(5,7,11,20),])
#374.8421, 712.2000, 1187.0, larger harmonics do not match acf
#############weibull median 
dist<-as.numeric(fire.dist$ocote)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#26  ##riacelestun
plot(f.a.anp$riacelestun$date,f.a.anp$riacelestun$area,type="h")
#very few fires, seasonality is bad, area is variable
cogr<-acf(f.a.anp$riacelestun$area,lag.max = 2000)
#one large peak at 1400 and two weak at 400a nd 1100
which(cogr$acf==max(cogr$acf[300:500]))
#362
which(cogr$acf==max(cogr$acf[1000:1100]))
#1061
which(cogr$acf==max(cogr$acf[1100:1500]))
#1422
##now with fft
pgm<-pwrspc(f.a.anp$riacelestun$area,1)
plot(pgm,type="l")
points(pgm[c(7,17,20),])#two overlapping harmonics 373.2105, 443.1875
#1181.8333 matches the acf
#############weibull median 
dist<-as.numeric(fire.dist$riacelestun)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#26 ## rialagartos
plot(f.a.anp$rialagartos,type="h")
#number of fires is ok seasonality is ok area is moderate
cogr<-acf(f.a.anp$rialagartos$area,lag.max = 1500)
#300 is relatively large. toeher are small probably 1400
which(cogr$acf==max(cogr$acf[100:500]))
#292
which(cogr$acf==max(cogr$acf[1000:1500]))
#1395
which(cogr$acf==max(cogr$acf[500:1000]))
#828
#maybe there is a double seasonality
##now with fft
pgm<-pwrspc(f.a.anp$rialagartos$area,1)
plot(pgm,type="l")
points(pgm[c(5,7,20),])#373.4211, 1182.5
#############weibull median 
dist<-as.numeric(fire.dist$rialagartos)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#27 ## sepultura
plot(f.a.anp$sepultura,type="h")
#nuber of fires is large, seasonality is good, area is variable
cogr<-acf(f.a.anp$sepultura$area,lag.max = 1500)
#marked 400 seasonalith
which(cogr$acf==max(cogr$acf[300:500]))
#368
##now with fft
pgm<-pwrspc(f.a.anp$sepultura$area,1)
plot(pgm,type="l")
points(pgm[c(7,12,18,20),])#one of the peaks do not correspond 654.5455 with acf
#378.9474 , 423.5,  654.5455 ->800, 1200.0 is weak
#############weibull median 
dist<-as.numeric(fire.dist$sepultura)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#28  ## siankaan
plot(f.a.anp$siankaan,type="h")
#number of fires is small, seasonality is bad, area is variable
cogr<-acf(f.a.anp$siankaan$area,lag.max = 1500)
#one peak near 1000
which(cogr$acf==max(cogr$acf[200:500]))
#337 weak
which(cogr$acf==max(cogr$acf[500:1000]))
#947
#now with fft
pgm<-pwrspc(f.a.anp$siankaan$area,1)
plot(pgm,type="l")
points(pgm[c(3,8,15),])
#############weibull median 
dist<-as.numeric(fire.dist$siankaan)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#29  ## snpedro
plot(f.a.anp$snpedro,type="h")
#number of fires small, seasonality is bad area is large
cogr<-acf(f.a.anp$snpedro$area,lag.max = 1500)
#small peak 300, small 1400 
which(cogr$acf==max(cogr$acf[200:500]))
#############weibull median 
dist<-as.numeric(fire.dist$snpedro)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
which(cogr$acf==max(cogr$acf[1200:1500]))
#1382
#now with fft
pgm<-pwrspc(f.a.anp$snpedro$area,1)
plot(pgm,type="l")
points(pgm[c(6,12,17,22),])
#30  ##staelena
plot(f.a.anp$staelena,type="h")
#only one fire, nothing to do
#31  ## tehuacan
plot(f.a.anp$tehuacan,type="h")
#number of fires is ok, seasonality is ok
cogr<-acf(f.a.anp$tehuacan$area,lag.max = 1500)
#large at 1100, small 400 700 1400
which(cogr$acf==max(cogr$acf[300:500]))
#366
which(cogr$acf==max(cogr$acf[500:1000]))
#731
which(cogr$acf==max(cogr$acf[1000:1500]))
#1097
##now with fft
pgm<-pwrspc(f.a.anp$tehuacan$area,1)
plot(pgm,type="l")
points(pgm[c(8,14,20),])
#373.3684, 545.6923->731, 1013.4286
#############weibull median 
dist<-as.numeric(fire.dist$tehuacan)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#failed
#32  ## terminos
plot(f.a.anp$terminos,type="h")
hst.ter<-hist(f.a.anp$terminos$area,breaks=10)
#hst.ter$counts<-log(hst.ter$counts)
#hst.ter$counts[hst.ter$counts==-Inf]<-0
plot(hst.ter,main="",xlab="Area",ylab="Log Frequency")
#number of fires is large, seasonality is great, area is large
cogr<-acf(f.a.anp$terminos$area,lag.max = 1000)
##400
which(cogr$acf==max(cogr$acf[300:500]))
#365
##now with fft
pgm<-pwrspc(f.a.anp$terminos$area,1)
plot(pgm,type="l")
points(pgm[c(11,20),],col="red")
#411.2105, 781.3000
#############weibull median 
dist<-as.numeric(fire.dist$terminos)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#33
plot(f.a.anp$triunfo,type="h")
#number of fires is ok, sesonality is ok area is moderate
cogr<-acf(f.a.anp$triunfo$area,lag.max = 2000,ylim=c(-0.05,0.2))
#lots of noise, one large peak 800 smaller at 400
which(cogr$acf==max(cogr$acf[200:500]))
#365
which(cogr$acf==max(cogr$acf[500:1000]))
#751
##now with fft
pgm<-pwrspc(f.a.anp$triunfo$area,1)
plot(pgm,type="l")
points(pgm[c(5,7,11,21),])
#354.700, 709.400, 1182.333 not match, 1773.500 not match
#34  # tuxtlas
plot(f.a.anp$tuxtlas,type="h")
#number of fires is ok, seasonality is ok, area is large
cogr<-acf(f.a.anp$tuxtlas$area,lag.max = 1500,ylim=c(-0.05,0.2))
#marked seasonality with noise 400 larger peak 900
which(cogr$acf==max(cogr$acf[200:500]))
#484
which(cogr$acf==max(cogr$acf[500:1000]))
#803
## now with fft
pgm<-pwrspc(f.a.anp$tuxtlas$area,1)
plot(pgm,type="l")
points(pgm[c(8,11,16,20),])
#374.2105, 474.0000 probably a continuum. 711.0 , 1015.7143 this one not match
#############weibull median 
dist<-as.numeric(fire.dist$tuxtlas)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#35  ## uaymil
plot(f.a.anp$uaymil,type="h")
#number of fires is small, seasonality is minimal
#maybe is ok, but only repeats once
cogr<-acf(f.a.anp$uaymil$area,lag.max = 1500,ylim=c(-0.05,0.1))
#onle larg at 1300, very small at 700 
which(cogr$acf==max(cogr$acf[500:1000]))
#773
which(cogr$acf==max(cogr$acf[1000:1500]))
#1327
## now with fft
pgm<-pwrspc(f.a.anp$uaymil$area,1)
plot(pgm,type="l")
points(pgm[c(6,8,11,13,16,18,23),])
#peaks are almost the same in heigth
#709.200, 1013.143 not match, 1418.4
#############weibull median 
dist<-as.numeric(fire.dist$uaymil)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#36
plot(f.a.anp$vallebravo,type="h")
#number of fires is ok, seasonality is good, area is small
cogr<-acf(f.a.anp$vallebravo$area,lag.max = 2000)
#larger peak at 700, smaller at 300
which(cogr$acf==max(cogr$acf[300:500]))
#380
which(cogr$acf==max(cogr$acf[600:800]))
#744 maybe is biennial with a smaller annual component
#now with fft
pgm<-pwrspc(f.a.anp$vallebravo$area,1)
plot(pgm,type="l")
points(pgm[c(5,11,20),])
#374.3684, 711.3, 1778.25
#############weibull median 
dist<-as.numeric(fire.dist$vallebravo)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
#37  #vizcaino
plot(f.a.anp$vizcaino,type="h")
#only one fire, nothing to do
#38  ## zicuiran
plot(f.a.anp$zicuiran,type="h")
#number of fires is ok, seasonality is ok, area is medium
cogr<-acf(f.a.anp$zicuiran$area,lag.max = 1000)
##400 weak, and noise
which(cogr$acf==max(cogr$acf[300:500]))
#353
###now with fft
pgm<-pwrspc(f.a.anp$zicuiran$area,1)
plot(pgm,type="l")
points(pgm[c(11,20),])
#373.8947 , 710.4
#############weibull median 
dist<-as.numeric(fire.dist$zicuiran)
dist[dist==0]<-0.1
ftdst<-fitdistr(dist,densfun="weibull",start=list(shape=0.5,scale=1))
ftdst$estimate[2]*(logb(2))^(1/ftdst$estimate[1])#optimization failed
