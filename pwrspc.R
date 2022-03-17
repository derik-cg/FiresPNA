#this function computes the fast fourier transform of a time series
#The fft is then translated to the frequency and time domain by computing
#the harmonics and the magnitude.
pwrspc<-function(ts,delta){
  #first get the length of the ts
  n<-length(ts)
  #compute the nyquist number
  nyq<-floor(n/2)
  #now get the harmonics
  hrm<-(n*delta)/0:(nyq-1)
  #compute the fft
  ts.fft<-fft(ts)
  #compute the magnitude
  mgn<-sqrt(Re(ts.fft[1:nyq])^2+Im(ts.fft[1:nyq])^2)*2/nyq
  #the periodogram is a data.frame with the harmonics and magnitude
  prgm<-data.frame(harmonics=hrm,magnitude=mgn)
  return(prgm)
}