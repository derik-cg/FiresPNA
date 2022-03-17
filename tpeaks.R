#this script finds peaks and then returns the top n peaks
#this takes as an input harmonics and magnitudes in a data.frame
tpeaks<-function(fout,p=3){
  n<-length(fout[,2]) #size of the periodogram
  peaks<-c() #initialize output structure
  for (i in 2:(n-1)){
    if( (fout[(i-1),2]<fout[i,2])&(fout[i,2]>fout[(i+1),2]) ) {
      pk<-i #this is the row
      mgn<-fout[i,2] #magnitude is to order, but i want the rows
      pair<-c(pk,mgn)
      peaks<-rbind(peaks,pair)#accumulate the pairs
    }
  }
  #now order the peaks by magnitude. I want the rows
  s.p<-peaks[order(peaks[,2],decreasing=T),][1:p,]
  #now use the rows to get the periods (from harmonics)
  pers<-fout[s.p[,1],]
  return(pers)
}