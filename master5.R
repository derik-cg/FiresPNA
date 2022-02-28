#this is a fifth pass on all the fire data for all years
#the objective of this script is to make a time series
#that is preliminary. This ts has the number of pixels that
#show fire for all the clipped and masked geotiffs.

rm(list=ls())
gc()

require(raster)
require(stringr)
# 1 change to the root directory for the analysis
root<-"C:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos/"

# 2 get the list of all directories within
#number of anps tif fils the key word is anps
year.list<-list.dirs(path=root,recursive=T,full.names=F)
year.list<-year.list[2:length(year.list)]

#list of all geotiffs clipped and masked

name.list<-list.files(path=root,pattern="anps.+tif",recursive=T)

#now prepare for the data frame. put the following 
#columns 1) full name 2) year 3) julian day
# number of burned pixels

tot<-length(name.list)
name<-rep("a",tot)
code<-rep(0,tot)
year<-rep(0,tot)
day<-rep(0,tot)
pixels<-rep(0,tot)

#loop
for (i in 1:tot){
  print(c(i,i/tot))
  #extract the parts from the name
  #first remove the slash
  tmp<-strsplit(name.list,"/")
  #extract the second part of the first element
  tmp<-tmp[[1]][2]
  #separate by underscores
  tmp<-strsplit(tmp,"_")
  name[i]<-tmp[[1]][3] #assign
  code[i]<-tmp[[1]][2] #assign
  ## separate the name by the dot
  tmp<-tmp[[1]][4]
  tmp<-strsplit(tmp,"\\.")
  tmp<-tmp[[1]][1]
  #extract year
  year[i]<-substring(tmp,2,5) #assign
  day[i]<-substring(tmp,6,8) #assign
  #read the geotiff
  rst.loop<-raster(paste(root,name.list[i],sep=""))
  #select only pixels greater than zero and count them
  pixels[i]<-sum(values(rst.loop)>2,na.rm=T)
}
allpix<-data.frame(name=name,code=code,year=year,day=day,
                   pixels=pixels)
save.image("C:/Users/Derik/Google Drive/Investigacion/fuegos/proyecto/anpixels.RData")
