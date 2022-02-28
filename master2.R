#master script for second pass
#This script gives a first review to the clipped geotiffs
#to check if they have fires

require(raster)
require(rgdal)
require(sf)
require(sp)

# 1 change to the root directory for the analysis
root<-"C:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos"
# 2 get the list of all directories within
#number of anps tif fils
tot.full.list<-list.files(path=root,pattern="anps.+tif",recursive=T,full.names=T)
tot.list<-list.files(path=root,pattern="anps.+tif",recursive=T,full.names=F)
#use the total list with partial path to build the name vector
#and use this for naming the output
nm.lst<-strsplit(tot.list,"/")
nm.lst<-lapply(nm.lst,'[[',2)
nm.lst<-unlist(nm.lst)
nm.lst<-strsplit(nm.lst,"[.]")
nm.lst<-lapply(nm.lst,'[[',1)
num.tifs<-length(tot.list)

#create a table for the tifs with fires
nombres<-rep("a",num.tifs)
fuegos<-rep(NA,num.tifs)
#loop
for (i in 1:num.tifs){ #year loop
  print(c("tif=",i))
  rst.loop<-raster(paste(root,tot.list[i],sep="/")) #load the raster
  #now count only if the raster has fires
  nombres[i]<-nm.lst[i]
  if(max(values(rst.loop),na.rm=T)>2) fuegos[i]<-1 #fire
  else  fuegos[i]<-0     #write a 0 meaning no fire
  rm(rst.loop)
}
vecnom<-lapply(nombres,'[[',1)
tif.fire<-data.frame(name=unlist(vecnom),fire=fuegos,
                     stringsAsFactors = F)
save(tif.fire,file="C:/Users/Derik/Google Drive/Investigacion/fuegos/proyecto/fires01.RData")
#total number of geotiffs is
num.tifs
#total numbet of geotifs with fire #1162
tot.fire<-sum(tif.fire$fire)
#the proportion of tiffs with fires
tot.fire/num.tifs # 0.1143251
