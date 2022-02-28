#master script for checking the whole country for fires
#This script gives a first review to the clipped geotiffs
#to check how many pixels are burnt. Previous versions gave
#the result that all geotiff have fires.
#This script was written by Derik Castillo on 20 march 2020 for the
#fires proyect

require(raster)
require(rgdal)
require(sf)
require(sp)

# 1 change to the root directory for the analysis
root<-"C:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos"
# 2 get the list of all directories within
#number of anps tif fils
tot.full.list<-list.files(path=root,pattern="burndate",recursive=T,full.names=T)
tot.list<-list.files(path=root,pattern="burndate",recursive=T,full.names=F)
#use the total list with partial path to build the name vector
#and use this for naming the output
nm.lst<-strsplit(tot.list,"/")
nm.lst<-lapply(nm.lst,'[[',2)
nm.lst<-unlist(nm.lst)
nm.lst<-strsplit(nm.lst,"[.]")
nm.lst<-lapply(nm.lst,'[[',2)
num.tifs<-length(tot.list)

#create a table for the tifs with fires
nombres<-rep("a",num.tifs)
fuegos<-rep(NA,num.tifs)
#loop
for (i in 1:num.tifs){ #year loop
  print(c("tif=",tot.list[i]))
  rst.loop<-raster(paste(root,tot.list[i],sep="/")) #load the raster
  #now see if the raster has fires
  nombres[i]<-nm.lst[i]
#this version checks wether there are fires or not
#  if(length(which(values(rst.loop)>0))!=0){ 
#    fuegos[i]<-1  #fire
#    print("1")
#  }
  ln<-length(which(values(rst.loop)>2))
  if(ln!=0){
    fuegos[i]<-ln
    print("1")
  }
  else{  
    fuegos[i]<-0     #write a 0 meaning no fire
    print("0")
  }
  rm(rst.loop) 
}

tif.fire<-data.frame(name=unlist(nombres),fire=unlist(fuegos),
                     stringsAsFactors = F)
save(tif.fire,file="C:/Users/Derik/Google Drive/Investigacion/fuegos/firestif.RData")
