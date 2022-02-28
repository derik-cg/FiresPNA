#this is a fourth pass in the fire projct
#This script was written by Derik Castillo on 1 april 2020
#This pass takes one clipped and masked raster file
#and vectorizes it.
########
#edit after struggling with the fail approach described
#above. This file takes the clipped and masked raster.
#then computes clumps using raster::clump. Then the
#clumps are vectorized. Saved in the same .RData

rm(list=ls())
gc()

require(raster)
require(rgdal)
require(sf)
require(sp)
require(dplyr)

# 1 change to the root directory for the analysis
root<-"C:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos/"

# 2 get the list of all directories within
#number of anps tif fils the key word is anps
year.list<-list.dirs(path=root,recursive=T,full.names=F)
year.list<-year.list[2:length(year.list)]
#tot.full.list<-list.files(path=root,pattern="anps",recursive=T,full.names=T)
#tot.list<-list.files(path=root,pattern="anps",recursive=T,full.names=F)
#now break the tot.list to get only the name (without the year)
#nm.lst<-strsplit(tot.list,"/")
#nm.lst<-lapply(nm.lst,'[[',2)
#nm.lst<-unlist(nm.lst)
#nm.lst<-strsplit(nm.lst,"[.]")
#nm.lst<-lapply(nm.lst,'[[',1)
#nm.lst<-unlist(nm.lst)
#num.tifs<-length(tot.list)
#the idea is to use the same number and just add a marker for vectorized
#version of each raster. Save as RData file. inside look for vct.rst that
#stands for vectorized raster

#meanwhile, make a list of all of the clipped and masked geotiffs with burnt pixels
#total size of list
total<-length(list.files(path=root,pattern="anps.+tif",recursive=T))
burnt<-rep(0,total)
names<-rep("a",total)
counter<-0

#loop by the tot.full.list
for (i in year.list){
  print(i)
  #change directory
  setwd(paste(root,i,sep=""))
  #read files that match "anps"
  lcl.lst<-list.files(path=".",pattern="anps.+tif")#geotiff file list
  #now make another list with the name but without the extension
  lcl.lst.nm<-strsplit(lcl.lst,"[.]")
  lcl.lst.nm<-lapply(lcl.lst.nm,'[[',1)
  lcl.lst.nm<-unlist(lcl.lst.nm)#geotif without extension
  num.lcl<-length(lcl.lst.nm) #size of local file list
  for (j in 1:num.lcl){
    #advance a counter
    counter<-counter+1
    #read the geotiff
    rst.loop<-raster(lcl.lst[j])
    mx<-max(values(rst.loop),na.rm=T)
    print(c(j,mx,counter))
    #here is where I can build a simple data frame
    if(mx>0){ # only if fires are present
      #save path and raster filename
       #    root i lcl.lst.nm .tif
      ref.tif<-paste(root,i,"/",lcl.lst[j],sep="")
      #print(ref.tif)
      #compute clumps
      clmp<-clump(rst.loop>0)
      #get name of vectorized file
      nm0<-lcl.lst.nm[j]
      nm<-paste("vct",as.character(lcl.lst.nm[j]),"RData",sep="_")
      #vectorize the clumps
      vct.rst<-rasterToPolygons(clmp) %>%
        st_as_sf %>% group_by(clumps) %>% summarize()
      #save the file
      save(vct.rst,ref.tif,file=nm)
      #clear the temporary files rst.loop and ply.loop
      rm(rst.loop,vct.rst)
      burnt[counter]<-1
      names[counter]<-lcl.lst.nm[j]
    }
  }
  gc()
}
burnt.anps<-data.frame(names=names,burnt=burnt)
save(burnt.anps,file="C:/Users/Derik/Google Drive/Investigacion/fuegos/proyecto/burntansps.RData")
###
