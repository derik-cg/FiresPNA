#master script
#this script is for processing the burndate rasters using the
#shapefiles for the natural protected areas in mexico
#This script was modified on 19 may 2020 to improve file naming
#to make it friendly for posterior scripts

require(raster)
require(rgdal)
require(sf)
require(sp)

source("C:/Users/Derik/Google Drive/Investigacion/fuegos/proyecto/ext.rst.R")

anp44<-c(4,10,16,25,26,30,39,43,56,57,59,69,72,73,77,79,86,89,90,92,93,
         94,95,102,110,114,115,123,127,130,131,132,133,134,136,138,139,
         140,141,142,143,144,145,167)
nom44<-c("balaan","staelena","calakmul","chamela","chankin","chichinautzin",
"pinacate","vizcaino","janos","michilia","lcantun","carmen","mapimi",
"nayarit","montesazules","ocampo","rialagartos","chuchujaqui","huautla",
"snpedro","tanchipa","guanajuato","laguna","cirios","zicuiran",
"gorda","monarca","cuatrocienegas","triunfo","encrucijada","sepultura",
"terminos","madre","riacelestun","tuxtlas","meztitlan","centla","ocote",
"siankaan","manantlan","tehuacan","uaymil","vallebravo","ojoliebre")
 count<-0
# 1 change to the root directory for the analysis
root<-"D:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos"
# 2 the absolute path to the shapefile is 
pt.shp<-"D:/Google Drive/Investigacion/fuegos/conanp/shapefile/SHAPE_ANPS"
# the shapefile is
nm.shp<-"182ANP_Geo_ITRF08_Julio04_2019.shp"
# 3 load the shapefile. this is for all the rasters
shpo<-readOGR(paste(pt.shp,nm.shp,sep="/"))
#check the projection should contain WGS84
tst<-projection(shpo)
if(grepl("WGS84",tst)!=1){
  shpo<-spTransform(shpo,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
}
# 2 get the list of all directories within
years<-list.dirs(path=root,recursive=TRUE,full.names=T)
#remove the first, since it is just the root
years<-years[2:length(years)]
# 4 loop through directories
for (i in years){#year loop
  print(c("year=",i))
  setwd(i) # go to the current directory
  rm(list=ls()[grep("anps",ls())]) #first clear all processed rasters
  gc()
  rst.f<-list.files(path=".",pattern="burndate")
  num.r<-length(rst.f) #number of rasters within dir
  rst.nm<-strsplit(rst.f,"[.]")
  for (j in 1:num.r){#month loop
    print(c("month=",j))
    name<-rst.nm[[j]][2] #this is the year and "month" use for naming
    #load the raster
    rst.loop<-raster(rst.f[j])
    for (k in 1:44){#anp loop
      print(c("anp=",k))
      rw<-anp44[k]#number of rows
      nmcut<-paste("anps",anp44[k],nom44[k],name,sep="_")
      assign(nmcut,#name object
             ext.rst(rst.loop,shpo[rw,]))#clip and mask
      writeRaster(eval(as.name(nmcut)), #object in memory
                  filename=nmcut,
                  format="GTiff",
                  overwrite=T)
      count<-count+1
      rm(list=ls()[grep("anps",ls())])#clear only rasters form other months
      gc()
      
    }
  }
}
