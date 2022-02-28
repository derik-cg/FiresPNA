#this script is the sixth pass on the fires project
#the objective of this script is to obtain the area of each
#and every polygon in the .RData files. That is in the vectorized burnt areas

rm(list=ls())
gc()

#load packages
require(sf)
#require(lwgeom)
require(lubridate)
require(raster)

# 1 change to the root directory for the analysis
root<-"D:/Google Drive/Investigacion/fuegos/conanp/fuegos"

#names of all rdata files

name.vct<-list.files(path=root,pattern="anps.+RData",recursive=T)
num.tot<-length(name.vct)
#prepare for a data.frame with the output data.
#1) anp name
#2) anp code
#3) year
#4) julian day
#5) date format
#6) polygon number according to layer
#7) area

#the total number of polygons is uncertain

name<-"a"
code<-0
year<-0
jul.min<-0
jul.max<-0
jul.med<-0
date<-as.Date("2000-01-01")
polygon<-0
area<-0

#break name.list
nm<-strsplit(name.vct,"\\/")
#extract second part o first element
nm<-lapply(nm,'[[',2)
nm<-unlist(nm)
#separate by underscores
nm<-strsplit(nm,"_")
#now get the name, fourth position
nm.loop<-lapply(nm,'[[',4)
nm.loop<-unlist(nm.loop)
#now get code, third positon
cd.loop<-lapply(nm,'[[',3)
cd.loop<-unlist(cd.loop)
#now get the year. fifth position
yrdt.loop<-lapply(nm,'[[',5)
yrdt.loop<-unlist(yrdt.loop)
#now extract the year
yr.loop<-substring(yrdt.loop,2,5)
#now extract the day
dt.loop<-substring(yrdt.loop,6,8)
rw.id<-0 #initialize counter for id de renglon
for (i in 1:num.tot){
  #load the data
  load(paste(root,name.vct,sep="")[i])
  # load the raster
  rst<-raster(ref.tif)
  #an object vct.rst is loaded
  #get number of polygons
  nm.p<-length(vct.rst[[1]])
  # #these numbers refer to ID of clumps in the original raster file. 
  # #those that are greater than zero
  # dt.use<-which(vct.rst[[1]]>0)
  #  print(nm.p)
  #find the number of polygons
  for (j in 1:nm.p){
    rw.id<-rw.id+1
    name[rw.id]<-nm.loop[i]
    code[rw.id]<-cd.loop[i]
    year[rw.id]<-yr.loop[i]
    julian<-unlist(extract(rst,vct.rst)[j])
    jul.min[rw.id]<-min(julian)
    jul.max[rw.id]<-max(julian)
    jul.med[rw.id]<-median(julian)
    area[rw.id]<-st_area(vct.rst[[2]][j])
    print(c(area[rw.id],i,j))
  }
}
  fire.area<-data.frame(name=name,code=code,year=year,julian.min=jul.min,
                        julian.max=jul.max,julian.median=jul.med,area=area)
  save(fire.area,file="C:/Users/Derik/Google Drive/Investigacion/fuegos/conanp/fuegos/proyecto")
#now compute date from year and julian
#first create a new column with origins
fire.area$origin<-as.Date(paste(fire.area$year,"01","01",sep="-"),format="%Y-%m-%d")
#now compute the date in each line using the origin and the julian date
fire.area$date<-ymd(fire.area$origin+fire.area$julian.min)
#now extract month
fire.area$month<-month(fire.area$date)
#save it again
save.image("C:/Users/Derik/Google Drive/Investigacion/fuegos/proyecto/fire.area.RData")
