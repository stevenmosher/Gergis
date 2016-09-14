 
###############################

####### Install packages
 
 
if(!"R.utils"     %in% rownames(installed.packages()))install.packages("R.utils")  
if(!"dplyr"       %in% rownames(installed.packages()))install.packages("dplyr") 
if(!"ggplot2"     %in% rownames(installed.packages()))install.packages("ggplot2") 
if(!"raster"      %in% rownames(installed.packages()))install.packages("raster")
if(!"rasterVis"   %in% rownames(installed.packages()))install.packages("rasterVis") 
if(!"zoo"         %in% rownames(installed.packages()))install.packages("raster") 
if(!"ncdf4"       %in% rownames(installed.packages()))install.packages("ncdf4") 
if(!"stringr"     %in% rownames(installed.packages()))install.packages("stringr") 
if(!"tidyr"       %in% rownames(installed.packages()))install.packages("tidyr") 
if(!"broom"       %in% rownames(installed.packages()))install.packages("broom") 
if(!"maps"        %in% rownames(installed.packages()))install.packages("maps") 


 

#######  Load libraries
 
library(tidyr)
library(broom)
library(R.utils)
library(dplyr)
library(ggplot2)
library(rasterVis)
library(raster)
library(zoo)
library(ncdf4)
library(stringr)
library(maps)


 

#  Quick and dirty function for extracting data from  ncdf files for all locations
#  and build a time series 

GenerateTimeSeries <- function(ncfile,location=cbind(ProxyMetadata$long,ProxyMetadata$lat),Name=ProxyMetadata$id){
  
    varname <-  "temp"
    start<-1850
     
    Anomaly <- brick(ncfile,varname=varname)
    ##  use  raster::extract because of library name space conflict depends on library load sequence
    E <- raster::extract(Anomaly,location,layer=1,nl=nlayers(Anomaly))
    E <- t(E)
    E <- ts(E, start=start, frequency = 12)
    colnames(E)<-Name
    
  return(E)
  
}

TidySeries <- function(Base=NULL,New,Source){
  
  
  DF <- tbl_df(data.frame(Date=time(New),Source=Source ,stringsAsFactors=FALSE))
  DF <- cbind(DF ,coredata(New)) %>% tbl_df() %>%gather(Site,InstrumentSeries,starts_with("X"))
  
  if(!is.null(Base))DF<-bind_rows(Base,DF)
  return(DF)
  
}


 



####     Climateaudit File names

Gergisloc  <- "http://www.climateaudit.info/data/multiproxy/gergis_2016/info_gergis_2016.csv"
GergisProx <- "http://www.climateaudit.info/data/multiproxy/gergis_2016/proxy_gergis_2016.tab"
download.file(GergisProx,"Gergis.tab",mode="wb")
load("Gergis.tab")
##  obj is a  mts  loaded as proxy
##  make it tidy
Proxy <- data.frame(Date=as.numeric(time(proxy)),stringsAsFactors=FALSE)
Proxy <-  cbind(Proxy ,coredata(proxy)) %>%tbl_df()
Proxy <- Proxy %>% gather(Site,Value,starts_with("X"))


ProxyMetadata <- tbl_df(read.csv(Gergisloc,stringsAsFactors = FALSE))

###    Current Hadley and crutem

http_hadcrut3v <-"https://crudata.uea.ac.uk/cru/data/crutem3/HadCRUT3v.nc"
http_hadcrut3  <-"https://crudata.uea.ac.uk/cru/data/crutem3/HadCRUT3.nc"
http_crutem3   <-"https://crudata.uea.ac.uk/cru/data/crutem3/CRUTEM3.nc"
http_crutem3v  <-"https://crudata.uea.ac.uk/cru/data/crutem3/CRUTEM3v.nc"

####  crutem archive
ct2012Nov   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-11/crutem3-2012-11.nc.gz"
ct2012Aug   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-08/crutem3-2012-08.nc.gz"
ct2012Jan   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-01/crutem3-2012-01.nc.gz"
ct2012Feb   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-02/crutem3-2012-02.nc.gz"
ct2012Mar   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-03/crutem3-2012-03.nc.gz"
ct2012Apr   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-04/crutem3-2012-04.nc.gz"

###  hadcrut  2012  May
 
hadcrut3v2012 <-"http://www.mediafire.com/download/78f6wx4z7qb1itg/HadCRUT3v-5-18-2012.nc"

#####
Downloads  <- "Downloads"

if(!file.exists(Downloads))dir.create(Downloads)



#################################  Download
 

download.file(url=http_hadcrut3v,destfile=file.path(Downloads,basename(http_hadcrut3v)),mode="wb")
download.file(url=http_hadcrut3,destfile=file.path(Downloads,basename(http_hadcrut3)),mode="wb")
download.file(url=http_crutem3v,destfile=file.path(Downloads,basename(http_crutem3v)),mode="wb")
download.file(url=http_crutem3,destfile=file.path(Downloads,basename(http_crutem3)),mode="wb")

#Archived  crutem

download.file(url= ct2012Nov,destfile=file.path(Downloads,basename(ct2012Nov)),mode="wb")
download.file(url= ct2012Aug,destfile=file.path(Downloads,basename(ct2012Aug)),mode="wb")
download.file(url= ct2012Jan,destfile=file.path(Downloads,basename(ct2012Jan)),mode="wb")
download.file(url= ct2012Feb,destfile=file.path(Downloads,basename(ct2012Feb)),mode="wb")
download.file(url= ct2012Mar,destfile=file.path(Downloads,basename(ct2012Mar)),mode="wb")
download.file(url= ct2012Apr,destfile=file.path(Downloads,basename(ct2012Apr)),mode="wb")


# Archived   hadcrut3v   may 2012 
#  rename it
 
##############  Unpack gzips
gzips <- list.files(Downloads,full.name=T, pattern= "gz")
destnames <- str_replace(string=gzips,pattern=".gz",replace="")
mapply(gunzip,filename=gzips,destname=destnames)


###  Get Filenames
NCDF_Files <- list.files(Downloads,full.names=TRUE, pattern=".nc")

 

####   Create a  tidy tbl df of all versions of crutem and hadcrut

AllSeries        <-TidySeries(Base=NULL,New=GenerateTimeSeries(NCDF_Files[grepl(basename(http_crutem3),NCDF_Files)]),
                              Source=basename(http_crutem3))

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl(basename(http_crutem3v),NCDF_Files)]),
                              Source=basename(http_crutem3v))

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl(basename(http_hadcrut3),NCDF_Files)]),
                              Source=basename(http_hadcrut3))
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl(basename(http_hadcrut3v),NCDF_Files)]),
                              Source=basename(http_hadcrut3v))
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-01.nc",NCDF_Files)]),
                              Source="crutem3-2012-01.nc")

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-02.nc",NCDF_Files)]),
                              Source="crutem3-2012-02.nc")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-03.nc",NCDF_Files)]),
                              Source="crutem3-2012-03.nc")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-04.nc",NCDF_Files)]),
                              Source="crutem3-2012-04.nc")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-08.nc",NCDF_Files)]),
                              Source="crutem3-2012-08.nc") 
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-11.nc",NCDF_Files)]),
                              Source="crutem3-2012-11.nc") 

 

###   plot law dome anomalies for all series
##  show how ggplot works with tidy data

ggplot(filter(AllSeries,Site=="X588_Law_Dome_18O_new"), aes(x=Date,y=InstrumentSeries, color=Source))+geom_line() +xlim(1960,2000)

###  How does old crutem compare to new?
ggplot(filter(AllSeries,Site=="X588_Law_Dome_18O_new" & (Source=="crutem3-2012-01.nc" | Source== basename(http_crutem3))
         ), aes(x=Date,y=InstrumentSeries, color=Source))+geom_line() +xlim(1960,2000)

## Hadcrut versus  HadV
ggplot(filter(AllSeries,Site=="X588_Law_Dome_18O_new" & (Source==basename(http_hadcrut3) | Source==  basename(http_hadcrut3v))
), aes(x=Date,y=InstrumentSeries, color=Source))+geom_line() +xlim(1960,2000)

ggplot(filter(AllSeries,Site=="X588_Law_Dome_18O_new" & (Source==basename(http_hadcrut3) | Source==  basename(http_crutem3))
), aes(x=Date,y=InstrumentSeries, color=Source))+geom_line() +xlim(1960,2000)

 

ggplot(filter(Proxy,Site=="X588_Law_Dome_18O_new"),
  aes(x=Date,y=Value))+geom_line() 


####  LawDome is a coast cell and gets special treatment in Hadcrut. 
##

 
HAT <- brick(file.path(Downloads,basename(http_hadcrut3v)),varname="temp")

# had stores Days since 1850,  jan 1850 is 15 days.
Days <- getZ(HAT)
 
#  get a layer 150 years  from the start 

L2  <- min(which(Days > 150*365))

# Lawdome  location

LD <- c(ProxyMetadata$long[2],ProxyMetadata$lat[2])

##   set an extent for cropping and mapping
## +- 20 deg long,  +- 5 lat

e <-extent(LD[1]-20,LD[1]+20,LD[2]-5,LD[2]+5)

LawDome <- crop(HAT,e)

# pick a layer
LD2000 <-raster(LawDome,layer=L2)

plot(LD2000,zlim=c(-2,2), main = "Law Dome Hadcrut")
points(x=LD[1],y=LD[2],col="red")
map("world",add=TRUE)



CRU <- brick(file.path(Downloads,basename(http_crutem3v)),varname="temp")

 
LawDomeCru <- crop(CRU,e)

LD2000c <-raster(LawDomeCru,layer=L2)

plot(LD2000c,main = "Law Dome Crutem",zlim=c(-2,2))
points(x=LD[1],y=LD[2],col="red")
map("world",add=TRUE)

 
