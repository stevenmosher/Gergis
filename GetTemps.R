#####################
#
#
#
###############################

####### Install packages
 
if(!"R.utils" %in% rownames(installed.packages()))install.packages("R.utils")  
if(!"dplyr"   %in% rownames(installed.packages()))install.packages("dplyr") 
if(!"ggplot2" %in% rownames(installed.packages()))install.packages("ggplot2") 
if(!"raster"  %in% rownames(installed.packages()))install.packages("raster") 
if(!"zoo"     %in% rownames(installed.packages()))install.packages("raster") 
if(!"ncdf4"   %in% rownames(installed.packages()))install.packages("ncdf4") 
if(!"stringr" %in% rownames(installed.packages()))install.packages("stringr") 
if(!"tidyr" %in% rownames(installed.packages()))install.packages("tidyr") 
if(!"broom" %in% rownames(installed.packages()))install.packages("broom") 

 

#######  Load libraries
library(tidyr)
library(broom)
library(R.utils)
library(dplyr)
library(ggplot2)
library(raster)
library(zoo)
library(ncdf4)
library(stringr)


 

#  Quick and dirty function for extracting data from  ncdf files for all locations

GenerateTimeSeries <- function(ncfile,location=cbind(ProxyMetadata$long,ProxyMetadata$lat),Name=ProxyMetadata$id){
  
    varname <-  "temp"
    start<-1850
     
    Anomaly <- brick(ncfile,varname=varname)
    ##  use  raster::extract becuse of library name space conflict
    E <- raster::extract(Anomaly,location,layer=1,nl=nlayers(Anomaly))
    E <- t(E)
    E <- ts(E, start=start, frequency = 12)
    colnames(E)<-Name
    
  return(E)
  
}


BerkeleyTimeSeries <- function(ncfile,location=cbind(ProxyMetadata$long,ProxyMetadata$lat),Name=ProxyMetadata$id){
  
  varname1 <-  "temperature" 
  varname2 <-  "climatology"
  start<-1850
  
  Anomaly     <- brick(ncfile,varname=varname1)
  Climate     <- brick(ncfile,varname=varname2)
  
  Z           <-getZ(Anomaly)
  dl    <- c(which(Z>2015.99))
  Anomaly <-dropLayer(Anomaly,i=dl)
  Temperature <- Anomaly+Climate 
  
  A <-  raster::extract(x=Temperature,y=location,layer=1,nl=nlayers(Anomaly))
  A  <- t(A)
  if(min(Z)<1800)start=1750
  
  A <- ts(A, start=start, frequency = 12)
   
  
  colnames(A)<-Name
  A <- window(A,start=1850,end=2016)
  return(A)
  
}

 TidySeries <- function(Base=NULL,New,Source){
    
  
   DF <- tbl_df(data.frame(Date=time(New),Source=Source ,stringsAsFactors=FALSE))
   DF <- cbind(DF ,coredata(New)) %>% tbl_df() %>%gather(Site,InstrumentSeries,starts_with("X"))
    
   if(!is.null(Base))DF<-bind_rows(Base,DF)
   return(DF)
   
 }



####      File names

Gergisloc="http://www.climateaudit.info/data/multiproxy/gergis_2016/info_gergis_2016.csv"

###    Current Had

http_hadcrut3v <-"https://crudata.uea.ac.uk/cru/data/crutem3/HadCRUT3v.nc"
http_hadcrut3  <-"https://crudata.uea.ac.uk/cru/data/crutem3/HadCRUT3.nc"
http_crutem3   <-"https://crudata.uea.ac.uk/cru/data/crutem3/CRUTEM3.nc"
http_crutem3v   <-"https://crudata.uea.ac.uk/cru/data/crutem3/CRUTEM3v.nc"

####  crutem archive
ct2012Nov   <-"https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-11/crutem3-2012-11.nc.gz"
ct2012Aug   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-08/crutem3-2012-08.nc.gz"
ct2012Jan   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-01/crutem3-2012-01.nc.gz"
ct2012Feb   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-02/crutem3-2012-02.nc.gz"
ct2012Mar   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-03/crutem3-2012-03.nc.gz"
ct2012Apr   <- "https://crudata.uea.ac.uk/cru/projects/acrid/crutem/crutem3-2012-04/crutem3-2012-04.nc.gz"


#Berkeley Data

berk        <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_LatLong1.nc"
blo         <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc"

#####
Downloads  <- "Downloads"

if(!file.exists(Downloads))dir.create(Downloads)



#################################  Download

download.file(url=berk,destfile=file.path(Downloads,basename(berk)),mode="wb")
download.file(url=blo,destfile=file.path(Downloads,basename(blo)),mode="wb")


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

##############  Unpack gzips
gzips <- list.files(Downloads,full.name=T, pattern= "gz")
destnames <- str_replace(string=gzips,pattern=".gz",replace="")
mapply(gunzip,filename=gzips,destname=destnames)


###  Get Filenames
NCDF_Files <- list.files(Downloads,full.names=TRUE, pattern=".nc")

####   get proxy locations
ProxyMetadata <- tbl_df(read.csv(Gergisloc,stringsAsFactors = FALSE))
 
##   Extract All data I just do it for all locations
##   return data  is a mts  structure  Proxy is in column--51 columns of time series
##   Then we tidy it all up



AllSeries        <-TidySeries(Base=NULL,New=GenerateTimeSeries(NCDF_Files[grepl("CRUTEM3.nc",NCDF_Files)]),
                              Source="CurrentCruTem")

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("CRUTEM3v.nc",NCDF_Files)]),
                              Source="CurrentCruTemV")

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("HadCRUT3.nc",NCDF_Files)]),
                              Source="CurrentHadcrut")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("HadCRUT3v.nc",NCDF_Files)]),
                              Source="CurrentHadcrutV")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-01.nc",NCDF_Files)]),
                              Source="Crutem2012Jan")

AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-02.nc",NCDF_Files)]),
                              Source="Crutem2012Feb")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-03.nc",NCDF_Files)]),
                              Source="Crutem2012Mar")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-04.nc",NCDF_Files)]),
                              Source="Crutem2012Apr")
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-08.nc",NCDF_Files)]),
                              Source="Crutem2012Aug") 
AllSeries        <-TidySeries(Base=AllSeries,New=GenerateTimeSeries(NCDF_Files[grepl("crutem3-2012-11.nc",NCDF_Files)]),
                              Source="Crutem2012Nov") 

###   plot law dome anomalies for all series

ggplot(filter(AllSeries,Site=="X588_Law_Dome_18O_new"), aes(x=Date,y=InstrumentSeries, color=Source))+geom_line() +xlim(1960,2000)

###   These are in absolute Temperature really slow to run
B                <- BerkeleyTimeSeries(NCDF_Files[grepl("Complete_TAVG",NCDF_Files)])
BLO              <- BerkeleyTimeSeries(NCDF_Files[grepl("Land_and_Ocean",NCDF_Files)])

 
 




