#########################
#lecture des différentes fonctions permettant de récupérer les données

##############################################################################################################
# This script allows to extract the raw mean temperature and the mean anomaly (in °C) at sites for given dates.
# Those sites have to be in metropolitan France (and Corsica)
#
# The anomaly is defined as the difference between the mean temperature of the site at the given date and 
# the mean of the mean temperature at this site for the given day and month from 1980 to 2010.
#
# It returns the data frame of each day and site (given in arguments) with a new column for the T° of 
# the day (tg_day), and as many columns as the number of days before the survey specified in arguments 
# (e.g. T° of the day before: tg_day_before1, T° two days before: tg_day_before2, etc) and the same 
# things for anomalies (tg_day_ano, tg_day_ano_before1, tg_day_ano_before2, etc.).
#
##############################################################################################################
#
# Meteorological data (from E-OBS) as to be download at this link : 
# https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Choose either  : Ensemble Mean TG from the 0.25 deg. regular grid 
# or the 0.1 deg. regular grid (more precise but heavier)
#
##############################################################################################################
#
# Arguments : 
#
# path_to_meteo_nc : path to access to the meteo .nc file
# 
# tableDaysSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS84
# -> a column named "Longitude" with the site longitude in WGS84
# -> a column with the sites unique IDs
# -> a column named "Date" with the date of the survey for each sites
#       Dates should be formatted this way : "2020-05-26" if characters
#       They might also be c("POSIXct","POSIXt") objects
#
# nbrPreviousDays : number of previous days for which T° should be returned
# (for example "3", to obtain the temperature for the survey date but for also the three previous days)

##############################################################################################################

extract_t_mean <- function(path_to_meteo_nc,tableDaysSites,nbrPreviousDays){
  
  ####Preliminary####
  
  #Install and open required packages
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("ncdf4","sf","stringr","lubridate","ggplot2","tidyr","data.table")
  load(packages)
  
  #creation of a site layer
  sites_SHP <- st_as_sf(tableDaysSites, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  if (all(class(tableDaysSites[,which(colnames(tableDaysSites)=="Date")])!=c("POSIXct","POSIXt"))){
    tableDaysSites[,which(colnames(tableDaysSites)=="Date")] <- parse_date_time(
      as.character(tableDaysSites[,which(colnames(tableDaysSites)=="Date")]),"%Y-%m-%d", tz="Europe/Paris")
  }
  
  ####I. Meteo data formatting####
  
  #Open meteo file
  tg.ncdf <- nc_open(path_to_meteo_nc)
  
  #This file has three dimensions : longitude, latitude, date
  #Extraction of the values taken by each of these dimensions
  lon <- tg.ncdf$dim$longitude$vals
  lat <- tg.ncdf$dim$latitude$vals
  date <- tg.ncdf$dim$time$vals
  date <- as_date(date,origin="1950-01-01") #conversion in dates object
  
  #lim date : from 1980-01-01 to today
  start_date = which(date=="1980-01-01")
  end_date = length(date)
  len_date <- end_date-start_date+1
  
  #lim longitude (for France)
  start_lon <- which(lon+6==min(abs(lon+6)))
  stop_lon <- which(lon-10==min(abs(lon-10)))
  len_lon <- stop_lon-start_lon+1
  
  #lim longitude (for France)
  start_lat <- which(lat-41==min(abs(lat-41)))
  stop_lat <- which(lat-52==min(abs(lat-52)))
  len_lat <- stop_lat-start_lat+1
  
  #data extraction from the .nc file into an array
  tg_array <- ncvar_get(tg.ncdf,"tg",start=c(start_lon,start_lat,start_date),count=c(len_lon,len_lat,len_date))
  dimnames(tg_array) <- list(c(lon[start_lon:stop_lon]),c(lat[start_lat:stop_lat]),c(as.character(date[start_date:end_date])))
  
  print("1. Data formatting OK")
  
  ####II. Link between sites and meteo points####
  
  ####____A. Detect longitude * latitude for which data are available####
  
  #Take one date not to recent (data has to exist) but not old either
  #Here arbitrarily : 2019-05-15
  #And extract temperature for each longitude * latitude at this date (array with 2 dimensions)
  
  date_map = which(date=="2019-05-15")
  tg_link_array <- ncvar_get(tg.ncdf,"tg",start=c(start_lon,start_lat,date_map),count=c(len_lon,len_lat,1)) #extraction
  dimnames(tg_link_array) <- list(c(lon[start_lon:stop_lon]),c(lat[start_lat:stop_lat])) #longitude and latitude coord
  tg_link_arrayDT <- as.data.frame(tg_link_array) #array to data.frame
  tg_link_arrayDT$lon <- rownames(tg_link_arrayDT) #longitude in a column
  tg_link_arrayDT <- gather(tg_link_arrayDT,key="lat",value="tg",c(1:((dim(tg_link_arrayDT)[2])-1))) #reshape the data frame in a data frame with a column longitude, a column latitude and a column with the mean temperature
  tg_link_arrayDT <- tg_link_arrayDT[which(!(is.na(tg_link_arrayDT$tg))),] #remove longitude * latitude with NA
  tg_link_arrayDT$coord <- paste0(tg_link_arrayDT$lon,"_",tg_link_arrayDT$lat) #create a column with the concatenation of longitude and latitude
  
  ####____B. Create a shapefile with the points of the meteo data####
  
  coord_meteo_SHP <- st_as_sf(tg_link_arrayDT, coords = c("lon","lat"), crs = 4326)
  coord_meteo_SHP <- st_transform(coord_meteo_SHP,crs=2154)
  
  ####____C. create the link between meteo data and sites####
  
  #Creation of a new column in the shapefile with the sites that indicate, for each site, the closest point
  #of the meteo data
  
  sites_SHP$corresp_EObs <- coord_meteo_SHP$coord[st_nearest_feature(sites_SHP,coord_meteo_SHP)]
  
  print("2. Link between sites and meteo points OK")
  
  ####III. Temperature and anomaly extraction for each night####
  
  tableDaysSites$corresp_EObs <- as.data.frame(sites_SHP$corresp_EObs)
  
  tableDaysSites$tg_day <- rep(NA,dim(tableDaysSites)[1])
  
  if (nbrPreviousDays !=0){
    for (i in 1:nbrPreviousDays){
      tableDaysSites[,dim(tableDaysSites)[2]+1] <- rep(NA,dim(tableDaysSites)[1])
      colnames(tableDaysSites)[dim(tableDaysSites)[2]] <- paste0("tg_day_before",i)
    }
  }
  
  tableDaysSites$tg_day_ano <- rep(NA,dim(tableDaysSites)[1])
  
  if (nbrPreviousDays !=0){
    for (i in 1:nbrPreviousDays){
      tableDaysSites[,dim(tableDaysSites)[2]+1] <- rep(NA,dim(tableDaysSites)[1])
      colnames(tableDaysSites)[dim(tableDaysSites)[2]] <- paste0("tg_day_ano_before",i)
    }
  }
  
  for (index in c(1:dim(tableDaysSites)[1])){
    
    date_day <- tableDaysSites[index,which(colnames(tableDaysSites)=="Date")]
    
    if (as.character(date_day) %in% (dimnames(tg_array)[[3]])==T){
      
      coord_meteo <- sites_SHP$corresp_EObs[index]
      coord_lon <- str_split(coord_meteo,"_",simplify = T)[1]
      coord_lat <- str_split(coord_meteo,"_",simplify = T)[2]
      
      #raw temperature
      tableDaysSites$tg_day[index] <- tg_array[coord_lon,coord_lat,as.character(date_day)]
      
      #anomaly
      if(str_sub(as.character(date_day),start = 6)!="02-29"){
        date_1980_2010 <- paste0(c(1980:2010),str_sub(date_day,start=5))
        anomaly_1980_2010 <- mean(tg_array[coord_lon,coord_lat,date_1980_2010])
        tableDaysSites$tg_day_ano[index] <- tableDaysSites$tg_day[index]-anomaly_1980_2010
        
        if (nbrPreviousDays!=0){
          for (i in (1:nbrPreviousDays)){
            tableDaysSites[index,which(colnames(tableDaysSites)==paste0("tg_day_before",i))] <- tg_array[coord_lon,coord_lat,as.character(date_day-days(i))]
            tableDaysSites[index,which(colnames(tableDaysSites)==paste0("tg_day_ano_before",i))] <- tableDaysSites[index,which(colnames(tableDaysSites)==paste0("tg_day_before",i))]-anomaly_1980_2010
            
          }
        }
      }
      
    }
    
    cat(paste0(round(index/(dim(tableDaysSites)[1])*100,digits=1),"%\r"))
  }
  
  print("3. Temperature and anomaly extraction for each night OK")
  
  return(tableDaysSites)
}




##############################################################################################################
# This script allows to extract the precipitations sum (in mm) at sites for given dates.
# Those sites have to be in metropolitan France (and Corsica)
#
# It returns the data frame of each day and site (given in arguments) with a new column for the precipitations
# of the day (rr_day), and as many columns as the number of days before the survey specified in arguments 
# (e.g. precipitations of the day before: rr_day_before1, precipitations two days before: rr_day_before2, etc).
#
##############################################################################################################
#
# Meteorological data (from E-OBS) as to be download at this link : 
# https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Choose either  : Ensemble Mean RR from the 0.25 deg. regular grid 
# or the 0.1 deg. regular grid (more precise but heavier)
#
##############################################################################################################
#
# Arguments : 
#
# path_to_meteo_nc : path to access to the meteo .nc file
# 
# tableDaysSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS84
# -> a column named "Longitude" with the site longitude in WGS84
# -> a column with the sites unique IDs
# -> a column named "Date" with the date of the survey for each sites
#       Dates should be formatted this way : "2020-05-26" if characters
#       They might also be c("POSIXct","POSIXt") objects
#
# nbrPreviousDays : number of previous days for which T° should be returned
# (for example "3", to obtain the temperature for the survey date but for also the three previous days)

##############################################################################################################

extract_precipitations <- function(path_to_meteo_nc,tableDaysSites,nbrPreviousDays){
  
  ####Preliminary####
  
  #Install and open required packages
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("ncdf4","sf","stringr","lubridate","ggplot2","tidyr","data.table")
  load(packages)
  
  #creation of a site layer
  sites_SHP <- st_as_sf(tableDaysSites, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  if (all(class(tableDaysSites[,which(colnames(tableDaysSites)=="Date")])!=c("POSIXct","POSIXt"))){
    tableDaysSites[,which(colnames(tableDaysSites)=="Date")] <- parse_date_time(
      as.character(tableDaysSites[,which(colnames(tableDaysSites)=="Date")]),"%Y-%m-%d", tz="Europe/Paris")
  }
  
  ####I. Meteo data formatting####
  
  #Open meteo file
  rr.ncdf <- nc_open(path_to_meteo_nc)
  
  #This file has three dimensions : longitude, latitude, date
  #Extraction of the values taken by each of these dimensions
  lon <- rr.ncdf$dim$longitude$vals
  lat <- rr.ncdf$dim$latitude$vals
  date <- rr.ncdf$dim$time$vals
  date <- as_date(date,origin="1950-01-01") #conversion in dates object
  
  #lim date : from 1980-01-01 to today
  start_date = which(date=="1980-01-01")
  end_date = length(date)
  len_date <- end_date-start_date+1
  
  #lim longitude (for France)
  start_lon <- which(lon+6==min(abs(lon+6)))
  stop_lon <- which(lon-10==min(abs(lon-10)))
  len_lon <- stop_lon-start_lon+1
  
  #lim longitude (for France)
  start_lat <- which(lat-41==min(abs(lat-41)))
  stop_lat <- which(lat-52==min(abs(lat-52)))
  len_lat <- stop_lat-start_lat+1
  
  #data extraction from the .nc file into an array
  rr_array <- ncvar_get(rr.ncdf,"rr",start=c(start_lon,start_lat,start_date),count=c(len_lon,len_lat,len_date))
  dimnames(rr_array) <- list(c(lon[start_lon:stop_lon]),c(lat[start_lat:stop_lat]),c(as.character(date[start_date:end_date])))
  
  print("1. Data formatting OK")
  
  ####II. Link between sites and meteo points####
  
  ####____A. Detect longitude * latitude for which data are available####
  
  #Take one date not to recent (data has to exist) but not old either
  #Here arbitrarily : 2019-05-15
  #And extract temperature for each longitude * latitude at this date (array with 2 dimensions)
  
  date_map = which(date=="2019-05-15")
  rr_link_array <- ncvar_get(rr.ncdf,"rr",start=c(start_lon,start_lat,date_map),count=c(len_lon,len_lat,1)) #extraction
  dimnames(rr_link_array) <- list(c(lon[start_lon:stop_lon]),c(lat[start_lat:stop_lat])) #longitude and latitude coord
  rr_link_arrayDT <- as.data.frame(rr_link_array) #array to data.frame
  rr_link_arrayDT$lon <- rownames(rr_link_arrayDT) #longitude in a column
  rr_link_arrayDT <- gather(rr_link_arrayDT,key="lat",value="rr",c(1:((dim(rr_link_arrayDT)[2])-1))) #reshape the data frame in a data frame with a column longitude, a column latitude and a column with the mean temperature
  rr_link_arrayDT <- rr_link_arrayDT[which(!(is.na(rr_link_arrayDT$rr))),] #remove longitude * latitude with NA
  rr_link_arrayDT$coord <- paste0(rr_link_arrayDT$lon,"_",rr_link_arrayDT$lat) #create a column with the concatenation of longitude and latitude
  
  ####____B. Create a shapefile with the points of the meteo data####
  
  coord_meteo_SHP <- st_as_sf(rr_link_arrayDT, coords = c("lon","lat"), crs = 4326)
  coord_meteo_SHP <- st_transform(coord_meteo_SHP,crs=2154)
  
  ####____C. create the link between meteo data and sites####
  
  #Creation of a new column in the shapefile with the sites that indicate, for each site, the closest point
  #of the meteo data
  
  sites_SHP$corresp_EObs <- coord_meteo_SHP$coord[st_nearest_feature(sites_SHP,coord_meteo_SHP)]
  
  print("2. Link between sites and meteo points OK")
  
  ####III. Temperature and anomaly extraction for each night####
  
  tableDaysSites$corresp_EObs <- as.data.frame(sites_SHP$corresp_EObs)
  
  tableDaysSites$rr_day <- rep(NA,dim(tableDaysSites)[1])
  
  if (nbrPreviousDays !=0){
    for (i in 1:nbrPreviousDays){
      tableDaysSites[,dim(tableDaysSites)[2]+1] <- rep(NA,dim(tableDaysSites)[1])
      colnames(tableDaysSites)[dim(tableDaysSites)[2]] <- paste0("rr_day_before",i)
    }
  }
  
  for (index in c(1:dim(tableDaysSites)[1])){
    
    date_day <- tableDaysSites[index,which(colnames(tableDaysSites)=="Date")]
    
    if (as.character(date_day) %in% (dimnames(rr_array)[[3]])==T){
      
      coord_meteo <- sites_SHP$corresp_EObs[index]
      coord_lon <- str_split(coord_meteo,"_",simplify = T)[1]
      coord_lat <- str_split(coord_meteo,"_",simplify = T)[2]
      
      #raw temperature
      tableDaysSites$rr_day[index] <- rr_array[coord_lon,coord_lat,as.character(date_day)]
      
      if (nbrPreviousDays!=0){
        for (i in (1:nbrPreviousDays)){
          tableDaysSites[index,which(colnames(tableDaysSites)==paste0("rr_day_before",i))] <- rr_array[coord_lon,coord_lat,as.character(date_day-days(i))]
        }
      }
      
    }
    
    cat(paste0(round(index/(dim(tableDaysSites)[1])*100,digits=1),"%\r"))
  }
  
  print("3. Precipitations extraction for each night OK")
  
  return(tableDaysSites)
}



##############################################################################################################
# This script allows to extract the wind speed (in m.s-1) at sites for given nights at 18, 0 or 6 o'clock.
# Those sites have to be in metropolitan France (and Corsica)
#
# It returns the data frame of each night and site (given in arguments) with 3 columns for the wind speed
# for the day studied (wind_night_18 for the wind speed at 18 o'clock, i.e beginning of night, wind_night_
# for the wind speed at midnight, wind_night_06 for the wind speed at 6 o'clock, i.e. end of the night)
# and as many columns as the number of days before the survey specified in arguments x 3 
# (e.g. wind speed of the day before at o'clock: wind_night_18_before1, wind speed two days before at 18 
# o'clock: wind_night_18_before2, etc).
#
##############################################################################################################
#
# /!\ The data used have a low spatial resolution (2°) but a high temporal one (every 6 hours)
# Package RNCEP - data from the NOAA
#
# An internet connection is required.
#
##############################################################################################################
#
# Arguments : 
# 
# tableNightsSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS84
# -> a column named "Longitude" with the site longitude in WGS84
# -> a column with the sites unique IDs
# -> a column named "Date" with the date of the survey for each sites.
#       The date for a night should be the date when it began.
#       Dates should be formatted this way : "2020-05-26" if characters.
#       They might also be c("POSIXct","POSIXt") objects.
#
# Col_ID : column name of the table containing the site unique IDs (ex : "CODE_SITE")
#
# nbrPreviousDays : number of previous nights for which wind speed should be returned
# (for example "3", to obtain the wind speed for the survey night but for also the three previous ones)
#
# year_beg : year when the surveys began
#
# year_end : year when the surveys ended
#
##############################################################################################################

extract_WindSpeed <- function(tableNightsSites,Col_ID,nbrPreviousDays,year_beg,year_end){
  
  #Install and open required packages
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("RNCEP","sf","lubridate","data.table","stringr","dplyr")
  load(packages)
  
  #Data formatting
  if (all(class(tableNightsSites[,which(colnames(tableNightsSites)=="Date")])!=c("POSIXct","POSIXt"))){
    tableNightsSites[,which(colnames(tableNightsSites)=="Date")] <- parse_date_time(
      as.character(tableNightsSites[,which(colnames(tableNightsSites)=="Date")]),"%Y-%m-%d", tz="Europe/Paris")
  }
  
  #####I. Extraction####
  Years=c(year_beg,year_end)
  LatWindow=c(41,52)
  LongWindow=c(-6,9)
  
  point_wind_U=NCEP.gather("uwnd.sig995","surface"
                           ,months.minmax=c(1,12)
                           ,years.minmax=Years
                           ,lat.southnorth=LatWindow, lon.westeast=LongWindow)
  
  
  point_wind_V=NCEP.gather("vwnd.sig995","surface"
                           ,months.minmax=c(1,12)
                           ,years.minmax=Years
                           ,lat.southnorth=LatWindow, lon.westeast=LongWindow)
  
  point_wind_UDT <- as.data.frame(as.data.table(point_wind_U))
  point_wind_VDT <- as.data.frame(as.data.table(point_wind_V))
  all(point_wind_UDT$V1==point_wind_VDT$V1)
  all(point_wind_UDT$V2==point_wind_VDT$V2)
  all(point_wind_UDT$V3==point_wind_VDT$V3)
  point_wind <- point_wind_UDT
  colnames(point_wind) <- c("latitude","longitude","year_date_time","wind_ms_U")
  point_wind$wind_ms_V <- point_wind_VDT$value
  point_wind$wind_ms <- sqrt((point_wind$wind_ms_U^2 + point_wind$wind_ms_V^2))
  
  point_wind$coord <- paste0(point_wind$latitude,"_",point_wind$longitude)
  point_wind$hour <- as.numeric(str_sub(point_wind$year_date_time,start=-2))
  point_wind$date <- parse_date_time(str_sub(point_wind$year_date_time,end=-4),"%Y_%m_%d", tz="Europe/Paris")
  
  ####II. Correspondance between survey points and RNCEP points####
  
  site_Nights <- distinct(tableNightsSites[,which(colnames(tableNightsSites) %in% c(Col_ID,"Longitude","Latitude"))])
  
  #Creation of a survey site layer
  sites_SHP <- st_as_sf(site_Nights, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  #Creation of RNCEP points layer
  site_RNCEP <- distinct(point_wind[,which(colnames(point_wind) %in% c("coord","longitude","latitude"))])
  RNCEP_SHP <- st_as_sf(site_RNCEP, coords = c("longitude","latitude"), crs = 4326)
  RNCEP_SHP <- st_transform(RNCEP_SHP,crs=2154)
  
  #Correspondance
  site_Nights$corresp_RNCEP <- site_RNCEP$coord[st_nearest_feature(sites_SHP,RNCEP_SHP)]
  tableNightsSites$corresp_RNCEP <- site_Nights$corresp_RNCEP[
    match(tableNightsSites[,which(colnames(tableNightsSites)==Col_ID)],site_Nights[,which(colnames(site_Nights)==Col_ID)])]
  
  ####III. Wind speed extraction####
  
  tableNightsSites$wind_night_18 <- rep(NA,dim(tableNightsSites)[1])
  tableNightsSites$wind_night_12 <- rep(NA,dim(tableNightsSites)[1])
  tableNightsSites$wind_night_06 <- rep(NA,dim(tableNightsSites)[1])
  
  if(nbrPreviousDays!=0){
    for (j in (1:nbrPreviousDays)){
      for (k in c("18","12","06")){
        tableNightsSites[,dim(tableNightsSites)[2]+1] <- rep(NA,dim(tableNightsSites)[1])
        colnames(tableNightsSites)[dim(tableNightsSites)[2]] <- paste0("wind_night_",k,"_before",j)
      }
    }
  }
  for (index in c(1:dim(tableNightsSites)[1])){
    
    #only data for this site
    point_wind_subset <- point_wind[which(point_wind$coord==tableNightsSites$corresp_RNCEP[index]),]
    
    if(length(which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==18)))>0){
      tableNightsSites$wind_night_18[index] <- point_wind_subset$wind_ms[
        which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==18))]
    }
    
    if(length(which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==12)))>0){
      tableNightsSites$wind_night_12[index] <- point_wind_subset$wind_ms[
        which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==12))]
    }
    if(length(which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==06)))>0){
      tableNightsSites$wind_night_06[index] <- point_wind_subset$wind_ms[
        which((point_wind_subset$date==tableNightsSites$Date[index])&(point_wind_subset$hour==06))]
    }
    
    
    if(nbrPreviousDays!=0){
      
      for (nbrNight in (1:nbrPreviousDays)){
        
        if (length(which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==18)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "wind_night_18_before",nbrNight))] <- point_wind_subset$wind_ms[
                which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==18))]
        }
        
        if (length(which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==12)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "wind_night_12_before",nbrNight))] <- point_wind_subset$wind_ms[
                which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==12))]
        }
        
        if (length(which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==06)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "wind_night_06_before",nbrNight))] <- point_wind_subset$wind_ms[
                which((point_wind_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_wind_subset$hour==06))]
        }
      }
    }
    
    cat(paste0(round(index/dim(tableNightsSites)[1]*100,digits=1),"%\r"))
    
  }
  
  tableNightsSites <- tableNightsSites[,which(colnames(tableNightsSites)!="corresp_RNCEP")]
  return(tableNightsSites)
}



##############################################################################################################
# This script allows to extract the cloud cover (in %) at sites for given nights at 18, 0 or 6 o'clock.
# Those sites have to be in metropolitan France (and Corsica)
#
# It returns the data frame of each night and site (given in arguments) with 3 columns for the cloud cover
# for the day studied (cloud_cover_18 for the cloud cover at 18 o'clock, i.e beginning of night, cloud_cover_00
# for the cloud cover at midnight, cloud_cover_06 for the cloud cover at 6 o'clock, i.e. end of the night)
# and as many columns as the number of days before the survey specified in arguments x 3 
# (e.g. cloud cover of the day before at o'clock: cloud_cover_18_before1, cloud cover two days before at 18 
# o'clock: cloud_cover_18_before2, etc).
#
##############################################################################################################
#
# /!\ The data used have a low spatial resolution (2°) but a high temporal one (every 6 hours)
# Package RNCEP - data from the NOAA
#
# An internet connection is required.
#
##############################################################################################################
#
# Arguments : 
# 
# tableNightsSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS84
# -> a column named "Longitude" with the site longitude in WGS84
# -> a column with the sites unique IDs
# -> a column named "Date" with the date of the survey for each sites.
#       The date for a night should be the date when it began.
#       Dates should be formatted this way : "2020-05-26" if characters.
#       They might also be c("POSIXct","POSIXt") objects.
#
# Col_ID : column name of the table containing the site unique IDs (ex : "CODE_SITE")
#
# nbrPreviousDays : number of previous nights for which cloud cover should be returned
# (for example "3", to obtain the cloud cover for the survey night but for also the three previous ones)
#
# year_beg : year when the surveys began
#
# year_end : year when the surveys ended
#
##############################################################################################################

extract_CloudCover <- function(tableNightsSites,Col_ID,nbrPreviousDays,year_beg,year_end){
  
  #Install and open required packages
  load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("RNCEP","sf","lubridate","data.table","stringr","dplyr")
  load(packages)
  
  #Data formatting
  if (all(class(tableNightsSites[,which(colnames(tableNightsSites)=="Date")])!=c("POSIXct","POSIXt"))){
    tableNightsSites[,which(colnames(tableNightsSites)=="Date")] <- parse_date_time(
      as.character(tableNightsSites[,which(colnames(tableNightsSites)=="Date")]),"%Y-%m-%d", tz="Europe/Paris")
  }
  
  #####I. Extraction####
  Years <- c(year_beg,year_end)
  LatWindow <- c(41,52)
  LongWindow <- c(-6,9)
  
  point_cloud_cover <- NCEP.gather("tcdc.eatm","gaussian"
                                   ,months.minmax=c(1,12)
                                   ,years.minmax=Years
                                   ,lat.southnorth=LatWindow, lon.westeast=LongWindow)
  
  point_cloud_cover <- as.data.frame(as.data.table(point_cloud_cover))
  head(point_cloud_cover)
  colnames(point_cloud_cover) <- c("latitude","longitude","year_date_time","cloud_cover")
  
  point_cloud_cover$coord <- paste0(point_cloud_cover$latitude,"_",point_cloud_cover$longitude)
  point_cloud_cover$hour <- as.numeric(str_sub(point_cloud_cover$year_date_time,start=-2))
  point_cloud_cover$date <- parse_date_time(str_sub(point_cloud_cover$year_date_time,end=-4),"%Y_%m_%d", tz="Europe/Paris")
  
  ####II. Correspondance between survey points and RNCEP points####
  
  site_Nights <- distinct(tableNightsSites[,which(colnames(tableNightsSites) %in% c(Col_ID,"Longitude","Latitude"))])
  
  #Creation of a survey site layer
  sites_SHP <- st_as_sf(site_Nights, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  #Creation of RNCEP points layer
  site_RNCEP <- distinct(point_cloud_cover[,which(colnames(point_cloud_cover) %in% c("coord","longitude","latitude"))])
  RNCEP_SHP <- st_as_sf(site_RNCEP, coords = c("longitude","latitude"), crs = 4326)
  RNCEP_SHP <- st_transform(RNCEP_SHP,crs=2154)
  
  #Correspondance
  site_Nights$corresp_RNCEP <- site_RNCEP$coord[st_nearest_feature(sites_SHP,RNCEP_SHP)]
  tableNightsSites$corresp_RNCEP <- site_Nights$corresp_RNCEP[
    match(tableNightsSites[,which(colnames(tableNightsSites)==Col_ID)],site_Nights[,which(colnames(site_Nights)==Col_ID)])]
  
  ####III. cloud cover extraction####
  
  tableNightsSites$cloud_cover_18 <- rep(NA,dim(tableNightsSites)[1])
  tableNightsSites$cloud_cover_12 <- rep(NA,dim(tableNightsSites)[1])
  tableNightsSites$cloud_cover_06 <- rep(NA,dim(tableNightsSites)[1])
  
  if(nbrPreviousDays!=0){
    for (j in (1:nbrPreviousDays)){
      for (k in c("18","12","06")){
        tableNightsSites[,dim(tableNightsSites)[2]+1] <- rep(NA,dim(tableNightsSites)[1])
        colnames(tableNightsSites)[dim(tableNightsSites)[2]] <- paste0("cloud_cover_",k,"_before",j)
      }
    }
  }
  for (index in c(1:dim(tableNightsSites)[1])){
    
    #only data for this site
    point_cloud_cover_subset <- point_cloud_cover[which(point_cloud_cover$coord==tableNightsSites$corresp_RNCEP[index]),]
    
    if(length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==18)))>0){
      tableNightsSites$cloud_cover_18[index] <- point_cloud_cover_subset$cloud_cover[
        which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==18))]
    }
    
    if(length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==12)))>0){
      tableNightsSites$cloud_cover_12[index] <- point_cloud_cover_subset$cloud_cover[
        which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==12))]
    }
    if(length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==06)))>0){
      tableNightsSites$cloud_cover_06[index] <- point_cloud_cover_subset$cloud_cover[
        which((point_cloud_cover_subset$date==tableNightsSites$Date[index])&(point_cloud_cover_subset$hour==06))]
    }
    
    
    if(nbrPreviousDays!=0){
      
      for (nbrNight in (1:nbrPreviousDays)){
        
        if (length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==18)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "cloud_cover_18_before",nbrNight))] <- point_cloud_cover_subset$cloud_cover[
                which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==18))]
        }
        
        if (length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==12)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "cloud_cover_12_before",nbrNight))] <- point_cloud_cover_subset$cloud_cover[
                which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==12))]
        }
        
        if (length(which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==06)))>0){
          tableNightsSites[
            index,which(colnames(tableNightsSites)==paste0(
              "cloud_cover_06_before",nbrNight))] <- point_cloud_cover_subset$cloud_cover[
                which((point_cloud_cover_subset$date==tableNightsSites$Date[index]-days(nbrNight))&(point_cloud_cover_subset$hour==06))]
        }
      }
    }
    
    cat(paste0(round(index/dim(tableNightsSites)[1]*100,digits=1),"%\r"))
    
  }
  
  tableNightsSites <- tableNightsSites[,which(colnames(tableNightsSites)!="corresp_RNCEP")]
  return(tableNightsSites)
}

#Preparation du tableau nécessaire pour faire fonctionner chaque fonction

library(sf)
options( "digits"=10, "scipen"=0)
mtq <- st_read("C:/Users/ludo2/Documents/These/caracterisation_parcelles_tournesol_2020/tournesol2020.shp", quiet=TRUE)
plot(mtq)
st_crs(mtq)
mtq_c <- st_centroid(mtq)
plot(st_geometry(mtq))
plot(st_geometry(mtq_c), add=TRUE, cex=1.2, col="red", pch=20)

mtq_reproj <- st_transform(mtq_c, 4326)
plot(st_geometry(mtq_reproj))

df1 <- as.data.frame(st_coordinates(mtq_reproj))
df <-cbind(mtq_reproj, df1)
df <- df[,c(4,7,8)]
names(df) <- c("id", "Longitude", "Latitude", "geometry")
df$Date <- "2020-08-20"


#Recuperation des données meteo pour les sites et la periode souhaitée
t_mean <- extract_t_mean("C:/Users/ludo2/Documents/These/data/meteo/tg_ens_mean_0.1deg_reg_v25.0e.nc",st_drop_geometry(df),50)
precipitation <- extract_precipitations("C:/Users/ludo2/Documents/These/data/meteo/rr_ens_mean_0.1deg_reg_v25.0e.nc", st_drop_geometry(df), 50)
wind <- extract_WindSpeed(st_drop_geometry(df),"id",50,2020,2020)
cloudcover <- extract_CloudCover(st_drop_geometry(df),"id",50,2020,2020)


#creation du jeux de données meteo
library(tidyverse)
t <- seq(183, 233)
#donnees temperature
test <- t_mean[,c(6:56)]
for(i in 1:ncol(test)){
  colnames(test)[i] <- t[i]
}
test <- cbind(t_mean$id, test)
test1 <- gather(test, key = "day", value ="temperature", 2:52)
colnames(test1)[1] <- "id"

#donnees precipitation
test <- precipitation[,c(6:56)]
for(i in 1:ncol(test)){
  colnames(test)[i] <- t[i]
}
test <- cbind(precipitation$id, test)
test2 <- gather(test, key = "day", value ="precipitation", 2:52)
colnames(test2)[1] <- "id"

#donnees de vent
t <- rep(t,3)
t <- t[order(t)]
t1 <- c(rep(c("18","12","06"),51))
t2 <- paste(t,t1,sep="_")
test <- wind[,c(5:157)]
for(i in 1:ncol(test)){
  colnames(test)[i] <- t2[i]
}
test <- cbind(wind$id, test)
test3 <- gather(test, key = "day", value ="wind_speed", 2:154)
colnames(test2)[1] <- "id"
test3 <- separate(data = test3, col = day, into = c("day", "hour"), sep = "_")
colnames(test3)[1] <- "id"

#donnees de couverture nuageuse
test <- cloudcover[,c(5:157)]
for(i in 1:ncol(test)){
  colnames(test)[i] <- t2[i]
}
test <- cbind(cloudcover$id, test)
test4 <- gather(test, key = "day", value ="cloudcover", 2:154)
colnames(test2)[1] <- "id"
test4 <- separate(data = test4, col = day, into = c("day", "hour"), sep = "_")
colnames(test4)[1] <- "id"




meteo_data <- left_join(test1, test2, by=c("id", "day"))
meteo_data <- left_join(meteo_data, test3, by=c("id", "day"))
meteo_data <- left_join(meteo_data, test4, by=c("id", "day", "hour"))
meteo_data <- meteo_data[,c(1,2,5,3,4,6,7)]



# fwrite(x = meteo_data, file = "data/meteo_data.csv")
