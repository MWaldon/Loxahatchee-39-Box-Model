### This file takes input as station name from DBHYDRO. It will triggee a table 
### with DBKEYs linked to names to populate the url query
### it does one station at a time because R times out under heavy data downloads

# *** this program was written by Dr. Donatto Surratt, National Park Service

#Develop loads and flow-weighted means
#install.packages("reshape")
#install.packages("zoo")
#install.packages("Hmisc")
library(reshape)
library(zoo)
#library(Hmisc)
#library(rgdal)
library(lubridate)
library(sf)
library(terra)
# Input is a station name: e.g. G251
# Start date: e.g., '2015-01-01'
# End date: e.g., '2019-06-30'
# wy: water year start month, 4 for State WY, 9 for Park WY
# PREF.prefer is TRUE then prefer the dbkey with RECORDER==PREF

flow.formatter <- function(staName, sdate, edate, wy = 4, 
                           PREF.prefer=FALSE, quiet=TRUE)
  { # begin function
  # download flow data for site=staName
  
  dbkeys.Q.read <- function() { # load flow dbkey table from csv file
    # dbkeys <- read.csv('C:/donatto/lox/r_codes/dbkeysFlow.csv',header=T)
    dbkeys <- read.csv('../DataSets/Surratt-2025-09/dbkyes4Waldon.csv',header=T)
    # remove the Dbkeys column which has leading zeros removed
    dbkeys <- dbkeys[, -which(names(dbkeys) == "Dbkey")]
    
    dbkeys$Start.Date <- as.Date(as.character(dbkeys$Start.Date),'%m/%d/%Y')
    dbkeys$End.Date <- as.Date(as.character(dbkeys$End.Date),'%m/%d/%Y')
    
    dbkeys <- dbkeys[-which(is.na(dbkeys$Start.Date)),] # remove NA observation
    # convert lat/long in degrees minutes seconds to decimal degrees
    dbkeys$Long <- as.numeric(substr(dbkeys$Longitude,1,2))+
      (as.numeric(substr(dbkeys$Longitude,3,4))/60)+
      (as.numeric(substr(dbkeys$Longitude,5,nchar(dbkeys$Longitude)))/3600)
    
    dbkeys$Lat <- as.numeric(substr(dbkeys$Latitude,1,2))+
      (as.numeric(substr(dbkeys$Latitude,3,4))/60)+
      (as.numeric(substr(dbkeys$Latitude,5,nchar(dbkeys$Latitude)))/3600)
    # set longitude to negative because it is west
    dbkeys$Long <- -1*(dbkeys$Long)
    # replace missing Recorder records with text 'none'
    dbkeys$Recorder[which(is.na(dbkeys$Recorder))] <- 'none'
    return(dbkeys)
  } # end function dbkeys.read
  dbkeys <- dbkeys.Q.read()
  
  # Compute UTM coordinates
  # make cord.dec a spatial object with coordinates as decimal degrees
  #  crs=4326 means spatial data is using WGS 84 geographic coordinate system, 
  #  with coordinates expressed in degrees of latitude and longitude
  cord.dec <- st_as_sf(dbkeys,coords=c('Long','Lat'),crs=4326)
  #cord.dec = SpatialPoints(cbind(-dbkeys$Long, dbkeys$Lat), proj4string = CRS("+proj=longlat"))
  #cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:26917"))
  # crs=26917 is EPSG:26917 CRS known as NAD83 / UTM zone 17N
  cord.UTM <- st_transform(cord.dec,crs=26917)
  #cord.UTM <- spTransform(cord.dec, CRS("+proj=latlong +datum=NAD83"))
  
  dbkeys$Xutm <- st_coordinates(cord.UTM$geometry)[,1] # cord.UTM@coords[1:nrow(dbkeys)]
  dbkeys$Yutm <- st_coordinates(cord.UTM$geometry)[,2]  #cord.UTM@coords[(nrow(dbkeys)+1):(nrow(dbkeys)+nrow(dbkeys))]
 
  # select the records for the selected site
  ss<-dbkeys[which(grepl(staName,dbkeys$Site)),]
  
  # days of each time series overlapping the desired period
  ss$dateDel <- pmax(0,pmin(ss$End.Date, edate) - pmax(ss$Start.Date, sdate))
  
  # if there are no overlapping dates then return
  if (max(ss$dateDel) == 0) {
    if (quiet == FALSE) print(c('no data for site ', staName))
    return(NA)
  }
  
  # select the best dbkey
  if (PREF.prefer & any(ss$Recorder=='PREF')) { # select PREF time series
    dbkeySelect <- ss$DBKEY[which(ss$Recorder=='PREF')]
  }  # end if
  else {
    
    # select the timeseries with longest period overlapping the desired period
    
    dbkeySelect <- ss$DBKEY[which.max(ss$dateDel)]
  } # end else
  dbkey.i <- which(dbkeys$DBKEY==dbkeySelect) # index of selected DBKEY
  
  # old new versiondbkeys$DBKEY[which(grepl(staName,dbkeys$Site)&which(dbkeys$Start.Date==min(dbkeys$Start.Date))&dbkeys$End.Date>=as.Date(edate))][1]
  
  
  #old version - updated 090419 -- dbkeys$DBKEY[which(grepl(staName,dbkeys$Site)&dbkeys$Start.Date<=as.Date(sdate)&dbkeys$End.Date>=as.Date(edate))][1]
  
  
  
  # Create the url
  sdate.f <- as.numeric(format(as.Date(sdate),'%Y%m%d'))
  edate.f <- as.numeric(format(as.Date(edate),'%Y%m%d'))
  
  url <- paste('http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=',
               sdate.f,
               '&v_end_date=',edate.f,
               '&v_report_type=format7&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_db_request_id=3351455&v_where_clause=&v_dbkey=',
               dbkeySelect,
               '%2F&v_os_code=Win&v_interval_count=5',sep='')
  
  # 2) Modified the working directory for your project
  # setwd(file.path('C:/donatto/lox/r_codes/'))
  
  
  # 3) Apply file to file.path 
  #local<-file.path("flow.csv")
  local<-file.path("../DataSets/Surratt-2025-09/flow.csv")
  download.file(url,local,method='libcurl')
  
  # 4) Get the data, separate to columns, names columns
  all_content <- readLines(local)
  if(isTRUE((grepl('FOUND',all_content[2])))){ # dbkey not found
    if (quiet==FALSE) print('No data returned for station ', staName)
    return(NA)
  } # end if
  # Find beginning of data
  index1 <- which(grepl("Daily\\b",all_content))
  
  meta <- all_content[2:index1]        # header data
  dataset <- all_content[-1:-index1]   # data
  # df.flow <- read.csv(textConnection(dataset),header=T, stringsAsFactor=F)
  # df.flow is the set of flow observations by date
  df.flow <- read.csv(textConnection(dataset),header=F, stringsAsFactor=F)
  # put the names for df.flow columns into df.meta as a list
  df.meta <- meta[[4]]
  df.meta <- strsplit(df.meta, ",")[[1]]
  df.meta <- lapply(df.meta, function(x) gsub("[\\\\\"]", "", x))
  #df.meta <- read.csv(textConnection(meta), header=T, sep=" ", stringsAsFactor=F)
  
  # reduce to date, qualifier, and flow
  df.flow <- df.flow[c(1,4,3)]
  #names(df.flow) <- c('Date',df.meta[1,1])
  names(df.flow) <- c('Date', 'Qualifier', 'CFS')
  df.flow$Date <- as.Date(df.flow$Date,'%d-%b-%Y')
  
  # set flow to 0 if negative  ** use if outflow is not needed
  # df.flow[2][df.flow[2]<0] <- 0
  
  # add water-year and Month-year to dataframe
  df.flow <- cbind(
    WY=ifelse(month(df.flow$Date)>wy,year(df.flow$Date)+1,year(df.flow$Date)),
    MY=as.yearmon(df.flow$Date),df.flow)
  
  # MY flows
  my.flow <- aggregate(df.flow[ncol(df.flow)],by=list(df.flow$MY),sum,na.rm=T)
  names(my.flow)[1] <- 'MY'
  
  # WY flows
  wy.flow <- aggregate(df.flow[ncol(df.flow)],by=list(df.flow$WY),sum,na.rm=T)
  names(wy.flow)[1] <- 'WY'
  
  
  if(quiet==FALSE) {
    plot(dbkeys$Xutm, dbkeys$Yutm, 
         col='blue', main=staName)
    text(dbkeys$Xutm,dbkeys$Yutm,dbkeys$Station, adj=0.5)
    points(dbkeys$Xutm[dbkey.i], dbkeys$Yutm[dbkey.i], 
           col='red', cex=3)
    plot(df.flow$Date, df.flow$CFS,
         main=staName)
  } # end if
  
  ret.list <- list(df.flow,my.flow,wy.flow,dbkeys, meta, staName, dbkey.i)
  names(ret.list) <- c('Q', 'Q.MonYr', 'Q.WY', 'DBKEYS', 'Q.head', 'structName', 'structIndex')
  
  file.save <- paste('../DataSets/Qupdate/',staName,'.Q.Rdata', sep = '')
  save(ret.list, file = file.save)
  return(ret.list)
} # end function flow.formatter

x <- flow.formatter(sname, sdate = sdate, edate = edate,
                    wy = 4, PREF.prefer = TRUE, quiet = FALSE)



