## ******** edited version-not working *************
library(httr)
library(jsonlite)
library(reshape)
library(zoo)
#library(Hmisc)
#library(rgdal)
library(lubridate)
library(sf)
library(terra)

#######################################################
###Source the flowFormatter.R record; change directory accordingly
###Needed for get conc when flowing for structures 
# You must change 
# source('C:/donatto/lox/r_codes/flowFormatterDBHI.R', echo=TRUE)
#######################################################


#source('flowFormatter.R') # function dbkeys.Q.read() is used by flow_ld
# compile function flow_ld <- function(staName,sdate,edate,wy)
#source('/CUpdate/flowFormatterDBHI.R', echo=TRUE) # 

conc_ld <- function(staName,sdate,edate,chem,wy){
  # staName is the site name
  # sdate, edate are the start & end dates for download
  # chem is number for parameter, 
  #  see https://insightsdata.sfwmd.gov/#/reference-tables?lookup=dataType
  #  example: chem=25 for total phosphorus
  # wy is beginning month of water year, 4 for SFWMD, 10 for USGS
  # convert date format to needed format
  sdate.f <- format(as.Date(sdate),'%Y%m%d')
  edate.f <- format(as.Date(edate),'%Y%m%d')
  
  # API endpoint
  url <- paste0(
    'https://api.sfwmd.gov/v1/insights-data/chem/report/data?reportType=timeseries&format=csv&startDate=',
    sdate.f,'&endDate=',edate.f,'&station=',staName,
    '&parameters=',chem)
  
  # Request headers
  headers <- c(
    "Accept" = "application/json, text/plain, */*",
    "Content-Type" = "application/json"
  )
  
  # Payload with detailed structure
  body <- toJSON(list(
    reportType = "timeseries",
    format = "csv",
    startDate = sdate.f,
    endDate = edate.f,
    query = list(
      locations = list(
        list(name = staName, type = "STATION")
      ),
      parameters = list(chem),
      methods = list("ALL"),
      projects = list("ALL"),
      matrices = list("ALL"),
      paramGroups = list("ALL")
    )
  ), auto_unbox = TRUE)
  
  outfile <- 'chem_report.csv'
  
  print(url)
  # Make the POST request
  response <- POST(
    url,
    add_headers(.headers = headers),
    body = body,
    encode = "json",
    write_disk(outfile, overwrite = TRUE)  # <- saves straight to disk
  )
  if (status_code(response) == 200) {
  
    d.conc <- read.csv('chem_report.csv',skip=22)
  # Check status and parse response
  #   data <- content(response, as = "parsed", simplifyVector = TRUE)
  #   d.conc <- data$timeseries
    # remove qualified data if they exist
    d.conc <- d.conc[which(grepl('SAMP',d.conc$sampleType)),]
    # just keep needed columns
    colKeep <- c('station','parameter','sampleType','collectDate','collectMethod','value','lat','long')
    d.conc <- d.conc[which(grepl(paste(c(colKeep),collapse='|'),names(d.conc)))]
    #print(data)
  } else {
    print(paste("Request failed with status:", status_code(response)))
    print(content(response, as = "text"))
  }
  if(nrow(d.conc)==0) return()
  d.conc$collectDate <- as.Date(d.conc$collectDate)
  d.conc$value <- ifelse(d.conc$value<0,abs(d.conc$value),d.conc$value)
  
  # correct the column names
  names(d.conc) <- c('Station.ID','Collection_Date','sampleType','Collection_Method','parameter','Value','latitude','longitude')
  # separate the auto from grab for selection purposes
  a.exist <- sum(d.conc$Collection_Method!='G')  # this is a check for auto sample later
  g <- d.conc[d.conc$Collection_Method=='G',]
  g <- g[c(1,2,4,6)]
  
  if(a.exist > 0){
    a <- d.conc[d.conc$Collection_Method!='G',]
    a <- a[c(1,2,4,6)]
  } else a <- data.frame(Station.ID=g$Station.ID,
                         Collection_Date=g$Collection_Date,
                         Collection_Method=g$Collection_Method,
                         Value=NA)
  dseq <- data.frame(Collection_Date=seq(as.Date(sdate),as.Date(edate),1))
  
  # merge g and a to pick a over g
  
  ag <- merge(dseq,a,by.x='Collection_Date',by.y='Collection_Date',all.x=T)
  names(ag)[1] <- 'Collection_Date'
  ag <- merge(ag,g,by.x='Collection_Date',by.y='Collection_Date',all.x=T)
  ag$Value <- ifelse(!is.na(ag$Value.x),ag$Value.x,ag$Value.y)
  #Test if there are values - if not return empty returns
  df.wqhold <- d.conc
  d.conc <- ag
  
  d.conc <- d.conc[which(grepl(paste(c('Collection_Date','Station.ID.x','Collection_Method.x','Value$'),collapse='|'),names(d.conc)))]
  names(d.conc) <- gsub('.x','',names(d.conc))
  
  if(length(which(!is.na(d.conc$Value)))>=3){
    
    # Set MDLs to MDL
    d.conc$Value <- ifelse(d.conc$Value<0,abs(d.conc$Value),d.conc$Value)
    
    d.conc <- cbind(WY=ifelse(month(d.conc$Collection_Date)>wy,year(d.conc$Collection_Date)+1,year(d.conc$Collection_Date)),MY=as.yearmon(d.conc$Collection_Date),d.conc)
    
    
    # MY geoMean - all data
    my.wq <- aggregate(d.conc[ncol(d.conc)],by=list(d.conc$MY),geoMean,na.rm=T)
    names(my.wq) <- c('MY',staName)
    
    # WY geoMean - all.data
    wy.wq <- aggregate(d.conc[ncol(d.conc)],by=list(d.conc$WY),geoMean,na.rm=T)
    names(wy.wq) <- c('WY',staName)
    
    # Separate Grab from Auto samples in prepare for interpolations; grabs are given
    grab.wq <- g
    names(grab.wq)[which(grepl('Collection_D',names(grab.wq)))] <- 'Collection_Date'
    grab.wq <- cbind.data.frame(MY=as.yearmon(grab.wq$Collection_Date),grab.wq)
    grab.wq <- cbind.data.frame(WY=ifelse(month(grab.wq$MY)>wy,year(grab.wq$MY)+1,year(grab.wq$MY)),grab.wq)
    # remove values >sdX10
    if(chem==25){
      #grab.wq$Value[grab.wq$Value>10*sd(grab.wq$Value,na.rm=T)] <- NA
    } else {grab.wq$Value[grab.wq$Value>15*sd(grab.wq$Value,na.rm=T)] <- NA}
    
    # MY grab geomeans
    if(nrow(grab.wq)>0){
      my.grab <- aggregate(grab.wq[ncol(grab.wq)],by=list(grab.wq$MY),geoMean,na.rm=T)
      names(my.grab) <- c('MY',staName)
    }else my.grab <- grab.wq
    
    # WY grab geomeans
    if(nrow(grab.wq)>0){
      wy.grab <- aggregate(grab.wq[ncol(grab.wq)],by=list(grab.wq$WY),geoMean,na.rm=T)
      names(wy.grab) <- c('WY',staName)
    }else wy.grab <- grab.wq
    
    # Need to test if auto samples exist - if not, skip this section
    if(a.exist>0){
      auto.wq <- a
      names(auto.wq)[which(grepl('Collection_D',names(auto.wq)))] <- 'Collection_Date'
      auto.wq <- cbind.data.frame(MY=as.yearmon(auto.wq$Collection_Date),auto.wq)
      auto.wq <- cbind.data.frame(WY=ifelse(month(auto.wq$MY)>wy,year(auto.wq$MY)+1,year(auto.wq$MY)),auto.wq)
      # remove values >sdX10
      #auto.wq$Value[auto.wq$Value>10*sd(auto.wq$Value,na.rm=T)] <- NA
    }else auto.wq <- data.frame(WY=grab.wq$WY,MY=grab.wq$MY,Station.ID=grab.wq$Station.ID,Collection_Date=grab.wq$Collection_Date,Collection_Method=grab.wq$Collection_Method,Value=NA)
    
    # MY auto geomeans
    if(a.exist>0){
      my.auto <- aggregate(auto.wq[ncol(auto.wq)],by=list(auto.wq$MY),geoMean,na.rm=T)
      names(my.auto) <- c('MY',staName)
    }else my.auto <- NA
    # WY auto geomeans
    if(a.exist>0){
      wy.auto <- aggregate(auto.wq[ncol(auto.wq)],by=list(auto.wq$WY),geoMean,na.rm=T)
      names(wy.auto) <- c('WY',staName)
    }else wy.auto <- NA
  } else {
    auto.wq <- grab.wq[1,]
    auto.wq[1,] <- NA 
    my.auto <- my.grab[1,]
    my.auto[1,] <- NA
    wy.auto <- wy.grab[1,]
    wy.auto[1,]
  }
  
  return(list(d.conc,my.wq,wy.wq,grab.wq,my.grab,wy.grab,auto.wq,my.auto,wy.auto))
} # end conc_ld

