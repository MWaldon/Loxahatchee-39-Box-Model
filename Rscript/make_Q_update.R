# This R script creates an updated structure flow dataframe for use in
#  updating the make_datasets.R script. This script downloads updated flows
#  for the period beginning 7/1/2009 .
library(reshape)
library(zoo)
library(lubridate)
library(sf)
library(terra)

source('flowFormatter.R') # functions for downloading & processing DBHYDRO data

load("../Datasets/39-Box-Datasets.Rdata") # dataset 1995-01-01 to 2009-06-30

sdate <- '2009-07-01' # start date for download
edate <- '2025-06-30' # end date for downloading

# function to create the empty Q update dataframe
  Q.data.new <- function(sdate, edate, struct) {
    Q.data.update <- data.frame(DATE = as.Date(as.Date(sdate):as.Date(edate)))
    nup <-     length(Q.data.update$DATE) # number of days in update
    nstruct <- length(struct$name)        # number of flow structures
    Q.data.update$DAY <- Date2Day(Q.data.update$DATE) # add the DAY column
    Qempty <- matrix(nrow = nup, 
                     ncol = nstruct)
    colnames(Qempty) <- struct$name
    Q.data.update <- cbind(Q.data.update, Qempty)
    return(Q.data.update)
    } # end function Q.data.new
  
# function to add a DBKEY timeseries to the Q dataframe 
  Q.data.add <- function(DBKEY, Q.data.update, sdate, edate, DBKEYS) {
    Qdupdate <- Q.data.update
    tseries <- DBKEYS[DBKEYS$DBKEY == DBKEY,]
    site <- flow.formatter(tseries$Site, sdate = sdate, edate = edate,
                           DBKEY.select = DBKEY,
                           wy = 4, quiet = FALSE)
    # add the new flow timeseries into the Q dataframe
    if (is.na(site[1])) {
      print('no data added')
    }
    else {
      d.match <- match(site$Q$Date, Q.data.update$DATE, nomatch = 0)
      st.col <- which(site$structName == names(Q.data.update))
      Qdupdate[d.match, st.col] <- site$Q$m3d
    }
    return(Qdupdate)
  } # end function Q.data.add
  
# function to replace all NA values in Q.data.update 
  Q.data.fill.na <- function(Qdupdate, fillwith = 'zero') {
    # currently the only option is to fill with zeros
    # find all missing values
    iQmissing <- is.na(Qdupdate) # logicals TRUE if initially missing flow values
    iQmissing <- data.frame(iQmissing)
    iQmissing$DATE <- Qdupdate$DATE # restore the DATE and DAY
    iQmissing$DAY <-  Qdupdate$DAY
    
    nQmissing <- colSums(iQmissing[,-1:-2]) #named vector, number of missing Q
    
    Q <- Qdupdate

    if (fillwith == 'zero') { # set missing flow values to zero
      for (i in 1:dim(Q)[1]) { # rows
        for (j in 3:dim(Q)[2]) { # data columns
          if (is.na(Q[i,j])) { 
            Q[i,j] <- 0 
            }
        } # end for j
      } # end for i
    } # end if
    Q.fill <- list(Q, nQmissing)
    names(Q.fill) <- c('Q', 'nQmissing')
    return(Q.fill)
  } # end function Q.data.fill.na
    
  
  
# ****** BEGIN ******
  # This if statement prevents the following lines from running when you 
  #   source the file. To rerun the download the user should run the lines
  #   one at a time using the Run button because some downloads may fail
  #   and need to be repeated until successful.
  
if (FALSE) { 
    
  # initialize 
  Q.data.update <-  Q.data.new(sdate,edate,struct) # build empty flow dataframe
  
# set diverted structures to zero flow: S5AS, S5A, S6
  nup <-     length(Q.data.update$DATE) # number of days in update
  Q.data.update$S5AS <- rep(0, nup)
  Q.data.update$S5A  <- rep(0, nup)
  Q.data.update$S6   <- rep(0, nup)

# get metadata for flow timeseries
  DBKEYS <- dbkeys.Q.read() 
  
  
# download the structure timeseries from the selected DBKEY
# "S39" 91598 positive flows are out of the Refuge
  Q.data.update <- Q.data.add('91598', Q.data.update, sdate, edate, DBKEYS)
  Q.data.update$S39 <- -Q.data.update$S39 # make positive in/negative out
  # missing on "2011-08-13" "2011-08-14" "2011-08-15" 
  #            "2021-07-09" "2021-07-10" "2021-07-11"
  # iQmissing$S39 <- which(is.na(Q.data.update$S39)) #  774  775  776 4392 4393 4394
  # Q.data.update$S39[iQmissing$S39] <- 0

# SITE   DBKEY
# "G94A" 91281
  Q.data.update <- Q.data.add('91281', Q.data.update, sdate, edate, DBKEYS)

# "G94B" 91282
  Q.data.update <- Q.data.add('91282', Q.data.update, sdate, edate, DBKEYS)
  
# "G94C" 91283
  Q.data.update <- Q.data.add('91283', Q.data.update, sdate, edate, DBKEYS)
  
# "ACME2" 15023
  Q.data.update <- Q.data.add('15023', Q.data.update, sdate, edate, DBKEYS)
  
# "ACME1" 90850
  Q.data.update$ACME1 <- rep(0, nup) # no flow since 2006
  
# "S362" 91517
  Q.data.update <- Q.data.add('91517', Q.data.update, sdate, edate, DBKEYS)
  
# "G300" 90939
  Q.data.update <- Q.data.add('90939', Q.data.update, sdate, edate, DBKEYS)
  
# "S5AS" no inflow since G300
  
# "S5A"  no inflow since STA1W G300/G301
  
# "G301" 90940
  Q.data.update <- Q.data.add('90940', Q.data.update, sdate, edate, DBKEYS)
  
# "G310" 90973
  Q.data.update <- Q.data.add('90973', Q.data.update, sdate, edate, DBKEYS)
  
# "G251" 90934
  Q.data.update <- Q.data.add('90934', Q.data.update, sdate, edate, DBKEYS)
  
# "S6"   diverted to STA2
  
# "G338" 91012 positive flows are into the Refuge
  Q.data.update <- Q.data.add('91012', Q.data.update, sdate, edate, DBKEYS)
  
# "S10E" deconstructed, no flow
  Q.data.update$S10E   <- rep(0, nup)

# "S10D"  TA421 positive flows are out of the Refuge
  Q.data.update <- Q.data.add('TA421', Q.data.update, sdate, edate, DBKEYS)
  Q.data.update$S10D <- -Q.data.update$S10D # make positive in/negative out
  
# "S10C" TA420 positive flows are out of the Refuge
  Q.data.update <- Q.data.add('TA420', Q.data.update, sdate, edate, DBKEYS)
  Q.data.update$S10C <- -Q.data.update$S10C # make positive in/negative out
  
# "S10A" TA419 positive flows are out of the Refuge
  Q.data.update <- Q.data.add('TA419', Q.data.update, sdate, edate, DBKEYS)
  Q.data.update$S10A <- -Q.data.update$S10A # make positive in/negative out

  
Q.data.raw <- Q.data.update # flow data as downloaded including missing values

# remove missing values
  Q.fill <- Q.data.fill.na((Q.data.raw))
  Q.data.update <- Q.fill$Q
  nQmissing     <- Q.fill$nQmissing
  rm(Q.fill) # cleanup
  
# store as .Rdata and as csv
  Qfile <- '../DataSets/Qupdate/Qdataupdate'
  save(Q.data.update, Q.data.raw, nQmissing,
       file = paste(Qfile, '.Rdata', sep = ''))
  write.csv(Q.data.update, paste(Qfile, '.csv', sep = ''), row.names = FALSE)
  
  
# --------------------------------------------  
} # end if FALSE
  