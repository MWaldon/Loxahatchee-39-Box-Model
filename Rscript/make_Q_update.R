# This R script creates an updated structure flow dataset
# for the period beginning 7/1/2009 
library(reshape)
library(zoo)
library(lubridate)
library(sf)
library(terra)

load("../Datasets/39-Box-Datasets.Rdata") # dataset 1995-01-01 to 2009-06-30

sdate <- '2009-07-01' # start date for download
edate <- '2025-06-30' # end date for downloading

# create the empty Q update dataframe
Q.data.update <- data.frame(DATE = as.Date(as.Date(sdate):as.Date(edate)))
nup <-  length(Q.data.update$DATE) # number of days in update
Q.data.update$DAY <- Date2Day(Q.data.update$DATE)
Qempty <- matrix(nrow = nup, 
                 ncol = nstruct)
colnames(Qempty) <- struct$name
Q.data.update <- cbind(Q.data.update, Qempty)

# set diverted structures to have zero flow
# S5AS, S5A, S6
Q.data.update$S5AS <- rep(0, nup)
Q.data.update$S5A  <- rep(0, nup)
Q.data.update$S6   <- rep(0, nup)


for (i  in 1:nstruct) {
  sname <- struct$name[i]
  print(sname)
  # find values for the structures missing values
  st.col <- which(names(Q.data.update) == sname) # column number for structure
  if (is.na(Q.data.update[1,st.col])) { 
    # fetch the data from DBHYDRO
    site <- flow.formatter(sname, sdate = sdate, edate = edate,
                        wy = 4, PREF.prefer = TRUE, quiet = FALSE)
    if ( ! is.na(site[1])) { # skip if NA was returned
    
      # convert CFS to m^3/d  1 CFS = 2446.6 m^3/d 
      site$Q$m3d <- site$Q$CFS * 2446.6
      
      # place flows in dataframe matching by date
      #   remove any NA values from site$Q$Date
      site$Q <- site$Q[which( ! is.na(site$Q$Date)),]
      #   find rows in Q.data.update$DATE that match dates in site$Q$Date
      d.match <- match(site$Q$Date, Q.data.update$DATE, nomatch = 0)
      Q.data.update[d.match, st.col] <- site$Q$m3d
    } # end if
  } # end if
} # end for
