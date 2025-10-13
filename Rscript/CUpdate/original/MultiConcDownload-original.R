# download multiple station concentrations with defined time ranges and parameters
# script provided by Dr. Donatto Surratt

rm(list = ls())

# change to your working directory
setwd('/media/otanod/148012E08012C85E/lox/r_codes')

# load libraries ----
library(EnvStats)

#
# load sources ----
source('concDBHIrevised.R',echo=T) # this model returns: 
# d.conc = [1] = daily concentrations of auto and grab combined into one time-series with preference to auto
# my.wq = [2] = month year concentrations like d.conc
# wy.wq = [3] = water year concentrations like d.conc
# grab.wq = [4] = daily grab sample conc
# my.grab = [5] = month year grab sample conc
# wy.grab = [6] = water year grab sample conc
# auto.wq = [7] = daily auto sample conc
# my.auto = [8] = month year auto sample conc
# wy.auto = [9] = water year auto sample conc

# input parameter for conc_ld function
# sdate = the start date
# edate = the end date
# param = the chemical to download by code; 25 = phosphate total; you need to look these up here: https://insightsdata.sfwmd.gov/#/waterquality -- check the Parameters drop down
# wy = water year; 0 for calendar; 9 for Oct - Sep; 4 for May - Apr; number represents the end of the year

# inputs for concentration loading ----
sdate <- '2004-01-01'
edate <- '2016-12-31'
param <- 25  # phosphate, total
wy <- 0

csta <- c('LOXA101','LOXA102','LOXA103','LOXA104','LOXA105','LOXA106','LOXA107','LOXA108','LOXA109','LOXA110','LOXA111','LOXA112','LOXA113','LOXA114','LOXA115','LOXA116','LOXA117','LOXA118','LOXA119','LOXA120','LOXA121','LOXA122','LOXA123','LOXA124','LOXA126','LOXA127','LOXA128','LOXA129','LOXA130','LOXA131','LOXA132','LOXA133','LOXA134','LOXA135','LOXA136','LOXA137','LOXA138','LOXA139','LOXA140')  # stations to load concentrations for

# load concentrations ----

i <- 1
j <- length(csta)
for(i in 1:j){
  assign(paste0(csta[i],'.daily'),conc_ld(csta[i],sdate,edate,param,wy)[1])
}
