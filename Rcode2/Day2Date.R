# these functions have been moved to the Functions script 1/29/25


Day2Date <- function(d)  
# function converts model day to date 
# day=1 is '1995-01-01', 
# TIME=0 is at midnight on the beginning of day 1.
  {
  return(as.Date(as.Date('1995-01-01')+d)-1)
}

Date2Day <- function(d) 
#function converts date to model day
# argument d may be either type Date or character
  { 
  d0 <- as.Date('1995-01-01')
  d1 <- as.numeric(d0)
  if(is.character(d)) d <- as.Date(d) # if arg was character change to Date
  d2 <- as.numeric(d-d1)+1
  return(d2)
}

Day2TIME <- function(d)
# function converts day to time in days at the beginning of day=d
  return(d-1)
