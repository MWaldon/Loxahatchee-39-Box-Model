# This script contains functions used in the 
#   Loxahatchee Refuge 39-Box model
# Functions list:
  # DEPTH
  #  Cell.Depth.calc <- function(v) # returns depth(m) as a vector 1..ncell
  #  Cell.Volume.calc <- function(d) # returns volume(m^3) as a vector 1..ncell
  #  Canal.BF.calc <- function() # calculate canal bank full parameters
  #  hydroperiod <- function(depth, fraction=TRUE, threshold=0.1) # hydroperiod
  #  depthduration <- function(depth) return duration as a function of depth

  # DATE
  #  Day2Date <- function(d)  # converts model day to date 
  #  Date2Day <- function(d)  # converts date to model day
  #  Day2TIME <- function(d)  # converts day to time at the beginning of day=d

  # FLOW TIMESERIES
  #  QoutHistoric <- function(DAY) # historic outflow  (m^3/day)
  #  Qinfunc <- function(DAY) # historic inflows (m^3/day)
  #  A1Floor <- function(DAY) # Regulation Schedule A1 floor stages
  #  QCalcOutS10 <- function(S10Stage,A1) # Regulatory release (10^6 m^3/day)
  #  QoutCalcCell <- function(S10Stage) # distribute calculated outflows
  #  QoutUsed <- function() # outflows from canal cells used in simulation

  # ET TIMESERIES
  #  Fet <- function(d) # ET reduction factor (dimensionless)

  # LINK FLOWS
  #  Link.Flow.calc <- function(Depth, Stage) { # returns link flows
  #  link2cell <- function(F) { # Link flows or mass transport
  #  sim.Velocity.calc <- function() { # link water velocity calculation
  
  # GRAPHICS
  # USGS_Stages.read <- function() { # read historic USGS gague data for graphs
  # Stage.graph <- function(gauges, cellnum) { #plot of stage at USGS gague sites
  # Stage.hist <- function(gauges, cellnum) { #hist of stage at USGS gague sites
  # marsh.map <- function(x, maptitle="") { # plot x as a marsh choropleth map

  # MAPPING & SHAPE FILES
  # sf.read <- function(Quiet=FALSE) { # reads canal, marsh, & boundary shape files
  # dist.p2p <- function(x1, x2) { # distance from point to point
  # dist.p2l <- function(x0, x1, x2) { # distance from point to line
  # dist.p2ls <- function(x0, x1, x2) { # distance from point to a line segment
  # dist.p2pg <- function(x0, pg) { # distance from point to polygon boundary
  # find.bordering <- function(x0, verts, tolerance=50) { # find cells with x0 on the border
  # bordering.list <- function(verts, tolerance=50) { # return a list of bordering cell numbers
  # bordering.n <- function(b_list) { # return vector of length of each item in list
  # vertices.list <- function(sfun) { # return the vertices in shape function
  # cell.xy <- function(i) { # return the cell xy centroid for cell i
  # links.plot <- function() { # plot links
  # point_in_cell <- function(shapefile) { # return point inside each cell
  #
  # find_polygon_line_intersection <- function(polygon, line_start, line_end)
  #   -- in file polygon_line_intersection.R
  # 
  
#
# load libraries used in functions and elsewhere
library(readxl)   # read Excel worksheets
library(sf)       # shape files
library(plotrix)  # plotting functions
library(matrixStats) # needed for colMin
library(readr)    # read csv text file
library(lubridate)# date functions


#---------------------------DEPTH-----------------------------------------------
Cell.Depth.calc <- function(v) { # returns depth(m) as a vector 1..ncell
  # uses cell and CanalVS datasets
  # global: ncanal, cell, CanalVS
  d <- rep(NA, ncell)       # create the depth vector
  c <- cell$type=='Canal'
  m <- cell$type=='Marsh'
  # marsh cell depth
  d[m] <- v[m]/cell$area[m] # marsh 
  # canal cell depth
  for (i in 1:ncanal) {
    d[i] <- approx(CanalVS$Vol[CanalVS$Cell==i],
                   CanalVS$Depth[CanalVS$Cell==i],
                   xout=v[i], method = "linear", rule= 2:1)$y
  } # end for
  return(d)
} # end Cell.Depth.calc

Cell.Volume.calc <- function(d) { # returns volume(m^3) as a vector 1..ncell
  # global: cell, CanalVS, ncanal
  # uses cell and CanalVS datasets
  v <- rep(NA, ncell)       # create the volume vector
  c <- cell$type=='Canal'
  m <- cell$type=='Marsh'
  # marsh cell volume
  v[m] <- d[m]*cell$area[m] # marsh 
  # canal cell depth
  for (i in 1:ncanal) {
    v[i] <- approx(CanalVS$Depth[CanalVS$Cell==i],
                   CanalVS$Vol[CanalVS$Cell==i],
                   xout=d[i], method = "linear", rule= 2:1)$y
  } # end for
  return(v)
} # end Cell.Volume.calc

# Bank-full canal stage, depth, volume and area
Canal.BF.calc <- function() { # calculate canal bank full parameters
  # canal is bank full (BF) when canal stage == adjacent perimeter cell
  #   soil elevation (cell#E0)
  # cell number for perimeter cell adjacent to canal cell
  #   canal cell#  1   2   3   4   5   6   7   8   9  10, 11
  # global: cell, CanalVS, ncell, ncanal
  CanalPerim <- c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32)
  CanalBFStage <- cell$E0[CanalPerim]           # (m)
  CanalEo      <- CanalVS$Stage[CanalVS$Vol==0] # (m)
  CanalBFDepth <- CanalBFStage - CanalEo        # (m)
  CanalBFVol   <- Cell.Volume.calc(c(CanalBFDepth, rep(0,ncell-ncanal)))[1:ncanal]
  # surface area = dVolume/dDepth
  CanalBFSurf <- (Cell.Volume.calc(c(CanalBFDepth+0.1, rep(0,ncell-ncanal)))[1:ncanal] -
                    CanalBFVol)/0.1               # (m^2)
  CBF <- data.frame(CanalPerim, CanalBFStage, CanalEo, 
                    CanalBFDepth, CanalBFVol, CanalBFSurf)
  names(CBF) <- c('PerimCell', 'Stage', 'E0', 'Depth', 'Vol', 'SurfArea')
  return(CBF)
} # end Canal.BF.calc

hydroperiod <- function(depth, fraction=TRUE, threshold=0.1) { 
# mumber or fraction of dry days
  # depth is a vector of daily water depths (m)
  # threshold is the water depth (m) below which the day is classed as dry
  # if fraction=TRUE then fraction as dry days/year is returned
  #   if fraction=FALSE then the number of dry days is returned
  drydays <- sum(depth < threshold)
  if (fraction) {drydays <- 365.25*drydays/length(depth)}
  return(drydays)
} # end hydroperiod

depthduration <- function(d, xout=NA) { 
  #return duration as a function of depth
  # if xout is the value of depth for which duration is returned
  #   default has xout = d
  # 
  depth <- sort(d)      # 
  n <- length(depth)    # rank of depth
  dur <- ((1:n)-0.5)/n # duration = P of equaling or exceeding depth)
  if (anyNA(xout)) { # set dd to depth duration values
    dd <- data.frame(depth,dur)
  } else {
    dd <- data.frame(xout,approx(depth, dur, xout = xout)$y)
    names(dd) <- c('depth', 'dur')
  }
  return(dd)
} # end depthduration

#_______________________DATE____________________________________________________      
Day2Date <- function(d)  # function converts model day to date 
  # day=1 is '1995-01-01', 
  # TIME=0 is at midnight on the beginning of day 1.
{
  return(as.Date(as.Date('1995-01-01')+d)-1)
}

Date2Day <- function(d)  #function converts date to model day
  # argument d may be either type Date or character
  # globals: Model.BaseDate
{ 
  d0 <- Model.BaseDate # '1995-01-01'
  d1 <- as.numeric(d0)
  if(is.character(d)) d <- as.Date(d) # if arg was character change to Date
  d2 <- as.numeric(d-d1)+1
  return(d2)
}

Day2TIME <- function(d)  # function converts day to time in days 
  # at the beginning of day=d
  return(d-1)

sim.i <- function(d) # return output matrix row subscript given a date
  # argument d may be either type Date or character
  # note that Dates[sim.i(d)] == d
  # globals: Start.Date
{
  if (is.character(d)) d <- as.Date(d)
  i <- 1 + d - Start.Date
  return(as.numeric(i))
}

#__________________FLOW TIMESERIES______________________________________________
# Timeseries 
# timeseries can extend from Model.BaseDate to Model.EndDate

QinExternal.calc <- function() { # Calculate flow boundaries for each cell
  # Set up matrix of all external inflows to each cell on each day
  # from structures + precipitation (m^3/day)
  # globals: Stop.Day, Start.Day, ncell, ncanal, PET
  QinExternal <- matrix(data = 0.0, nrow = Stop.Day-Start.Day+1, ncol = ncell)
  for (DAY in seq(Start.Day,Stop.Day, 1)) {
    Day_sub <- DAY -Start.Day +1 # row subscript
    # structure inflows
    QinExternal[Day_sub,1:ncanal] <- Qinfunc(DAY)  # canal cell structure inflows
    # precipitation
    QinExternal[Day_sub,] <- QinExternal[Day_sub,] + 
      (cell$area*PET$P[DAY]) 
  } # end for DAY loop
  return(QinExternal)
}

Qinfunc <- function(DAY) { # historic inflows (m^3/day)
  # canal inflows by cell on DAY
  # globals: ncanal, Inflow
  Q <- rep(0.0, length=ncanal)
  # use structure daily inflows in dataset Inflow
  #Q[1] 	= 0  # no inflows 
  Q[2]  	= Inflow$G301[DAY]   # G301_in
  Q[3] 	  = Inflow$G310[DAY] + 
    Inflow$G251[DAY]   # G310_in + G251_in
  Q[4]  	= Inflow$S6[DAY] + 
    Inflow$G338[DAY]   # S6_in + G338_in
  #Q[5] 	= 0
  #Q[6] 	= 0
  #Q[7] 	= 0   
  Q[8]  	= Inflow$G94A[DAY]   # G94A_in 
  Q[9]  	= Inflow$G94C[DAY]   # G94C_in
  Q[10] 	= Inflow$ACME1[DAY] + 
    Inflow$ACME2[DAY] +   # G94D == ACME2
    Inflow$S362[DAY]   # G94D_in + ACME1_in + S362_in 
  Q[11] 	= Inflow$G300[DAY] + 
    Inflow$S5AS[DAY] + 
    Inflow$S5A[DAY]   # G300_in + S5AS_in + S5A_in
  return(Q)
  # the following code used an alternative inflow dataset derived from an archived Excel spreadsheet
  #    # canal inflows by cell on DAY
  #    Q <- rep(0.0, length=ncanal)
  #    #Q[1] 	= 0  
  #    Q[2]  	= Inflow$G301_S[DAY]   # G301_in
  #    Q[3] 	  = Inflow$G310_P[DAY] + 
  #      Inflow$G251_P[DAY]   # G310_in + G251_in
  #    Q[4]  	= Inflow$S6[DAY] + 
  #      Inflow$G338_C[DAY]   # S6_in + G338_in
  #    #Q[5] 	= 0
  #    #Q[6] 	= 0
  #    #Q[7] 	= 0   
  #    Q[8]  	= Inflow$G94A_C[DAY]   # G94A_in 
  #    Q[9]  	= Inflow$G94C_C[DAY]   # G94C_in
  #    #  G94D == ACME2
  #    Q[10] 	= Inflow$ACME1[DAY] + 
  #      Inflow$ACME2[DAY] +
  #      Inflow$S362_P[DAY]   # G94D_in + ACME1_in + S362_in 
  #    Q[11] 	= Inflow$G300_S[DAY] + 
  #      Inflow$S5AS_S[DAY] + 
  #      Inflow$S5A_P[DAY]   # G300_in + S5AS_in + S5A_in
  
} # end Qinfunc

QoutHistoric <- function(DAY) { # historic outflow  (m^3/day) 
  # canal outflows by cell on DAY
  # globals: ncanal, Outflow
  Q <- rep(0.0, length=ncanal)
  #Q[1] =  0 
  Q[2] 	= Outflow$G301[DAY] # G301_out  
  #Q[3] = 0 
  Q[4] 	= Outflow$S10E[DAY] + 
    Outflow$G338[DAY] #S10E_out +G338_out
  Q[5] 	= Outflow$S10D[DAY] #S10D_out 
  Q[6] 	= Outflow$S10C[DAY] #S10C_out 
  Q[7] 	= Outflow$S10A[DAY] #S10A_out 
  Q[8] 	= Outflow$S39[DAY] + 
    Outflow$G94A[DAY] #S39_out + G94A_out
  Q[9] 	= Outflow$G94B[DAY] + 
    Outflow$G94C[DAY] #G94B_out + G94C_out
  # ****** error - flows from cell 11 not 10 (corrected mgw) *******
  # Q[10] = Outflow$S5AS[DAY] #S5AS_out  
  Q[11] = Outflow$G300[DAY] +
    Outflow$S5AS[DAY] #G300_out 
  return(Q)
} # end Qouthistoric

A1Floor <- function(DAY) { # Regulation Schedule A1 floor stages 
  # find "day of year" for DAY
  doy <- yday(Day2Date(DAY)) # day-of-year, yday is in library lubridate
  # define points on the regulation schedule zone A1 floor line
  d <- c(   0,   132,   188,  267,  334,  366) # day-of-year
  A <- c(17.2, 15.75, 15.75, 17.5, 17.5, 17.2) # stage (ft)
  # approx function uses piecewise linear interpolation (stats library)
  A1F <- approx(d,A, xout = doy, rule=1)$y # A1Floor (ft)
  A1F <- A1F*0.3048 # A1Floor converted from feet to meters (m)
  return(A1F)
} #end A1Floor  

QCalcOutS10 <- function(S10Stage,A1) { # Regulatory release (thousand m3/d) 
  #   as a function of difference between stage and A1 zone floor (ft).
  # This is based on historic S10 flow 1/1/1995 - 8/31/2007
  #   (initially copied from file CA1-elevations.xls) 
  
  #deltaS <- (S10Stage-A1)/0.3048  # convert meters to feet
  # units of x = feet, units of y = 10^3 cubic meters^3/day
  #y3 <- 8116 + (2*(8116-6356)/(1-0.9)) # linear extension to 3 ft
  #x <- c(-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,
  #        0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,3)
  #y <- c(0,32,113,64,94,148,311,238,175,328,288,450,765,957,1323,2274,
  #       3188,2832,4469,5901,7406,6602,6356,8116,y3)
  #Q <- approx(x, y, xout = deltaS, rule=2)$y # (10^3 m^3/day)
  # Reduction factor & Scale up S10 regulatory flows by ratio of 
  #   total regulatory release/S10
  # Q <- 0.2457*RSQfact*(196.7/144.8)*Q # (10^3 m^3/day)
  # Q <- 1000*Q                       #  # (m^3/day)
  #  convert thousand m3/d to m3/d
  
  # Alternative using fitted equation
  # This polynomial approximation was fitted to [11:21] of the piecewise
  #   linear x-y function that was used by Berkeley Madonna. 
  #   See script S10RuleAnalysis.R model ymod21.
  deltaS <- S10Stage-A1
  if (deltaS < -0.1) { # see script S10RuleAnalysis.R
    Q <- 0
  } else {
    Q <- 0.9541909 + (12.9026725*deltaS) + (79.9679408*(deltaS^2))}
  #Q <- rep(0, length(deltaS))
  #Q[S10Stage >= -0.1] <-  
  #  0.9541909 + (12.9026725*deltaS) + (79.9679408*(deltaS^2))
  return(Q) # total regulatory release flow (10^6 m^3/day)
  
} # end QCalcOutS10

QoutCalcCell <- function(S10Stage) { # distribute calculated outflows
  # among canal cells
  # globals: ncanal, RSQfact
  QoutCalc <- rep(NA,ncanal)
  #Qx <- 1000*(((2*0.2457)+0.2629)/((3*0.2457)+0.2629))*(196.7/144.8)
  #Qc <- Qx*RSQfact*QCalcOutS10(S10Stage,A1)
  Qc <- RSQfact*QCalcOutS10(S10Stage,A1)*1000000 # (m^3/day)
  QoutCalc[5:7]  <-  0.2457*Qc # (m3/day)
  QoutCalc[8]    <-  0.2629*Qc    # (m3/day)
  # QoutCalc[5]    <-  0  for test
  return(QoutCalc)
} # end QoutCalcCell 

QoutUsed <- function() { # outflows from canal cells used in simulation
  # globals: Qout, CalcQRo, QoutCalc, NonReg, DAY
  # historic structure outflow 
  # Q is a vector of canal cell outflows
  #   as outflows, Q values are negative
  Q <- -Qout # use negative for outflow
  # Regulatory release, calculated output
  # Canal cells 5, 6, 7, and 8 correspond to the S-10 & S-39 structures
  # canal cells:  5 (S10D), 6 (S10C), 7 (S10A), 8 (S39) 
  #   are involved in regulation schedule
  #   only S39 has water supply outflow when 1-8C WL < A1Floor
  # When CalcQRo is TRUE, regulatory outflow through these structures is
  #   calculated using the Refuge regulation schedule.
  if (CalcQRo == TRUE) {
    # Calculate QoutCalc 5, 6, 7, & 8
    # RSQfact can be used in options 2 and 3 to reduce the 
    #  intensity of the regulatory release (dimensionless)
    # stage at the USGS 1-8C gage is used by the regulation schedule
    # moved QoutCalc calculation from derivs to top of daily for loop 
    ## S10Stage <- Stage[9] # calculate using stage at 1-8C
    # keep historical outflow for C1, C2, C3, C4, C9, C10, and C11
    ## QoutCalc <- QoutCalcCell(S10Stage)
    #QoutCalc[5:7]  <-  0.2457*RSQfact*(196.7/144.8)*
    #                  1000*QCalcOutS10(S10Stage,A1) # (m3/day)
    #QoutCalc[8]    <-  0.2629*RSQfact*(196.7/144.8)*
    #                  1000*QCalcOutS10(S10Stage,A1)    # (m3/day)
    # replace historic flows with calculated flows (negative for outflow)
    Q[5] <- -(QoutCalc[5] + NonReg$S10D[DAY]) # hurricane deviation
    Q[6] <- -(QoutCalc[6] + NonReg$S10C[DAY]) # hurricane deviation
    Q[7] <- -(QoutCalc[7] + NonReg$S10A[DAY]) # hurricane deviation
    Q[8] <- -(QoutCalc[8] + NonReg$S39[DAY]) # S39 water supply
  } # end if CalcQRo
  return(Q) # end QoutUsed (m^3/day)
  
}

Fet <- function(d) { # ET reduction factor (dimensionless) 
  # as a function of depth d (m)
  # globals: Het, ETmin
  f <- pmin(1, d/Het)   # ET reduction for depth < Het 
  f <- pmax(ETmin, f)   # ET reduction for depth > ETmin
  return(f)
} # Fet

#__________________LINK FLOWS___________________________________________________
Link.Flow.calc <- function(Depth, Stage) { # returns link flows
  # flow is from link$up[i] to link$dn[i] (m^3/day)
  # Depth is vector of cell depths, Stage is vector of cell stages (m)
  # globals: link, B
  ldepth <- Depth[link$dn]  # depth at downstream end of link (m)
  dStage <- Stage[link$up] - Stage[link$dn]  # link stage change (m)
  # use wetland discharge power law (Kadlec and Knight, 1996)
  #   Discharge = (flow width)*(stage gradient)*(flow depth)^3
  #     stage gradient = (stage drop)/(flow length)
  Q <- 10000000*B$val*link$Width*(ldepth^3)*(dStage/link$Radius)
  return(Q) # (m^3/day)
}

link2cell <- function(F) { # Link flows or mass transport
  # are added/subtracted to/from cells
  # F has dimension 1:nlink
  # globals: ncell, nlink
  dm <- rep(0, ncell) # derivative caused by inflow/outflow
  for (i in 1:nlink) { 
    # transport is from link$up to link$dn
    dm[link$dn[i]] <- dm[link$dn[i]] + F[i]
    dm[link$up[i]] <- dm[link$up[i]] - F[i]
  } # end for i
  return(dm) # net +/- link flow to cells (m^3/day), dm length is ncell
} # end link2cell

sim.Velocity.calc <- function() { # link water velocity calculation
  #  velocity (m/day) = discharge (m^3/day) / cross section area (m^2)
  #  globals: sim.Linkflow, link, sim.Depth, nlink, nr,nX
  # create matricies with the same dimensions as sim.Linkflow
  A <- array(data=NA, dim=c(nr, nlink, nX)) # same as sim.Linkflow
  v <- A
  for (i in 1:nlink) { # for each link calculate the vector of daily velocities 
    # u <- link$up[i]  # not used
    d <- link$dn[i]    # downstream cell number
    w <- link$Width[i] 
    for (j in 1:nX) {
      # cross sectional area A is depth*width
      A[,i,j] <- sim.Depth[,d,j]*w # using downstream depth for area
      v[,i,j] <- sim.Linkflow[,i,j]/A[,i,j]   # velocity v = Q/A (m/day)
    } # end for j
  } # end for i
  return(v)
} # end sim.Velocity.calc

#__________________Graphics_____________________________________________________

USGS_Stages.read <- function() { # read historic USGS gague data for graphs
  # globals: none
  USGS_Stages <- read_excel("../DataSets/Water_Level_timeseries.xlsx", 
                            sheet = "Timeseries", range = "Q4:V5299")
  USGS_Stages[USGS_Stages == -999] <- NA # replace -999 stage with NA
  USGS_Stages$DATE <- as.Date(as.Date('1995-01-01'):as.Date('2009-06-30'))
  # add datum correction estimate
  USGS_Stages$North <- USGS_Stages$North + 0.48
  USGS_Stages$South <- USGS_Stages$South + 0.48
  
  # read update
  USGS_Stage_Update_dld <- read_csv( # read downloaded USGS gage data 
    "../DataSets/USGSupdate/USGS_Stage_Update.csv", 
    col_types = cols(SOURCE_TIMESERIES = col_character()), 
    skip = 45)
  USGS_Stage.dbkeys <- unique(USGS_Stage_Update_dld$TIMESERIESID)
  
  # DBKEYS: "15809" "AO104" "FE775" "FE776" "FE777" "MW671" "RW494"
  db_15809 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "15809",]
  db_AO104 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "AO104",]
  db_FE775 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "FE775",]
  db_FE776 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "FE776",]
  db_FE777 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "FE777",]
  db_MW671 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "MW671",]
  db_RW494 <- USGS_Stage_Update_dld[USGS_Stage_Update_dld$TIMESERIESID == "RW494",]
  
  rm(USGS_Stage_Update_dld) #cleanup, no longer needed
  rm(db_AO104)  # 1-8T gage, datum is STG88, will instead use db_15809 
  
  # check if remaining DATE columns are all equal (must fix if not equal)
  #  all have length of 5936
    x <- identical(db_15809$TIMESTAMP,db_FE775$TIMESTAMP) &
       identical(db_15809$TIMESTAMP,db_FE776$TIMESTAMP) &
       identical(db_15809$TIMESTAMP,db_FE777$TIMESTAMP) &
       identical(db_15809$TIMESTAMP,db_MW671$TIMESTAMP) &
       identical(db_15809$TIMESTAMP,db_RW494$TIMESTAMP) 
  if (!x) { # if all date vectors are not equal
    print('ERROR date vectoors do not match')
    } # end if 
  
  # Create final update dataframe
  # STATION  "GS8T"  "GS7"   "GS8C"  "GS9"   "South" "North" "DATE" 
  # DBKEY    15809  FE775    FE776  FE777     MW671   RW494
  Supdate <- data.frame(GS8T=db_15809$VALUE,
                        GS7 =db_FE775$VALUE,
                        GS8C=db_FE776$VALUE,
                        GS9 =db_FE777$VALUE,
                        South=db_MW671$VALUE,
                        North=db_RW494$VALUE,
                        DATE =db_15809$TIMESTAMP)
  # convert all stages from feet to meters by dividing by 3.28084 ft/m
  Supdate[,-7] <- Supdate[,-7]/3.28084
  
  USGS_Stages <- rbind(USGS_Stages, Supdate) # combine old and updated stages
  
  # North and South gages are missing in download because they are not NGVD 29
  # get data from USGS_Stage_Update.obs loaded by updated make_datasets.R
  #   USGS_Stage_Update.obs ends June 30, 2025, USGS_Stages ends September 30
  USGS_Stages$North[1:dim(USGS_Stage_Update.obs)[1]] <- USGS_Stage_Update.obs$North
  USGS_Stages$South[1:dim(USGS_Stage_Update.obs)[1]] <- USGS_Stage_Update.obs$South
  
  return(USGS_Stages)
} # end USGS_Stages.read

# USGS_Stages <- USGS_Stages.read()
# plot(USGS_Stages$DATE,USGS_Stages$GS8C, type='l')
# lines(USGS_Stages$DATE, A1Floor(Date2Day(USGS_Stages$DATE)), col='red')

Stage.graph <- function(gauges, cellnum) { #plot of stage at USGS gage sites
  # gagues is a vector of 1 or 2 gauges for plotting
  # cellnum is a cell number for plotting
  # globals: cell, USGS_Stages Start.Date, Stop.Date
  y <- subset(USGS_Stages, select = gauges[1])[[1]] # stages for 1st gague
  ylabel <- paste(gauges, collapse = ", ") # concatenate gauge names
  ylabel <- paste(ylabel, " stage (m)")
  plot(USGS_Stages$DATE, y, type='l', col='black',
       ylab=ylabel, 
       ylim = c(4,6), xlab='Date')
  # draw a vertical line at approx. start date of current regulation schedule
  lines(c(RSched.start,RSched.start), c(0.0,10.0), col='black', lty = 3)
  if(length(gauges)>1) {
    y <- subset(USGS_Stages, select = gauges[2])[[1]] # stages for 2nd gague
    lines(USGS_Stages$DATE, y, type='l', col='blue')
  }
  # plot a horizontal line at the soil elevation for marsh & bankfull for canal
  if (cellnum>ncanal){ nb <- cellnum } else 
                     { nb <- Canal.BF[cellnum,]$PerimCell}
  lines(USGS_Stages$DATE, rep(cell$E0[nb], length(USGS_Stages$DATE)), 
        col='green', lty = 2)
  # plot the simulated stage
  lines(as.Date(Start.Date:Stop.Date), sim.Stage.da[,cellnum], col='red')
} # end Stage.graph

Stage.hist <- function(gauges, cellnum) { #hist of stage at USGS gague sites
  # gagues is a vector of 1 or 2 gauges for plotting
  # cellnum is a cell number for plotting
  # globals: cell, USGS_Stages Start.Date, Stop.Date
  y <- subset(USGS_Stages, select = gauges[1])[[1]] # stages for 1st gague
  # select period of simulation
  y <- y[USGS_Stages$DATE>=Start.Date & USGS_Stages$DATE<=Stop.Date]
  y <- y[! is.na(y)] # remove missing values
  if (length(y)>30) { # skip histogram if too few values
    ylabel <- paste(gauges, collapse = ", ") # concatenate gauge names
    ylabel <- paste(ylabel, " stage pdf (1/m)")
    xlimit = c(4,6)
    hist(y, border='black', col = 'gray',
         ylab=ylabel, 
         xlim = xlimit, xlab='Stage (m)',
         main = '', freq = FALSE)
  } # end if
  if(length(gauges)>1) {
    y2 <- subset(USGS_Stages, select = gauges[2])[[1]] # stages for 2nd gague
    # select period of simulation
    y2 <- y2[USGS_Stages$DATE>=Start.Date & USGS_Stages$DATE<=Stop.Date] 
    y2 <- y2[! is.na(y)] # remove missing values
    if (length(y2)>30) { # skip histogram if too few values
      hist(y2, border ='blue', col = NA, 
           add = TRUE, freq = FALSE)
    } # end if
  } # end if
  # plot a vertical line at the soil elevation for marsh & bankfull for canal
  if (cellnum>ncanal){ nb <- cellnum } else 
                     { nb <- Canal.BF[cellnum,]$PerimCell}
  lines(rep(cell$E0[nb], 2), c(0.0, 999.),
        col='green', lty = 2)
  # plot the simulated stage
  hist(sim.Stage.da[,cellnum][!is.na(y)], border ='red', col = NA, 
       add = TRUE, freq = FALSE)
} # end Stage.hist

marsh.map <- function(x, maptitle="") { # plot x as a marsh choropleth map
  # create a temporary copy of the marsh shape file
  # x is a vector with ncell values
  # globals: marsh_sf
  marsh_sf_temp <- marsh_sf
  marsh_sf_temp$x <- x
  plot(marsh_sf_temp["x"], main = maptitle) # see help plot_sf
} # end marsh.map

#__________________Mapping & Shape Files________________________________________
# read and plot shape files

sf.read <- function(Quiet=FALSE) { # reads canal, marsh, & boundary shape files
  library(sf) # library containing _sf functions
  # function read_sf now preferred over st_read
  # this function sets the following global variables (uses <<-) 
  #    canal_sf, marsh_sf, marsh_boundary_sf, Id2icell, cell.plotxy
  canal_sf          <<- read_sf("../Maps/New_canal_cells.shp", quiet = Quiet)
  marsh_sf          <<- read_sf("../Maps/New_marsh_cells.shp", quiet = Quiet)
  marsh_boundary_sf <<- read_sf("../Maps/New_marsh_boundary.shp", quiet = Quiet)
  
  # The variable Id in the shape marsh files is not the same as the model cell 
  #   number. Id2icell is a map between Id and model cell number icell.
  Id2icell <<- data.frame(Id=c(1:11,1:28), icell= c(1:11,  # canal
                           14, 32, 12, 13, 15, 33, 16, 17, # Marsh Id 1-8
                           30, 31, 18, 19, 20, 21, 22, 23, # Marsh Id 9-16
                           24, 25, 26, 27, 28, 29, 34, 35, # Marsh Id 17-24
                           36, 37, 38, 39                  # Marsh Id 25-28
  ))
  Id2icell$type <<- c(rep('c',ncanal),rep('m', ncell-ncanal))
  
  # add icell variable to canal and marsh shape files
  canal_sf$icell <<- Id2icell[1:ncanal,]$icell
  marsh_sf$icell <<- Id2icell[(ncanal+1):ncell,]$icell
  
  # set cell plotting xy as a point inside the cell
  #   (note that the cell centroid the centroid is not guaranteed 
  #    to be inside the polygon, especially for concave  canal polygons)
  cell.plotxy <<- matrix(data = NA, nrow = ncell, ncol = 2)
  # Id2icell maps the polygon Id number to cell number
  cell.plotxy[Id2icell$icell[1:ncanal], ]         <<- point_in_cell(canal_sf)
  cell.plotxy[Id2icell$icell[(ncanal+1):ncell], ] <<- point_in_cell(marsh_sf)
  
  vertices <<- vertices.list(marsh_sf) # get all marsh polygon vertices
  # all values are returned as globals
  return(TRUE)
} # end sf.read

dist.p2p <- function(x1, x2) { # distance from point to point
  # x1 and x2 are vectors with (x, y) coordinates
  dx2 <- (x1[1]-x2[1])^2
  dy2 <- (x1[2]-x2[2])^2
  d <- sqrt(dx2+dy2)
  return(d)
} # end dist.p2p

dist.p2l <- function(x0, x1, x2) { # distance from point to line
  # x0 x1 and x2 are vectors with (x, y) coordinates
  # x0 is point, x1 and x2 define the line
  # see Wikipedia article "Distance from a point to a line"
  d12 <- dist.p2p(x1,x2) # distance between x1 and x2
  if (d12==0) { # x1 and x2 are the same point
    d <- dist.p2p(x0,x1)
  }
  else { # x1 != x2
    x <- 1
    y <- 2
    dx <- x2[x]-x1[x]
    dy <- x2[y]-x1[y]
    # a2 is twice the area of the triangle with its vertices at the three points
    a2 <- abs((dy*x0[x]) - (dx*x0[y]) + (x2[x]*x1[y]) - (x2[y]*x1[x]))
    d <- a2/d12 # height of triangle is twice the area divided by the base
  }
  return(d)
}

dist.p2ls <- function(x0, x1, x2) { # distance from point to a line segment
  # x0 x1 and x2 are vectors with (x, y) coordinates
  # x0 is point, x1 and x2 define the line segmant ends
  d01 <- dist.p2p(x0,x1)     # distance x0 to x1
  d02 <- dist.p2p(x0,x2)     # distance x0 to x2
  d12 <- dist.p2p(x1,x2)     # distance x1 to x2
  d0 <- dist.p2l(x0, x1, x2) # shortest distance x0 to line through x1,x2
  dh <- sqrt(d12^2 + d0^2)   # hypotenuse when x0, x1, x2 is a right triangle
  perim90 <- d0+d12+dh       # perimeter x0, x1, x2 when its a right triangle
  perim <- d01+d12+d02       # actual perimeter x0, x1, x2
  # projection is outside of line segment then perimeter is larger than perim90
  if(perim>perim90) { 
    # use shortest distance to an end point
    d <- min(d01,d02)
  }
  else { # projection is on the line segment
    # use shortest distance to line
    d <- d0 
  }
  return(d)
}

dist.p2pg <- function(x0, pg) { # distance from point to polygon boundary
  # x0 is a vector with (x, y) coordinates
  # pg is a matrix of coordinates of polygon vertices (x,y)
  n <- dim(pg)[1] # number of vertices
  if (n==1) { # pg is a single point
    d <- dist.p2p(x0, pg)
    return(d) # return point to point distance and exit function
  }
  if (!all(pg[1,]==pg[n,])) { # close the polygon is not closed
    n <- n+1
    pg <- rbind(pg, pg[1,])
  } # end if
  d <- 1E10 # initialize to a big number
  for (i in 1:(n-1)) { # loop through polygon line segments
    dtest <- dist.p2ls(x0,pg[i,], pg[i+1,])
    d <- min(d, dtest)
  } # end for
  return(d)
} # end function dist.p2pg

find.bordering <- function(x0, verts, tolerance=50) { # find cells with x0 on the border
  # x0 has x,y coordinates of test location
  # verts is a dataframe of polygon vertices (created in Marsh.Map.contour.R)
  # tolerance is distance in meters considered to be on the border
  # returns vector of cell numbers where x0 is on the border of the cell
  # globals: verts, ncanal, ncell
  nm <- ncell-ncanal # number of marsh cells
  nvert <- dim(verts$x) # number of vertices
  bordercells <- c() # initialize vector of border cell numbers
  nborder <- 0       # initialize number of bordering cells
  for (i in 1:nm) { # loop through all marsh cell Id values
    pgdf <- verts[verts$Id==i,] # polygon i dataframe
    pg   <- cbind(pgdf$x, pgdf$y) # polygon coordinate matrix
    d <- dist.p2pg(x0,pg)
    if (abs(d)<tolerance) { # if x0 is on the border of pg
      nborder <- nborder+1 
      # append the newly found cell number
      bordercells <- c(bordercells, Id2icell[ncanal+i,2]) 
    } # end if
  } # end for
  return(bordercells)
} # end find.bordering

bordering.list <- function(verts, tolerance=50) { # return a list of bordering cell numbers
  # verts is a dataframe of polygon vertices (created in Marsh.Map.contour.R)
  n <- dim(verts)[1] # number of vertices
  b_list <- vector("list", n) # create list with n items
  for (i in 1:n) { # fill in the list
    b_list[[i]] <- find.bordering(c(verts$x[i],verts$y[i]), verts, tolerance)
    # points(verts$x[i],verts$y[i], 
    #        col=length(my_list[[i]]), cex=length(my_list[[i]]))
  } # end for i
  return(b_list)
} # end bordering.list

bordering.n <- function(b_list) { # return vector of length of each item in list
  # b_list is a list of vectors
  nv <- length(b_list) # number of items in the list
  x <- rep(NA,nv)      # initialize vector x
  for (i in 1:nv) x[i] <- length(b_list[[i]]) # length of each item
  return(x)
} # end function bordering.n

vertices.list <- function(sfun) { # return the vertices in shape function
  # return all polygon vertices in a dataframe
  # library(dplyr)      # For sorting function arrange (no longer used)
  
  mgeo <- sfun$geometry # marsh cell geometry
  vertices <- NULL # initialize list of vertices
  for (i in 1:(ncell-ncanal)) { # append vertices from each polygon to vertices
    vert <- mgeo[[i]]        # extract the ith polygon
    vert <- as.matrix(vert)
    nv <- dim(vert)[1] # number of vertices in vert
    ic <- sfun$icell[i]
    vert <- cbind(i, ic,1:nv, vert)    # add a cell Id and icell column equal to i
    vertices <- rbind(vertices,vert) # append to rows of vertices
  }
  vertices <- data.frame(vertices)   # convert to dataframe
  names(vertices) <- c('Id','icell','n','x','y')
  ic <- sfun$icell
  vertices$icell <- ic[vertices$Id]    # add marsh cell number column
  # add bordering cells information
  b_list <- bordering.list(vertices, # list of vectors of border cell numbers
                           tolerance = 50) # bordering if within tolerance (m)
  b_n    <- bordering.n(b_list) # number of border cells
  vertices$b_n    <- b_n 
  vertices$b_list <- b_list
  return(vertices)
  
} # end vertices.list
# plot vertices to test function
# vertices <- vertices.list(marsh_sf)
# plot(vertices$x,vertices$y)
# for (i in 1:(ncell-ncanal)) {
#   vert <- vertices$Id==i
#   lines(vertices$x[vert],vertices$y[vert], col='yellow')
# } # end for i


cell.xy <- function(i, centroid = TRUE) { # return cell xy 
  # default xy is centroid for cell i
  # if centroid==FALSE then return xy plotting point
  # i is a single value, not a vector
  # globals: ncanal, Id2icell, canal_sf, marsh_sf, cell.plotxy
  if (centroid) { # default, return centroid 
    Id <- Id2icell[Id2icell$icell==i,]$Id # map cell number to cell Id
    if (i>ncanal) { # marsh cell
      x <- marsh_sf$cen_X[Id] # 'cen' was used in marsh_sf
      y <- marsh_sf$cen_Y[Id]
    }
    else { # canal cell
      x <- canal_sf$Cen_X[Id] # note: Cen not cen was used in canal_sf
      y <- canal_sf$Cen_Y[Id] 
    } # end if else canal/marsh
    xy <- c(x,y) # centroid coordinate pair
  } # end if 
  else { # centroid==FALSE
    # cell.plotxy is calculated in function sf.read
    xy <- cell.plotxy[i,] # plotting point coordinate pair
    }
  names(xy) <- c('x','y')
  return(xy) # return cell x y coordinates
} # end cell.xy

links.plot <- function() { # plot links
  # plot the polygon borders
  # globals: marsh_sf, canal_sf, nlink, link, cell
  plot(marsh_sf$geometry, lwd = 1, border = 'gray') #, graticule = TRUE)
  plot(canal_sf$geometry, lwd = 1, border = 'black' , add = TRUE)
  # plot the marsh polygon centroids
  points(marsh_sf$cen_X,marsh_sf$cen_Y, col='red', cex=3)
  # plot the canal polygon centroids
  points(canal_sf$Cen_X,canal_sf$Cen_Y, col='darkgreen', cex=2, pch = 19)
  
  for (i in 1:nlink) {
    u <- link$up[i]
    d <- link$dn[i]
    xyu <- cell.xy(u)
    xyd <- cell.xy(d)
    # draw arrow from x0,y0 to x1,y1, code=2 puts arrowhead at x1,y1 
    arrows(xyu['x'], xyu['y'], xyd['x'], xyd['y'],
           code=2, length = 0.20, angle = 10, col = 'blue', lwd = 2)
  } # end for 
} # end links.plot

point_in_cell <- function(shapefile) { # return point inside each cell
  # returns xy coordinates of a characteristic point in the cell polygons
  # uses sf library function st_point_on_surface
  #  This algorithm involves finding a horizontal scan line intersecting 
  #  the polygon and then picking a point on the widest interior segment 
  #  of that line.
  
  n <- dim(shapefile)[1] # number of cells in shapefile
  p <- matrix(data=NA, nrow=n, ncol=2) # matrix of point coordinates
  pos <- st_point_on_surface(shapefile)$geometry # list of n points
  for (i in 1:n) {
    p[i,] <- pos[[i]] # put point coordinates into the result matrix
  }
  return(p)
} # end point_in_cell


#__________________end of script 39BoxFunctions.R#______________________________
print("39-Box Model functions sourced")