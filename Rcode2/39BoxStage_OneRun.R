# This OneRun fork tries running the simulation as a single call to the solver. 
# It still has an error and is abandoned.
# It was learned in developing this fork that the solver is calling the
# derivative routine many many times. Adjusting the error relative and
# absolute error parameters used by the solver was needed. The default
# values are too small.
# 
# The 39-Box model simulates water hydrology and water quality in the 
# A.R.M. Loxahatchee National Wildlife Refuge. It was first developed using
# the simulation environment Berkeley-Madonna by researchers at the University
# of Louisiana at Lafayette in cooperation with the U.S. Fish and Wildlife
# Service. 
# Original development team members include:
# Mike Waldon, Ehab Meselhe, William Roth, Tracy Chen and Hamid Bazgirkhoob
#
# Metric units are used in this model.
#   Unit conversion: 1 g/m3 = 1 mg/L = 1 ppm = 1000 ppb,
#                    1 ppb  = 1 ug/L
#   time unit in calculations is day
#
#_____________________________________________________________________________
#                         PREAMBLE
# remove all variables from the environment except filename
  rm(list = ls()[! ls() %in% "filename"])

  # reminder - install packages (Tools tab) before loading libraries
  library(chron)    # date functions
  library(stats)    # statistics functions
  library(deSolve)  # solvers for ordinary differential equations
  library(lubridate)# additional date functions
  library(dplyr)    # data manipulation
  library(readxl)   # read Excel worksheets

#_____________________________________________________________________________
#                         PARTIAL LIST OF VARIABLES
# Simulation control, date, and time
#   TIME              simulated time in days after 1/1/1995
#   DAY               integer part of (Time+1), day=1 on 1/1/1995
#
# geometry
#   ncell             total number of cells
#   ncanal            number of canal cells
#   nlink             number of links for link-node model
#   nstruct           number of flow control structures (gates and pumps)
#
# constants
#   mindepth          minimum depth (m)
#   lseep             canal seepage constant (1/day)
#   rseep             marsh seepage constant (1/day)
#   cell$E0           surface elevation (elevation with cell volume=0) (m)
#   Eb                water elevation outside of Refuge
  
# Initial values
#   Einit$elev        Initial cell stage (m)
#
# Variables
#   sim.Stage         stage (water surface elevation)
#   sim.depth         water depth E-E0 (m)
#
# from saved datasets
#   cell$area         cell surface area (m^2)
#   cell$E0           soil elevation (m)


#_____________________________________________________________________________
#                         INITIALIZE
  # set up the constants for this run by sourcing the input file
  source(paste(filename,'.R', sep="")) # set user defined constants

  # include functions stored in other files
  source('Day2Date.R')        # 39-Box day and Date functions
  source('depth.R')           # cell depth and volume functions 
  
  # Initialize global parameters intended to be available in all functions
  # Set initial simulation control parameters
  # TIME=0 corresponds to 1/1/1995 0:00, 1/1/1995 is day 1
  
  Start.Day  = Date2Day(Start.Date)
  Start.Time = Day2TIME(Start.Day) 
  # stop time is 5483
  Stop.Day   =  Date2Day(Stop.Date) # last full day of simulation
  Stop.Time  =  Day2TIME(Stop.Day)+1 # for 2004-2010 2557 days, 
  # DTseconds   <- 180 # seconds # (not used) 
  # DT          <-DTseconds/86400. # time step (days) (not used)
  # DTOUT       <- 1 # time interval for saving output for analysis (days) (not used) 
    
    # regulation schedule B floor stage, the B floor stage is 14 feet
    BFloor <- 14*0.3048   # B floor (m)
    
    # minimum depth
    mindepth = 0.05       # minimum depth (m), avoids division by zero

  # Calculated constants
      minvol = mindepth*cell$area # minimum volume
      # power law  transport coefficient
      B <- data.frame(link$type, rep(NA,nlink)) # (1/m-day)
      names(B) <- c('type','val')
      B$val[B$type=='cc'] <- Bcc
      B$val[B$type=='cm'] <- Bcm
      B$val[B$type=='mm'] <- Bmm
      seep <- rep(NA,ncell)# seepage constant- loss to groundwater
      seep[1:ncanal] <-         lseep # canal
      seep[(ncanal+1):ncell] <- rseep # marsh
      
#_____________________________________________________________________________      
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
#_____________________________________________________________________________      
      
  # Bank-full canal stage, depth, volume and area
    Canal.BF.calc <- function() { # calculate canal bank full parameters
      # canal is bank full (BF) when canal stage == adjacent perimeter cell
      #   soil elevation (cell#E0)
      # cell number for perimeter cell adjacent to canal cell
      #   canal cell#  1   2   3   4   5   6   7   8   9  10, 11
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
    Canal.BF <- Canal.BF.calc() # bank full stage in each canal cell ()

#_____________________________________________________________________________
  # Timeseries 
  # timeseries can extend from Model.BaseDate to Model.EndDate
  # canal cell inflow and outflow
    # historic outflow  (m^3/day) {
    QoutHistoric <- function(DAY) { 
      # canal outflows by cell on DAY
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
    
    # historic inflows (m^3/day)
    Qinfunc <- function(DAY) { 
      # canal inflows by cell on DAY
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
    
    A1Floor <- function(DAY) {
    # Regulation Schedule A1 floor stages 
      # find "day of year" for DAY
      doy <- yday(Day2Date(DAY)) # yday is in library lubridate
      d <- c(   0,   132,   188,  267,  334,  366)
      A <- c(17.2, 15.75, 15.75, 17.5, 17.5, 17.2)
      A1F <- approx(d,A, xout = doy, rule=1)$y # A1Floor (ft)
      A1F <- A1F*0.3048 # A1Floor (m)
      return(A1F)
    } #end A1Floor  
    
    QCalcOutS10 <- function(S10Stage,A1) {
      # Regulatory release (thousand m3/d) as a function of 
      #   difference between stage and A1 zone floor (ft).
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
    
    QoutCalcCell <- function(S10Stage) {
      # distribute calculated outflows among canal cells
      QoutCalc <- rep(NA,ncanal)
      #Qx <- 1000*(((2*0.2457)+0.2629)/((3*0.2457)+0.2629))*(196.7/144.8)
      #Qc <- Qx*RSQfact*QCalcOutS10(S10Stage,A1)
      Qc <- RSQfact*QCalcOutS10(S10Stage,A1)*1000000
      QoutCalc[5:7]  <-  0.2457*Qc # (m3/day)
      QoutCalc[8]    <-  0.2629*Qc    # (m3/day)
      QoutCalc[5]    <-  0 # for test, remove **********************************************
      return(QoutCalc)
    } # end QoutCalcCell 
    
    Fet <- function(d) {
      # ET reduction factor (dimensionless) as a function of depth d (m)
      f <- pmin(1, d/Het)   # ET reduction for depth < Het 
      f <- pmax(ETmin, f)   # ET reduction for depth > ETmin
      return(f)
    } # Fet
    
    Link.Flow.calc <- function(Depth, Stage) {
      # the function returns link flows from up to dn (m^3/day)
      ldepth <- Depth[link$dn]  # depth at downstream end of link
      dStage <- Stage[link$up] - Stage[link$dn]  # stage drop of link
      Q <- 10000000*B$val*link$Width*(ldepth^3)*dStage/link$Radius # power law
      return(Q) # (m^3/day)
    }
    
    link2cell <- function(F) {
      # Link flows or mass transport are added/subtracted to/from cells
      # F has dimension 1:nlink
      dm <- rep(0, ncell) # derivative caused by inflow/utflow
      for (i in 1:nlink) { 
        # transport is from link$up to link$dn
        dm[link$dn[i]] <- dm[link$dn[i]] + F[i]
        dm[link$up[i]] <- dm[link$up[i]] - F[i]
      } # end for i
      return(dm)
    } # end link2cell

derivs <- function(simtime, State, Params) { # return derivitive of cell volume
# derivs is called by function ODE, 
  # derivs returns the derivative of the cell volume state variables
# WATER BUDGET
  # Params arguments are currently not used
  
  TIME <- simtime
  DAY  <- floor(simtime) +1
  if (DAY != Day_pre) {
    Day_pre <<- DAY
    newday  <<- TRUE
    t0   <<- DAY - Start.Day # simulation time at start of this day
    t1   <<- t0+1           # simulation time at end of this day
    Day_sub <- t1 # subscript for matrix rows
    A1 <<- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10  
    # Qin <- Qinfunc(DAY)  # canal cell inflows (m^3/d)
    Qin <<- QinExternal[t1,]  # cell structure inflows+ precip (m^3/d)
  } else {
    newday <- FALSE
  }

  Qout <- QoutHistoric(DAY) # historic outflow
  
  # Volume - rate of change of cell volume (m^3/day)
    V <- State[1:ncell] # state variable estimates from deSolve integrator
    Depth <- Cell.Depth.calc(V)
    Stage <- cell$E0 + Depth
    # initialize cell volume derivatives
    dVdt <- rep(0,ncell) # derivative of volume is net flow into cell (m^3/day)
    # add up each component of flow in each cell
    # historic structure outflow  
    dVdt[1:ncanal] <- -Qout # historic outflow, note-use negative for outflow
    # Regulatory release, calculated outflow
    # Canal cells 5, 6, 7, and 8 correspond to the S-10 & S-39 structures
    # canal cells:  5 (S10D), 6 (S10C), 7 (S10A), 8 (S39) 
    #   are involved in regulation schedule
    #   only S39 has water supply outflow when 1-8C WL < A1Floor
    # When CalcQRo is TRUE, regulatory outflow through these structures is
    #   calculated using the Refuge regulation schedule.
    if (CalcQRo == TRUE) { # then replace historic outflows with calculated 
          # Calculate QoutCalc 5, 6, 7, & 8
      # RSQfact was used in BM version options 2 and 3 to scale the 
      #  intensity of the regulatory release (dimensionless)
      # stage at the USGS 1-8C gage is used by the regulation schedule
      S10Stage <- Stage[9] # calculate using stage at 1-8C
      # keep historical outflow for C1, C2, C3, C4, C9, C10, and C11
      QoutCalc <- QoutCalcCell(S10Stage)
      # from BM model
        #QoutCalc[5:7]  <-  0.2457*RSQfact*(196.7/144.8)*
         #                  1000*QCalcOutS10(S10Stage,A1) # (m3/day)
        #QoutCalc[8]    <-  0.2629*RSQfact*(196.7/144.8)*
         #                  1000*QCalcOutS10(S10Stage,A1)    # (m3/day)
      # replace historic flows with calculated flows (negative for outflow)
        # plus nonregulatory outflows (hurricane deviations & water supply)
      dVdt[5] <- -(QoutCalc[5] + NonReg$S10D[DAY]) # hurricane deviation
      dVdt[6] <- -(QoutCalc[6] + NonReg$S10C[DAY]) # hurricane deviation
      dVdt[7] <- -(QoutCalc[7] + NonReg$S10A[DAY]) # hurricane deviation
      dVdt[8] <- -(QoutCalc[8] + NonReg$S39[DAY]) # S39 water supply
    } # end if CalcQRo
  # link flows
    Link.Q <- Link.Flow.calc(Depth, Stage) # flow through the nlink links
    #dVdt[1:ncanal] <- dVdt[1:ncanal] + Q
    # flow into downstrem cells from links
    dVdt <- dVdt + link2cell(Link.Q)
  # historic structure inflow
    #dVdt[1:ncanal] <- dVdt[1:ncanal] + Qin # structure inflows (positive)
    # Precipitation
    #dVdt <-  dVdt + (cell$area*PET$P[DAY]) # precip (positive)
  # Historic structure inflow + precipitation
    dVdt <-  dVdt + QinExternal[t1,]
  # Evapotranspiration
    ET <- Fet(Depth)*PET$ET[DAY]  # potential ET * ET reduction factor (m/day)
    dVdt <-  dVdt - (cell$area*ET) # ET (negative)
  # Seepage to groundwater
    GWloss <- seep*(Stage-Eb)  # loss to groundwater (m/day)
    dVdt <-  dVdt - (cell$area*GWloss) # seepage (negative)
    
    if (newday) {points(simtime,Stage[9], col = 'red')} # plot a point
      
  return(list(dVdt))    
  } # end derivs
    
#_____________________________________________________________________________     
#_____________________________________________________________________________
  # set initial conditions
    # calculate initial cell volume from initial cell depth
    Vinit <- Cell.Volume.calc(Einit$elev - cell$E0) # initial cell volumes
    #vinit <- Cell.Volume.calc(BMSimObsStage[1,15:53] - cell$E0) # for exact match to BM 39-Box  
  # set up simulation output data matricies
    # simulated daily cell volume (m^3), depth (m), and stage (m)
    sim.Volume <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Depth  <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Stage  <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Volume[1,] <- Vinit
    sim.Depth[1,]  <- Cell.Depth.calc(Vinit)
    sim.Stage[1,]  <- sim.Depth[1,] + cell$E0
    # start plot to show stage at 1-8c gauge
    plot(Start.Day,sim.Stage[1,9], # plot canal at USGS 1-8C
         xlim=c(Start.Day,Stop.Day), ylim=c(3,7), col='green',
         xlab = 'DAY', ylab = '1-8C Stage (m)')
    run.time <- Sys.time() # initial time, used to determine simulation run time
#_____________________________________________________________________________
  # Calculate flow boundaries for each cell
  # Define a matrix ndays rows by ncells columns of all external inflows 
    # from structures + precipitation (m^3/day)
  QinExternal <- matrix(data = 0.0, nrow = Stop.Day-Start.Day+2, ncol = ncell)
  for (DAY in seq(Start.Day,Stop.Day, 1)) {
    Day_sub <- DAY -Start.Day +1 # row subscript, DAY=Start.Day-1+Day_sub
    # structure inflows
    QinExternal[Day_sub,1:ncanal] <- Qinfunc(DAY)  # canal cell structure inflows
    # precipitation
    QinExternal[Day_sub,] <- QinExternal[Day_sub,] + 
      (cell$area*PET$P[DAY]) # plus precipitation (m^3/day)
  } # end for DAY loop
#_____________________________________________________________________________
   # initially set A1 here because it must be globally available  
   A1 <- A1Floor(Start.Day) # regulation schedule A1 stage used by QCalcOutS10
   # run the simulation from start to stop
   Times_out <- seq(Start.Time,Stop.Time, by=1) # initial time to end time
   ndays <- length(Times_out) # number of days of simulation including initial day
   Day_pre <- -9999 # initialize here for use in derivs function
   # run the integration
   #state1 <- lsoda(Vinit, Times_out, derivs, 0, # rtol = 1e-2, atol = 10000)
   #                atol = 50000, hini = 0.0, 
   #                verbose = FALSE, tcrit = Stop.Time)
   state1 <- radau(Vinit, Times_out, derivs, 0, # rtol = 1e-2, atol = 10000)
                   atol = 1000, hini = 0.0, 
                   verbose = FALSE) 
   # state1 is a ndays by ncell+1 matrix with columns for time & cell volume 
      # state1[,1] first column is simulation time, it should equal Times_out
      # state1[1,2:(ncell+1)] is initial cell volume
      # state1[2:ndays,2:(ncell+1)] is simulated cell volume
    # save the result in 3 matricies with ndays rows and ncells columns
    sim.Volume <- state1[,2:(ncell+1)] # simulated daily volume 
    for (i in (1:ndays)) { # loop through all output rows
      sim.Depth[i,]  <- Cell.Depth.calc(sim.Volume[i,]) # simulated daily depth
      sim.Stage[i,]  <- sim.Depth[i,] + cell$E0 # simulated daily stage
    } # end for DAY

  run.time <- Sys.time() - run.time
  print(run.time)
  
# end of model run
#_____________________________________________________________________________
  # Save output
  save(run.title, filename, sim.Volume, sim.Depth, sim.Stage,
       file=paste("../Output/",filename,".Rdata", sep=""))
#_____________________________________________________________________________
  