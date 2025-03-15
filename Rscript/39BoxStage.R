# IN NORMAL USE THIS SCRIPT IS SOURCED FROM SCRIPT "runStage.R" 
# The 39-Box model simulates water hydrology and water quality in the 
# A.R.M. Loxahatchee National Wildlife Refuge. It was first developed using
# the simulation environment Berkeley-Madonna by researchers at the University
# of Louisiana at Lafayette in cooperation with the U.S. Fish and Wildlife
# Service. 
# Original development team members include:
# Ehab Meselhe, William Roth, Tracy Chen, Hamid Bazgirkhoob
#   ... Mike Waldon, 
#
# Metric units are used in this model.
#   Unit conversion: 1 g/m3 = 1 mg/L = 1 ppm = 1000 ppb,
#                    1 ppb  = 1 ug/L
#   time unit in calculations is day
#
#_____________________________________________________________________________
#                         PREAMBLE

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
  # Initialize global parameters intended to be available in all functions
  # Set initial simulation control parameters
  # TIME=0 corresponds to 1/1/1995 0:00, 1/1/1995 is day 1
  # start time is 3287
  
  Start.Day  = Date2Day(Start.Date)
  Start.Time = Day2TIME(Start.Day) 
  # stop time is 5483
  Stop.Day   =  Date2Day(Stop.Date) # last full day of simulation
  Stop.Time  =  Day2TIME(Stop.Day)+1 # for 2004-2010 2557 days
  # time variables from Berkeley-Madonna model that are not used here
  #   DTseconds   <- 180 # seconds # (not used) 
  #   DT          <-DTseconds/86400. # time step (days) (not used)
  #   DTOUT       <- 1 # time interval for saving output for analysis (days)
  
  # Approx start of current reg schedule
  RSched.start = as.Date('1995-06-01') # useful in comparison to observed
  
  Dates <- as.Date(Start.Date:(Stop.Date+1)) # vector of simulated dates
    
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
      
  Canal.BF <- Canal.BF.calc() # bank full stage in each canal cell (m)
      
derivs <- function(simtime, State, Params) {
# derivs is called by function ODE, 
  # derivs returns the derivative of the volume state variable
# WATER BUDGET
  # Params arguments are currently not used
  # Day_sub <- floor(simtime) +1 # subscript for matrix rows, calculated in DAY loop
  # Volume - rate of change of cell volume (m^3/day)
    V <- State[1:ncell] # state variable estimates from ODE
    Depth <- Cell.Depth.calc(V)
    Stage <- cell$E0 + Depth
    dVdt <- rep(0,ncell) # derivative of volume is net flow into cell (m^3/day)
  # add up each component of flow in each cell
   # outflows from canal cells
     dVdt[1:ncanal] <- QoutUsed() # outflow from canal cells (negative)
   # link flows
     Link.Q <- Link.Flow.calc(Depth, Stage) # flow through the nlink links
     # flow into (- out of) cells from links
     dVdt <- dVdt + link2cell(Link.Q)
  # historic structure inflow + Precipitation
     dVdt <-  dVdt + QinExternal[t1,]
  # Evapotranspiration
     ET <- Fet(Depth)*PET$ET[DAY]  # potential ET * ET reduction factor (m/day)
     dVdt <-  dVdt - (cell$area*ET) # ET (negative)
  # Seepage to groundwater
     GWloss <- seep*(Stage-Eb)  # loss to groundwater (m/day)
     dVdt <-  dVdt - (cell$area*GWloss) # seepage (negative)
      
  return(list(dVdt))    
  } # end derivs
    
#_____________________________________________________________________________     
#_____________________________________________________________________________
  # set initial conditions
    Vinit <- Cell.Volume.calc(Einit$elev - cell$E0) # initial cell volumes
    #vinit <- Cell.Volume.calc(BMSimObsStage[1,15:53] - cell$E0) # for match to BM 39-Box  

    # set up simulation output matricies
    sim.Volume  <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Depth   <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Stage   <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncell)
    sim.Outflow <- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = ncanal)
    sim.Linkflow<- matrix(data = NA, nrow = Stop.Day-Start.Day+2, ncol = nlink)
    
    # fill the initial row of the simulation output matricies
    sim.Volume[1,]   <- Vinit
    sim.Depth[1,]    <- Cell.Depth.calc(Vinit)
    sim.Stage[1,]    <- sim.Depth[1,] + cell$E0
    Qout <- QoutHistoric(Start.Day) # historic outflow
    # calculate S10 regulatory outflow
    S10Stage <- sim.Stage[1,9] # Stage[9] # calculate using stage at 1-8C
    DAY <- Start.Day
    A1 <- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10
    QoutCalc <- QoutCalcCell(S10Stage)
    sim.Outflow[1,]  <- QoutUsed() # outflow from canal cells (negative)
    sim.Linkflow[1,] <- Link.Flow.calc(sim.Depth[1,], sim.Stage[1,]) # link flow
  # calculate inflow boundary condition for each cell for each day of simulation
    #   as matrix of all external inflows to each cell on each day.
    # QinExternal is a matrix nrow = Stop.Day-Start.Day+1, ncol = ncell
  QinExternal <- QinExternal.calc() # cell inflows + precip (m^3/day)

  # set up to plot canal at USGS 1-8C during simulation
    par(mfrow = c(1, 1)) # reset to one graph per plot
    plot(Start.Day,sim.Stage[1,9], 
         xlim=c(Start.Day,Stop.Day), ylim=c(3,7), col='green',
         xlab = 'DAY', ylab = '1-8C Stage (m)')
    
  run.time <- Sys.time() # beginning timer value for run time measurement
  
  # Loop through the days from start to stop
  for (DAY in seq(Start.Day,Stop.Day, 1)) {
    TIME <- DAY-1
    t0   <- DAY - Start.Day # simulation time at start of this day
    t1   <- t0+1            # simulation time at end of this day
    Day_sub <- t1           # matrix row subscript 
    t2   <- t1+1           # used as matrix index for result storage 
    A1 <- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10  
    # Qin <- Qinfunc(DAY)  # canal cell inflows (m^3/d)
    Qin <- QinExternal[t1,]  # cell structure inflows+ precip (m^3/d)
    Qout <- QoutHistoric(DAY) # historic outflow
    # calculate S10 regulatory outflow
    S10Stage <- sim.Stage[t1,9] # Stage[9] # calculate using stage at 1-8C
    QoutCalc <- QoutCalcCell(S10Stage)

    # run the simulation for 1 day
    state1 <-  # ode(Vinit, (0:1), derivs, DAY)
      #lsoda(Vinit, (0:1), derivs, 0, # rtol = 1e-2, 
      #     atol = 50000, hini = 0.0, verbose = FALSE, tcrit = 1.0)
       radau(Vinit, (0:1), derivs, 0, 
             atol = 1000)
      # state1 is a 2 by (ncell+1) matrix with columns for time & cell volume 
      # state1[,1] first column is simulation time
      # state1[1,2:(ncell+1)] is initial cell volume
      # state1[2,2:(ncell+1)] is final cell volume
    # reset the initial condition to final value
    Vinit <- state1[2,2:(ncell+1)] # cell volumes at end of 1-day simulation
    
    # save the result - fill the next row of the simulation output matricies
    sim.Volume[t2,]   <- Vinit # save the cell volumes at time t2 (day t1)
    sim.Depth[t2,]    <- Cell.Depth.calc(Vinit) 
    sim.Stage[t2,]    <- sim.Depth[t2,] + cell$E0
    S10Stage <- sim.Stage[t2,9] # Stage[9] # calculate using stage at 1-8C
    QoutCalc <- QoutCalcCell(S10Stage) # note A1 is unchanged
    sim.Outflow[t2,]  <- QoutUsed() # outflow from canal cells (negative)
    sim.Linkflow[t2,] <- Link.Flow.calc(sim.Depth[t2,], sim.Stage[t2,]) # link flow
    
    # signal the end of the loop
    points(DAY,sim.Stage[t2,9], col = 'red')
  } # end for DAY loop
  sim.Velocity <- sim.Velocity.calc() # calculate link velocities matrix
  
  run.time <- Sys.time() - run.time
  print(run.time)
  
# end of model run
#_____________________________________________________________________________
  # Save output
  save(run.title, filename, sim.Volume, sim.Depth, sim.Stage,
       sim.Outflow, sim.Linkflow, sim.Velocity,
       Start.Date, Stop.Date,
       file=paste("../Output/",filename,".Rdata", sep=""))
#_____________________________________________________________________________
  