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
  
    
  Dates <- as.Date(Start.Date:(Stop.Date+1)) # vector of simulated dates
    
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
    V <- State[1:ncell] # cell volume state variable estimates from ODE
    Depth <- Cell.Depth.calc(V) # cell depth (m)
    Stage <- cell$E0 + Depth    # cell stage (m)
    # derivative of volume is net flow into cell (m^3/day)
    dVdt <- rep(0,ncell) # initialize derivative vector 
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
    # initial cell volumes from initial depth = stage - cell bottom elevation
    Vinit <- Cell.Volume.calc(Einit$elev - cell$E0) 
    #vinit <- Cell.Volume.calc(BMSimObsStage[1,15:53] - cell$E0) # for match to BM 39-Box  

    # times during the single day simulation to save output
    sim.tout <- data.frame(dy = (0:nX)/nX) # decimal days of save times
    sim.tout$hr <- sim.tout$dy*24  # decimal hours of save times
    
    # set up simulation output matricies
    nr <- Stop.Day-Start.Day+2 # number of rows
    # subscripts represent day, cell/link, and time
    sim.Volume  <- array(data=NA, dim=c(nr, ncell, nX))
    sim.Depth   <- array(data=NA, dim=c(nr, ncell, nX))
    sim.Stage   <- array(data=NA, dim=c(nr, ncell, nX))
    # outflow is only from the ncanal canal cells
    sim.Outflow <- array(data=NA, dim=c(nr, ncanal, nX)) 
    sim.Linkflow<- array(data=NA, dim=c(nr, nlink, nX))
    # set up output sim time dataframe - time from start of simulation
    sim.time <- data.frame(time = as.numeric(0:(Stop.Day-Start.Day+1)),
                           DAY = Start.Day:(Stop.Day+1))
    sim.time$DATE <- Day2Date(sim.time$DAY)
    # subscript for inflow/outflow/PETarrays (this is equal to sim.time$DAY)
    sim.time$sub  <- as.integer(sim.time$DATE - Model.BaseDate +1)
    
    # fill the initial row of the simulation output matricies
    sim.Volume[1, ,1]   <- Vinit
    sim.Depth[1, ,1]    <- Cell.Depth.calc(Vinit)
    sim.Stage[1, ,1]    <- sim.Depth[1, ,1] + cell$E0
    Qout <- QoutHistoric(Start.Day) # historic outflow
    # calculate S10 regulatory outflow
    S10Stage <- sim.Stage[1, 9, 1] # calculate using stage at 1-8C
    DAY <- Start.Day
    A1 <- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10
    QoutCalc <- QoutCalcCell(S10Stage)
    sim.Outflow[1, ,1]  <- QoutUsed() # outflow from canal cells (negative)
    sim.Linkflow[1, ,1] <- Link.Flow.calc(sim.Depth[1, ,1], sim.Stage[1, ,1])
  # calculate inflow boundary condition for each cell for each day of simulation
    #   as matrix of all external inflows to each cell on each day.
    # QinExternal is a matrix nrow = Stop.Day-Start.Day+1, ncol = ncell
    QinExternal <- QinExternal.calc() # cell inflows + precip (m^3/day)

  # set up to plot canal at USGS 1-8C during simulation
    par(mfrow = c(1, 1)) # reset to one graph per plot
    plot(Start.Day,sim.Stage[1,9,1], # plot the initial 1-8C stage
         xlim=c(Start.Day,Stop.Day), ylim=c(3,7), col='green',
         xlab = 'DAY', ylab = '1-8C Stage (m)',
         main = paste('running: ', filename))
    
  run.time <- Sys.time() # beginning timer value for run time measurement
  
  # Loop through the days from start to stop
  for (DAY in seq(Start.Day,Stop.Day, 1)) {
    TIME <- DAY-1
    # t0, t1, t2 are times beginning at zero on Start.Day
    t0   <- DAY - Start.Day # simulation time at start of this day (0, 1, 2,...)
    t1   <- t0+1            # simulation time at end of this day (1,2,3,...)
    # Day_sub <- t1           # matrix row subscript for QinExternal ...
    t2   <- t1+1           # used as matrix index for result storage (2,3,4,...)
    A1 <- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10  
    Qin <- QinExternal[t1,]  # cell structure inflows + precip (m^3/d)
    Qout <- QoutHistoric(DAY) # historic outflow
    # calculate S10 regulatory outflow from initial 1-8C stage
    S10Stage <- sim.Stage[t1, 9, 1] # Stage[9] # calculate using stage at 1-8C
    QoutCalc <- QoutCalcCell(S10Stage)

    # run the simulation for 1 day
    state1 <-  # ode(Vinit, sim.tout$dy, derivs, DAY)
      #lsoda(Vinit, sim.tout$dy, derivs, 0, # rtol = 1e-2, 
      #     atol = 50000, hini = 0.0, verbose = FALSE, tcrit = 1.0)
       radau(Vinit, sim.tout$dy, derivs, 0, 
             atol = 1000)
      # state1 is a (nX+1) by (ncell+1) matrix with columns for time & cell volume 
      # state1[,1] first column is simulation time
      # state1[1,2:(ncell+1)] is initial cell volumes
      # state1[nX+1,2:(ncell+1)] is final cell volumes
    # setup for next step, reset the initial condition to current final value
    Vinit <- state1[nX+1,2:(ncell+1)] # cell volumes at end of 1-day simulation
    
    # *** delete*** S10Stage <- sim.Stage[t2, 9, 1] # calculate using initial stage at 1-8C
    # save intermediate results - fill rows/slices of the simulation output matricies
    for (i in 1:(nX+1)) {
      if (i <= nX) { # then save to the current step
        ti <- t1
        ii <- i
      }
      else { # final value should initialize next step
        ti <- t2
        ii <- 1
      }
      sim.Volume[ti, ,ii] <- state1[i,2:(ncell+1)]
      sim.Depth[ti, ,ii]  <- Cell.Depth.calc(sim.Volume[ti, ,ii])
      sim.Stage[ti, ,ii]  <- sim.Depth[ti, ,ii] + cell$E0
      # save flows
      # note structure outflows are here unchanged during the day
      #      this code allows for future scenarios in which it is variable
      QoutCalc <- QoutCalcCell(S10Stage) # note unchanged throughout the day
      sim.Outflow[ti, ,ii] <- QoutUsed()  # uses Qout and QoutCalc
      sim.Linkflow[ti, ,ii] <- Link.Flow.calc(sim.Depth[ti, ,ii], 
                                              sim.Stage[ti, ,ii]) # link flow
    } # end for i
    
    # signal the end of the loop
    points(DAY,sim.Stage[t2,9,1], col = 'red')
    if (yday(Day2Date(DAY))==1) { # print year if it is January 1
      cat(paste0('\r starting year ', year(Day2Date(DAY))))
    }
  } # end for DAY loop
  
  # Final calculations
  
    # calculate link velocities matrix
    sim.Velocity <- sim.Velocity.calc() # calculate link velocities matrix
    
    # calculate the daily averages
    sim.Stage.da <- apply(sim.Stage[1:(nr-1), , ], c(1, 2), mean)
    sim.Depth.da <- apply(sim.Depth[1:(nr-1), , ], c(1, 2), mean) 
    
    # time for execution of the model
    run.time <- Sys.time() - run.time 
    print(' ') # finish the previous line
    print(run.time)
    
# end of model run
#_____________________________________________________________________________
  # Save output - save variables used to calculate constituent mass balance
  save(run.title, filename, # run identifiers 
       Start.Date, Stop.Date, Dates, sim.time, # run parameters
       nX, CalcQRo, Eb,
       sim.Volume, sim.Depth, sim.Stage, # state related
       sim.Outflow, sim.Linkflow, Inflow, # flows needed for mass balance
       Outflow, PET, 
       ETmin, Het, lseep, rseep, RSQfact, mindepth, # model parameters
       sim.Velocity, # particle tracking
       file=paste("../Output/",filename,".Rdata", sep=""))
#_____________________________________________________________________________
  