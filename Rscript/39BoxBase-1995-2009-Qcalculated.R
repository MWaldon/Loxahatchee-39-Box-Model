# This R script sets model run constants needed for the 39-Box Stage model

# USERS SHOULD NOT CHANGE THIS BASE RUN FILE.
# TO RUN A DIFFERENT SCENARIO - COPY THIS FILE TO A NEW NAME,
#   THEN MAKE SCENARIO-RELATED CHANGES IN THE NEW FILE. TO USE THE NEW FILE
#   RUN THE SCRIPT runStage.R & ENTER THE NAME WHEN PROMTED.

# Run descriptive title (What is special about this model run?)
  run.title <- '1995-2009 Qout Calculated' # Enter the run title here

# beginning date for the model run, ending date for model run
  Start.Date <- as.Date('1995-01-01')  # first possible is as.date('1995-01-01')
  Stop.Date  <- as.Date('2009-06-30')  #  last possible is as.Date('2009-06-30')
  
# simulation control
  CalcQRo = TRUE # TRUE # Use calculated outflow if TRUE
  RSQfact = 1    # Factor multiplying calculated output (dimensionless)
  
# seepage, loss to groundwater
  lseep =  0.0484046    # canal seepage constant (1/day)
  rseep = 8.16167e-10   # marsh seepage constant (1/day)
  
# power law constants used to calculate link flow rates by link types
  Bcc = 6.97621   # canal-canal links
  Bcm = 1.13863   # canal-marsh links
  Bmm = 4.55002   # marsh-marsh links
  
# ET reduction constants
  ETmin  = 0.20         # minimum ET reduction factor for marsh
  Het      = 0.25       # depth below which ET is reduced (m)
  
  Eb = 3.5  # water surface elevation outside refuge (m)
  
# Load required dataframes created by script make_datasets.R
  load(file="../Datasets/39-Box-Datasets.Rdata")

# Initial conditions
  # initial water surface elevations (also termed water stage)
  Einit <- data.frame(cell$type,rep(NA,ncell)) # E is surface elevation (m)
  names(Einit) <- c('type', 'elev')
  Einit$elev[Einit$type=='Canal'] <- 5.1   # initial canal cell stage (m)
  Einit$elev[Einit$type=='Marsh'] <- 5.09   # initial marsh cell stage (m)
  
  # On the following lines the user may alter the loaded dataframes
  #   for sensitivity or calibration.
  # Example: To model the impact of increasing flow through the S39 by 20%
  #   NonReg$S39 <- 1.2*NonReg$S39 
  
  
  
  