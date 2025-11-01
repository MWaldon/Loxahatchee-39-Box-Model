# This R script sets model run constants needed for the 39-Box Stage model

# USERS SHOULD NOT CHANGE THIS BASE RUN FILE.
# TO RUN A DIFFERENT SCENARIO - 
#   MAKE SCENARIO-RELATED CHANGES IN A RENAMED COPY OF THE
#   SCRIPT runStage.R, see Users Manual for more

# Run descriptive title (What is special about this model run?)
  run.title <- 'Run Title' # Enter the run title here

# beginning date for the model run, ending date for model run
  Start.Date <- as.Date('1995-01-01')  # first possible is as.date('1995-01-01')
  Stop.Date  <- as.Date('2025-04-30')  #  last possible is as.Date('2025-04-30')
  
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
  Het    = 0.25         # depth below which ET is reduced (m)
  
  Eb = 3.5  # water surface elevation outside refuge (m)

# Regulation schedule
  # Approx start of current reg schedule
  RSched.start = as.Date('1995-06-01') # useful in comparison to observed
  # regulation schedule B floor stage, the B floor stage is 14 feet
  BFloor <- 14*0.3048   # zone B floor (m)
  
# minimummarsh cell depth
  mindepth = 0.05       # minimum depth (m), avoids division by zero
  

# Initial conditions
  # initial water surface elevations (also termed water stage)
  Einit <- data.frame(cell$type,rep(NA,ncell)) # E is surface elevation (m)
  names(Einit) <- c('type', 'elev')
  # Einit$elev[Einit$type=='Canal'] <- 5.1   # initial canal cell stage (m)
  # Einit$elev[Einit$type=='Marsh'] <- 5.09   # initial marsh cell stage (m)
  library(readxl)
  Einit_input <- read_excel("../DataSets/Einit.xlsx", 
                            range = "B2:AN2", 
                            col_names = as.character(1:ncell))
  Einit$elev <- as.vector(Einit_input[1,1:ncell], mode = "numeric")
  rm(Einit_input) # cleanup
  
  
  