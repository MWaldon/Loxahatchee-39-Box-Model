# This R script sets model run constants needed for the 39-Box Stage model
# Run descriptive title (What is special about this model run?)
  run.title <- 'Enter the run title here'
# name of file that will save the run results
  run.filename <- '39BoxBase'

  
# simulation control
  CalcQRo = TRUE # Calculated outflow if TRUE
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

# Initial conditions
  # initial water surface elevations (also termed water stage)
  Einit <- data.frame(cell$type,rep(NA,ncell)) # E is surface elevation (m)
  names(Einit) <- c('type', 'elev')
  Einit$elev[Einit$type=='Canal'] <- 5.1   # initial canal cell stage (m)
  Einit$elev[Einit$type=='Marsh'] <- 5.09   # initial marsh cell stage (m)
  
  Eb = 3.5  # water surface elevation outside refuge (m)
  