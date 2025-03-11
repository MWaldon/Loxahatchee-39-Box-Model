# This script reads data and creates data frames. Data frames are saved 
# to the file 39-Box-Datasets.

library(readxl)

# remove all variables from the environment
rm(list = ls())

# ____________________________________________________________
# make dataframes with NA values
# cell dataframe
  ncell <- 39  # number of cells
  NAcell <- rep(NA, ncell) # array of missing
  # create dataframe with all missing values
  namecol <- c('num','area', 'E0', 'minvol', 
               'init_Cl', 'initTP','init_SO4', 'init_TPstore',
               'type', 'name', 'description')
  cell <- data.frame(1:ncell, 
                     NAcell, NAcell, NAcell, NAcell, NAcell,
                     NAcell, NAcell, NAcell, NAcell, NAcell)
  names(cell) <- namecol

# link dataframe
  nlink = 52 # number of links for link-node model
  NAlink <- rep(NA, nlink) # array of missing
  # create dataframe with all missing values
  namecol <- c('num','Radius', 'Width', 'dn', 'up', 'type')
  link <- data.frame(1:nlink, NAlink, NAlink, NAlink, NAlink, NAlink)
  names(link) <- namecol

# boundary inflow & outflow dataframes
  RBaseDate <- as.Date(0)   # day zero for R dates is 01Jan1970
  ModelBaseDate <- as.Date('1995-01-01') # earliest date for model, day=1
  ModelEndDate <-  as.Date('2030-01-01') # last possible date for model
  ndays <- as.numeric(ModelEndDate) - as.numeric(ModelBaseDate) +1
  days <- 1:ndays
  dates <- ModelBaseDate+days-1
  NAflow <- rep(NA,ndays)
  # import structure names
  StructureNames <- read_excel("../DataSets/StructureFlow.xlsx", 
                              sheet = "Inflow", range = "C2:U2")
  StructureNames <- names(StructureNames) # names for 19 in/out structures
  StructureNames <- c('day', StructureNames)
  Inflow <- data.frame(days, 
                       NAflow, NAflow, NAflow, NAflow, NAflow,
                       NAflow, NAflow, NAflow, NAflow, NAflow,
                       NAflow, NAflow, NAflow, NAflow, NAflow,
                       NAflow, NAflow, NAflow, NAflow)
  names(Inflow) <- StructureNames
  Outflow <- Inflow
  
# Non-regulation schedule flows - hurricanes & water supply (m^3/day)
  NonReg <- data.frame(days, NAflow, NAflow, NAflow, NAflow)
  names(NonReg) <- c('day', 'S10A', 'S10C', 'S10D', 'WaterSup')
  
# Precipitation & Evapotranspiration, P & EP (m/day)
  PET <- data.frame(days, NAflow, NAflow)
  names(PET) <- c('day', 'P','ET')
  
    
# boundary inflow concentrations
  # import structure names
  CStructureNames <- read_excel("../DataSets/InputC.xlsx", 
                               sheet = "TP", range = "B2:M2")
  CStructureNames <- names(CStructureNames) # names for 13 in/out structures
  CStructureNames <- c('day', CStructureNames)
  initCl <- data.frame(days, 
                       NAflow, NAflow, NAflow, NAflow, NAflow,
                       NAflow, NAflow, NAflow, NAflow, NAflow,
                       NAflow, NAflow, NAflow)
  names(initCl) <-c('day',CStructureNames)
  initTP <-  initCl
  initSO4 <- initCl

# ____________________________________________________________
# ____________________________________________________________
# read cell related arrays
 # area (m^2)
  areatable <- read_excel("../DataSets/CellParameters.xlsx", 
                               sheet = "Area")
  names(areatable) <- c('cell', 'ctype', 'area', 'BMArea')
  #cell$area <- areatable$area # this line would use areas from the User Manual
  cell$area <- areatable$BMArea # use areas exported from Berkeley Madonna
  cell$type <- areatable$ctype
  
 # bottom elevation (m)
  E0table <- read_excel("../DataSets/CellParameters.xlsx", 
                          sheet = "Elevation")
  names(E0table) <- c('cell', 'Etype', 'E0')
  cell$E0 <- E0table$E0
    
 # minimum cell volume (m) ***not actually needed
  MinVoltable <- read_excel("../DataSets/CellParameters.xlsx", 
                        sheet = "MinVol")
  names(MinVoltable) <- c('cell', 'MVtype', 'minVol')
  cell$minvol <- MinVoltable$minVol
  
# SO4 dissapearance rate
  kso4 <- read_excel("../DataSets/kso4.xlsx", 
                     sheet = "kso4")
  cell$kso4 <- kso4$kso4
  cell$subtype <- kso4$subtype
  
 # initial Cl (mg-Cl/L)
  Clinitial <- read_excel("../DataSets/Clinitial.xlsx", 
                            sheet = "Clinitial")
  names(Clinitial) <- c('cell', 'Clinit', 'name', 'description')
  cell$init_Cl <- Clinitial$Clinit
  cell$name <- Clinitial$name
  cell$description <- Clinitial$description

  # initial TP (mg-P/L)
  TPinitial <- read_excel("../DataSets/TPinitial.xlsx", 
                          sheet = "TPinitial")
  names(TPinitial) <- c('cell', 'TPinit', 'name', 'description')
  cell$initTP <- TPinitial$TPinit

  # initial SO4 (mg-SO4/L)
  SO4initial <- read_excel("../DataSets/SO4initial.xlsx", 
                          sheet = "SO4initial")
  names(SO4initial) <- c('cell', 'SO4init', 'name', 'description')
  cell$init_SO4 <- SO4initial$SO4init
  
  # initial storage in marsh cells
  TPstoreinitial <- read_excel("../DataSets/TPstoreemginitial.xlsx", 
                           sheet = "TPemgStoreinitial")
  names(TPstoreinitial) <- c('cell', 'TPstoreinit', 'name', 'description')
  cell$init_TPstore[cell$type=='Marsh'] <- TPstoreinitial$TPstoreinit
  
# ____________________________________________________________  
# read link related arrays
 # radius (m), average length  for link#=1 calculated from centroild distance
  Rtable <- read_excel("../DataSets/LinkParameters.xlsx", 
                          sheet = "Radius")
  names(Rtable) <- c('link', 'name', 'Radius')
  link$Radius <- Rtable$Radius
  
 # Width (m), average canal/marsh width (m) for link#1 estimated from length for marsh
  Wtable <- read_excel("../DataSets/LinkParameters.xlsx", 
                       sheet = "Width")
  names(Wtable) <- c('link', 'name', 'Width')
  link$Width <- Wtable$Width
  
 # link upstream and downstream cells
  UDtable <- read_excel("../DataSets/Linkupdn.xlsx", 
                       sheet = "linkupdn")
  names(UDtable) <- c('link', 'up', 'dn')
  link$dn <- UDtable$dn
  link$up <- UDtable$up
  
 # set link types cc = canal-canal, cm = canal-marsh, mm = marsh-marsh
  link$type[1:11] <-  'cc'
  link$type[12:22] <- 'cm'
  link$type[23:52] <- 'mm'

# ____________________________________________________________
 # read canal cell volume-stage data
  CanalVS <- read_excel("../DataSets/Volume-Stage-Canal.xlsx", 
                                   sheet = "S-V")
# ____________________________________________________________
 # read historic inflow & outflow
  # import structure names
  InflowTable <- read_excel("../DataSets/StructureFlow.xlsx", 
                               sheet = "Inflow", range = "b2:U5297")
  Inflow[InflowTable$Days,] <- InflowTable
  
  OutflowTable <- read_excel("../DataSets/StructureFlow.xlsx", 
                            sheet = "Outflow", range = "b2:U5297")
  Outflow[OutflowTable$Days,] <- OutflowTable

# ____________________________________________________________
  # read historic structure inflow & outflow (from B-M model)
    StructOut <- read_excel("../DataSets/StructureInOut.xlsx", 
      sheet = "Qout", skip = 1)
    
    StructIn  <- read_excel("../DataSets/StructureInOut.xlsx", 
                            sheet = "Qin", skip = 1)
    
    StructTP  <- read_excel("../DataSets/StructureInOut.xlsx", 
                           sheet = "TPin", skip = 1)
    
    StructCl  <- read_excel("../DataSets/StructureInOut.xlsx", 
                           sheet = "Clin", skip = 1)
    
    StructSO4 <- read_excel("../DataSets/StructureInOut.xlsx", 
                           sheet = "SO4in", skip = 1)
  
# ____________________________________________________________
 # Outflow exceptions to the regulation schedule
#  - hurricanes via S10s & water supply
  NonRegTable <- read_excel("../DataSets/Regulation.xlsx", 
                             sheet = "Regulation")
  NonReg[NonRegTable$TIME,] <- NonRegTable

# ____________________________________________________________
 # read historic precip, & ET
  PETTable <- read_excel("../DataSets/PET.xlsx", 
                         sheet = "P-ET")
  PETTable <- data.frame(PETTable$Day,PETTable$P,PETTable$ET)
  PET[PETTable$PETTable.Day,] <- PETTable 
  
  # ____________________________________________________________
 # read historic inflow concentrations
  initClTable <- read_excel("../DataSets/InputC.xlsx", 
                                sheet = "CL", range = "A2:M5297")
  initCl[initClTable$day,] <- initClTable
  #
  initTPTable <- read_excel("../DataSets/InputC.xlsx", 
                            sheet = "TP", range = "A2:M5297")
  initTP[initTPTable$day,] <- initTPTable
  #
  initSO4Table <- read_excel("../DataSets/InputC.xlsx", 
                            sheet = "SO4", range = "A2:M5297")
  initSO4[initSO4Table$day,] <- initSO4Table
 
# ____________________________________________________________
  # read observed stage & concentration values for calibration
  # observed stage row 1=name, 2=cell#, 3..n=day & stage
  ObsStage <- read_excel("../DataSets/obs_stageall.xlsx", 
                           sheet = "obs_stageall")
  # replace stage=0 with NA

# ____________________________________________________________
# ____________________________________________________________
 # find first and last days for the datasets
  DatasetNames <- c('cell', 'link', 'Inflow', 'Outflow', 'NonReg', 'PET',
                    'initTP', 'initCl', 'initSO4')
  TSeriesNames <- c('Inflow', 'Outflow', 'NonReg', 'PET',
                    'initTP', 'initCl', 'initSO4')
  first <- c(min(InflowTable$Days),  min(OutflowTable$Days), 
             min(NonRegTable$TIME), min(PETTable$PETTable.Day),     
             min(initTPTable$day),  min(initClTable$day), 
             min(initSO4Table$day) )
  last  <- c(max(InflowTable$Days),  max(OutflowTable$Days), 
             max(NonRegTable$TIME), max(PETTable$PETTable.Day),     
             max(initTPTable$day),  max(initClTable$day), 
             max(initSO4Table$day) )
  StartDate <- ModelBaseDate+first-1
  EndDate   <- ModelBaseDate+last-1
  TSeriesLimits <- data.frame(TSeriesNames, first, last, StartDate,EndDate)
# ____________________________________________________________
  
  save(cell, link, Inflow, Outflow, NonReg, PET, CanalVS,
       initTP, initCl, initSO4, 
       StructOut, StructIn, StructTP, StructCl, StructSO4,
       ObsStage, TSeriesLimits,
       file="../Datasets/39-Box-Datasets.Rdata")
# ____________________________________________________________
# END make_datasets.R  