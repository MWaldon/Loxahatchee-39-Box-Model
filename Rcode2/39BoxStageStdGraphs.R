# plot the standard stage graphs following a model run

library(readxl)      # read Excel worksheets
library(matrixStats) # needed for colMin

# source('39BoxFunctions.R') # these are likely already loaded

# Load required datasets 
  load(file="../Datasets/39-Box-Datasets.Rdata") # created by make_datasets.R
  load(file=paste("../Output/",filename,".Rdata", sep="")) # output from run

# read USGS gague data for graphical comparison to simulations
#  daily average stage by "DATE"
#  gague names:  "GS8T"  "GS7"   "GS8C"  "GS9"   "South" "North"
  USGS_Stages <- USGS_Stages.read()

# Create standard group of graphics
  par(mfrow = c(1, 2)) # side-by-side graphs on one plot
      
  Stage.graph('North',        35)       # North
  Stage.hist ('North',        35)
  
  Stage.graph('South',        38)       # South
  Stage.hist ('South',        38)
  
  Stage.graph(c('GS7','GS9'), 37)       # 7 & 9
  Stage.hist (c('GS7','GS9'), 37)
  
  Stage.graph('GS8T',         29)       # 8T (could also use cell 37)
  Stage.hist ('GS8T',         29)
  
  Stage.graph('GS8C',         9)        # 8C
  Stage.hist ('GS8C',         9)
  
  par(mfrow = c(1, 1)) # reset to one graph per plot
  
# plot water canal water velocity upstream of the USGS 1-8C gague
  plot(Dates, sim.Velocity[,8], type='l', col='blue',
       xlab = 'Date', ylab = 'Velocity Upstream 1-8C (m/day)')
  # plot water interior water velocity cell 36 to 37
  plot(Dates, sim.Velocity[,51], type='l', col='blue',
       xlab = 'Date', ylab = 'Interior velocity cell 36-37 (m/day)')
  
# map minimum depth 
  sf.read()
  
  minMarshDepth <-  colMins(sim.Depth)[(ncanal+1):ncell]
  meanMarshDepth <- colMeans(sim.Depth)[(ncanal+1):ncell]
  maxMarshDepth <-  colMaxs(sim.Depth)[(ncanal+1):ncell]

  marsh.map(minMarshDepth,  maptitle="Minimum Simulated Marsh Depth (m)")
  marsh.map(meanMarshDepth, maptitle="Mean Simulated Marsh Depth (m)")
  marsh.map(maxMarshDepth,  maptitle="Maximum Simulated Marsh Depth (m)")
  
  