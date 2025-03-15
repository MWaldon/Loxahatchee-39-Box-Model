# plot comparison for S-6 diversion scenario

# get base stage timeseries
filename <- "39BoxBase"
load(file=paste("../Output/",filename,".Rdata", sep=""))
Depth <- sim.Depth
# depth of interior marsh near S-6
d36 <- Depth[,36]

# get S-6 scenario output
filename <- "NoS6Inflow"
load(file=paste("../Output/",filename,".Rdata", sep=""))
Depth.noS6 <- sim.Depth
# depth of interior marsh near S-6
d36.noS6 <- Depth.noS6[,36]

# Truncate length of d36 to match d36.noS6
n <- length(d36.noS6)
d36 <- d36[1:n]

# graph the depth timeseries
x <- as.Date(Start.Date:Stop.Date)
plot(x,d36[1:(n-1)], col='black', type = 'l',
     xlab='Date', ylab = 'Depth (m)')
lines(x,d36.noS6[1:(n-1)], col='red')

# Graph histograms (similar to function Stage.hist)
  par(mfrow = c(1, 1)) # reset to one graph per plot
  xlabel <- 'Depth (m)'
  ylabel <- 'Depth pdf (1/m)'
  xlimit = c(0,1.2)
  
  # plot no S-6 scenario
  hist(d36.noS6, border ='red', col = 'gray', 
       ylab=ylabel, xlab=xlabel, xlim = xlimit, 
       main = '', freq = FALSE)
  
    hist(d36, border='black', col = NA, lwd=4,
         add = TRUE, freq = FALSE)
  
# display the hydroperiods
  print(paste('   hydroperiod with S6 inflow =', 
              round(hydroperiod(d36), digits = 1),' (days/yr)'))
  print(paste('hydroperiod with no S6 inflow =', 
              round(hydroperiod(d36.noS6), digits = 1),' (days/yr)'))
    