# calculate cumulative intrusion from link velocity

# see the function colors() if more colors are needed
mycolors <- 
  c('black', 'red', 'green', 'blue', 'gray', 'magenta', 'cyan', "darkseagreen")

transect.intrusion.plot <- function(transect, plot.title) { 
  # Plot transect intrusion, transect is vector of links 
  n <- length(transect) # must be 2 or greater
  ymx <- max(sim.intrusion[,transect])
  ymn <- min(sim.intrusion[,transect])
  plot(Dates, sim.intrusion[ , transect[n]], col=mycolors[n],
       type = 'l', ylab = 'Total distance (m)', xlab = 'year', 
       ylim = c(ymn, ymx),  
       main = paste(plot.title, ', links', paste(transect,collapse = " ")) )
  for (i in seq(from = n-1, to = 1, by = -1)) { 
    lines(Dates, sim.intrusion[, transect[i]], col=mycolors[i])
  } # end for i
} # end transect.plot

# set intrusion matrix equal in size to sim.velocity
sim.intrusion <- matrix(nrow = length(Dates), ncol = nlink)
for (i in (1:nlink)) { # loop through all structures
  sim.intrusion[,i] <- cumsum(sim.Velocity[,i]) # cumulative sum is distance (m)
}

# plot transect north between G-300 & G-301
  transect <- c(12, 23, 35)  #links between cells 1-12-13-35
  plot.title <- 'G-300 - G-301 cells 1-12-13-35'
  transect.intrusion.plot(transect, plot.title)

# plot transect at cell 10, S-362 Acme-1 G94-D
  transect <- c(21, 32, 44)  #links between cells 10-30-31-35
  plot.title <- 'STA-1E S-362 cells 10-30-31-35'
  transect.intrusion.plot(transect, plot.title)
  
# plot transect at cell 3, G-251 STA-1W
  transect <- c(14, 25, 37)  #links between cells 3-16-17-34
  plot.title <- 'STA-1W G-251 cells 3-16-17-34'
  transect.intrusion.plot(transect, plot.title)
  
# plot transect at cell 4, S-6
  transect <- c(15, 26, 38)  #links between cells 4-18-19-36
  plot.title <- 'S-6 cells 4-18-19-36'
  transect.intrusion.plot(transect, plot.title)

# plot transect at cell 7, S-10A
  transect <- c(18, 29, 41)  #links between cells 7-24-25-38
  plot.title <- 'S-10A cells 7-24-25-38'
  transect.intrusion.plot(transect, plot.title)
  
# plot transect at cell 8, S-39 G94A
  transect <- c(19, 30, 42)  #links between cells  8-26-27-39
  plot.title <- 'S-39 G-94A cells 8-26-27-39'
  transect.intrusion.plot(transect, plot.title)

# ____________________________________________________________________
# Canal - may not be very useful, and canal-canal velocities are inexact
  # However, they are plotted here for interest

  # plot transect at cell 1, Canal West
  transect <- c(1, 2, 3, 4, 5, 6, 7)  #links between cells 1-2-3-4-5-6-7-8
  plot.title <- 'Canal West cells 1-2-3-4-5-6-7-8'
  transect.intrusion.plot(transect, plot.title)
  
# plot transect at cell 1, Canal east
  transect <- c(11, 10, 9, 8)  #links between cells 1-11-10-9-8
  plot.title <- 'Canal East cells 1-11-10-9-8'
  transect.intrusion.plot(transect, plot.title)
  