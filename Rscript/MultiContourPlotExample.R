# These functions make and display a contour plots series

cplots.make <- function(   # create and display a series of plots
# example of using the MarshMap.contour function
  z.plot = sim.Depth, # property to contour plot on the z axis
  dstart = as.Date('1999-01-01'), # starting date
  dend =   as.Date('1999-01-31'), # ending date
  dstep = 1,                      # step size in days
  cplots.title = "Simulated Marsh Depth (m)",
  zmin = 0,
  zmax = 2.0,
  raster.len = 250)
  {
  i <- 0  # plot counter
  cplots <- list() # initialize list containing plot objects 
  cdates <- as.Date(c())    # initialize vector of plot dates
  dseq <- as.Date(seq(from=dstart, by=dstep, to=dend)) # plot dates
  print(paste('creating', length(dseq), 'plots from', 
              as.Date(dstart), 'to', as.Date(dend)))
  for (d in dseq) {
    i <- i+1  # increment plot counter
    cdates[i] <- as.Date(d)  # save the plot date
    # create the plot 
    cpl <- MarshMap.contour(z.plot[Date2Day(cdates[i]),], 
                     zlimit = c(zmin,zmax), 
                     raster.length = raster.len, 
                     cplot.date = cdates[i], 
                     cplot.title = cplots.title,
                     plot.return = TRUE)
    cpl$xyz <- NULL # removes xyz from cpl list leaving only the plot in the list
    
    cplots <- append(cplots, cpl) # append new plot to list cplots
    } # end for d
    return(cplots)
  } # end function cplots.make

cplots.show <- function(cplots, loops=2) {
  # use this function to sequentially display the cplots.make contour plots
  
  print('click red STOP (top of console window) to stop the looped graphics')
  cat('Loop count = ') # print without new line
  for (i in 1:loops) {
    graphics.off() # clear Plots window
    cat(i, ' ') # display the loop number
    Sys.sleep(4)
    for (j in 1:length(cplots)) { # loop through the plots
      print(cplots[[j]]) # display the plot
      # delay between plots, some min delay is needed to allow plot to complete
      Sys.sleep(0.8) 
    } # end for j
  } # end for i
  cat(' show completed') 
} # end function cplots.show
