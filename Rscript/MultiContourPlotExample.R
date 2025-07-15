# These functions make and display a contour plots series 
#   using using function MarshMap.contour

# example use: weekly depth (cm) in 1999 
# cplots <- cplots.make(z.plot = sim.Depth*100, dstart = as.Date('1999-01-01'), dend =   as.Date('1999-12-31'), dstep = 7, zmin = 0, zmax = 200, raster.len = 500, cplots.title = 'Marsh Depth (cm)')
#   object.size(cplots) # for this example is total 19,461,744 bytes (53 plots)
# cplots.show(cplots, loops = 2) # show the plots in sequence
# cplots.save(cplots, fname='depth-1999-weekly') # save the plots
# cplots.load(fname='depth-1999-weekly')

# Functions list:
# cplots.make <- function( optional args ) # example of using MarshMap.contour
# cplots.show <- function(cplots, loops=2, dlay=0.8) # sequentially display plots
# cplots.save <- function(cplots, fname='', dlay=0, R.save=TRUE, png.save=TRUE)
#   save cplots into file fname
# cplots.load <- function(fname) # get plots saved in file fname

source('Marsh_Map_contour.R') # source the marsh contour plot function  

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

cplots.show <- function(cplots, loops=2, dlay=0.8) {
  # use this function to sequentially display the cplots.make contour plots
  
  print('click red STOP (top of console window) to stop the looped graphics')
  cat('Loop count = ') # print without new line
  for (i in 1:loops) {
    graphics.off() # clear Plots window (shuts down all graphics devices)
    cat(i, ' ') # display the loop number
    Sys.sleep(4) # delay before showing plot series
    for (j in 1:length(cplots)) { # loop through the plots
      # delete previous plot (use if there are too many plots in cplots)
      # dev.off() 
      print(cplots[[j]]) # display the plot
      # delay between plots, some min delay is needed to allow plot to complete
      Sys.sleep(dlay) 
    } # end for j
  } # end for i
  cat(' show completed') 
} # end function cplots.show

cplots.save <- function(cplots, fname='', 
      dlay=0, R.save=TRUE, png.save=TRUE) { # save cplots into file fname
  
  if (fname=='') { # ask user for file name
    fname <- readline('Enter save file name (without extension): ')
  }
  
  folder_path <-  paste("../Output/Graphics/", fname, sep='')
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  } else {
    cat("Folder already exists:", folder_path, "\n")
    yn <- readline("Continue (Y/N)?")
    if (!(toupper(yn)=='Y')) return(FALSE) # yn not Y or y then return w/o saving
  } # end if
  
  if (R.save) { # save to an R data file
    save(cplots,
         file= paste(folder_path, '/', fname, '.Rdata', sep=''))
  } # end if R.save
  
  if(png.save) { # save each plot in a separate png files
    library(grDevices) # R graphics devices and support
    
    for (j in 1:length(cplots)) { # loop through the plots
      f <- paste(folder_path, '/', fname, '-', j, '.png', sep = '')
      png(f, width = 800, height = 800) # Opens a PNG device as file f
      print(cplots[[j]]) # make the plot
      # delay between plots, some min delay is needed to allow plot to complete
      Sys.sleep(dlay) # some devices may need time to finish
      dev.new <- dev.off() # saves the plot
    } # end for j
  } # end if png.save
  return(TRUE)
} # end function cplots.save

cplots.load <- function(fname) { # get plots saved in file fname
  folder_path <-  paste("../Output/Graphics/", fname, sep='')
  if(!dir.exists(folder_path)) { # test if folder exists
    cat("ERROR: Folder ", folder_path, "not found")
    return(FALSE)
  } # end if folder exists
  pfname <- paste(folder_path, '/', fname, '.Rdata', sep='')
  if(!file.exists(pfname)) { # test if file exists
    cat("ERROR: File ", pfname, "not found")
    return(FALSE)
  } # end if file exists
  load(file= paste("../Output/Graphics/", fname, '/', fname, '.Rdata', sep=''),
       envir = .GlobalEnv, # save into the global environment
       verbose = TRUE) # list loaded object names to console
} # end function cplots.load
