Canal_Map <- function(z,  plot.date = '', plot.title ='',
                      xy.step = 40,
                      n.steps = 10,
                      zlim    = NULL,
                      ncolor  = 11,
                      axes    = FALSE,
                      pal     = NULL)
  # plots canal values in vector z 
  # cplot.date is date for title as either a date object or text
  # cplot.title is text for plot title
  # because canal cells are very narrow plot n.step times with offset xy.step
  # zlim is 2 element vector of min and max z plotting value
  # ncolor is number of colors to use
  # axes = TRUE then plot xy axes
  # pal is name of color palette function to use, for example rainbow
  # globals: canal_sf, ncanal
  # example:
  #  d <- "2007-07-15"
  #  Canal_Map(sim.Depth[sim.i(d),], n.steps = 20, xy.step = 20, plot.date = d, plot.title = 'Depth', zlim = c(2,5), ncolor = 32)
{
  # default palette function
  if (is.null(pal)) {
    library("RColorBrewer")
    if (ncolor>11) {
      print(paste('Warning: ncolor = ', ncolor,
      ', maximum allowd for default palette RdBu is 11', sep = ''))
      print('ncolor reset to = 11')
      ncolor <- 11
    } # endif
    RdBu.pal <- function(n) brewer.pal(n = n, name = "RdBu")
    pal <- RdBu.pal # sets default palette
  } # end if
  
  c_sf <- canal_sf  # canal boundary multipolygon 
  c_sf$z <- z[1:ncanal] # add z value attributes column
  c_sf3 <- NULL # final repeated multipolygon
  for (i in 1:n.steps) { # create duplicate polygons with a small offsets
    dxy <- (i*xy.step) -(0.5*n.steps*xy.step) # xy offset
    # step in x direction
    c_sf2 <- c_sf # new set of polygons
    st_geometry(c_sf2) <- st_geometry(c_sf2) + c(dxy, 0) # add offset
    st_crs(c_sf2) <- st_crs(c_sf) # restore crs to allow merger
    c_sf3 <- rbind(c_sf3, c_sf2) # merge previous with new
    # step in y direction
    c_sf2 <- c_sf # new set of polygons
    st_geometry(c_sf2) <- st_geometry(c_sf2) + c(0, dxy) # add offset
    st_crs(c_sf2) <- st_crs(c_sf) # restore crs to allow merger
    c_sf3 <- rbind(c_sf3, c_sf2) # merge previous with new
  } # end for
  # set color breaks
  if (is.null(zlim)) b <- "pretty" # default style for breaks
  else b <- seq(zlim[1], zlim[2], length.out = ncolor+1) 
  # create the plot
  # *** breaks and palette options are not working properly and are commented out ***
  plot(c_sf3["z"],  # plot the geometry with values from z column
       border = NA, # do not plot the borders
       # breaks = b,  # vector of breaks or 'pretty'
       axes = axes, # TRUE or FALSE, plot the axes?
       main = paste(plot.title, plot.date), # plot title
       # pal = pal,   # color palette function
       key.pos = 4) # color key on 1-bottom, 2-left, 3-top, 4-right, NULL-omit
  return(TRUE)
} # end canal.map

