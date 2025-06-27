MarshMap.contour <- function(z, cplot.date = '', cplot.title ='',
                             raster.length = 500,
                             idp =           1.0,
                             zlimit =        NULL,
                             UseCentroid =   FALSE,
                             boundary.add =  TRUE,
                             plot.do =       TRUE,
                             plot.return =   FALSE) 
{ # contour plotting function
  # Plot contours of z over the marsh map, 
  # returns a list containing:
  #   spatial points dataframe with all points used to generate the plot
  #   optionally list contains the plot object if plot.return = TRUE
  # z is vector length ncell of values for each polygon by cell number icell
  #   for example, z = stage, depth, soil elevation, ..., in all cells by cell #
  # cplot.date is date for title as either a date object or text
  # cplot.title is text for plot title
  # raster.length is the distance between raster points
  # zlimit is a vector with min max z plotting values, example zlimit = c(0,1.5)
  # idp is the inverse distance weighting power (smaller gives smoother, distant
  #   objects have geater weight)
  # UseCentroid = TRUE then use the cell centroid rather than plotting point
  # boundary.add = TRUE then add boundary vertices z values for interpolation
  # plot.do = TRUE then do draw the plot
  # plot.return = TRUE then return the plot object in the return list
  # global marsh_sf, ncell, ncanal
  #   (note: programming this function was initially assisted by Perplexity.ai)

# -------------------------- FUNCTIONS -----------------------------------  
ls.add <- function(xyz, lmax) { # add points along a line segment
  # xyz is a 2x3 matrix of beginning and ending coordinates
  # lmax is a maximum final distance for any ls point from an xy point
  d <- dist.p2p(xyz[1,1:2], xyz[2,1:2]) # total length of line segment
  nadd <- trunc(d/(2*lmax)) -1 # number of points to add
  ntot <- nadd+2 # final number of points including end points
  xyz.new <- matrix(nrow = ntot, ncol = 3) # define the result matrix
  xyz.new[1,] <- xyz[1,]
  xyz.new[ntot,] <- xyz[2,]
  dxyz <- (xyz[2,]-xyz[1,])/(nadd+1) # linear increments vector dx, dy, dz
  if (nadd>0) { # if there are any point to add
    for (i in 1:(nadd)) { # loop through new points
      xyz.new[(i+1),] <- xyz[1,] + (i*dxyz)
    } # end for i
  } # end if
  return(data.frame(x=xyz.new[,1], y=xyz.new[,2], z=xyz.new[,3]))
} # end function ls.add

z_points.add <- function(lmax, z_points, z) { # add vertices to z_points
  # also add more points along long line segments
  # lmax is a maximum length
  # z_points is dataframe of xyz for cell plotting ordered by marsh cell$Id
  # z is vector length ncell of values for each polygon by cell number icell
  # global vertices (all marsh polygon vertices)
  # returns z_points.new, z_points dataframe with added xyz values
  z_points.new <- z_points # create new dataframe for expanded xyz points
  np <- dim(vertices)[1] # number of rows, or number of marsh polygon vertices
  vz <- rep(NA,np) # z value for all vertices
  for (i in 1:np) { # loop through all the vertices
    # calculate vz, the vertex z value for the ith vertex
    #   vz is the mean of z for each bordering polygon
    cn <- vertices[i,]$b_list[[1]] # vector of cell numbers of bordering cells
    cz <- z[cn] # vector of z values for bordering cells
    vz[i] <- mean(cz) # z mean of border cells
    # the polygons are closed, loop through all line segments 
    if (vertices[i,]$n!=1){ # skip first vertex in each polygon
      # define endpoints and z of the current line segment
      xyz <- matrix(byrow = TRUE, nrow = 2, ncol = 3, data = 
                      c(vertices[i-1,]$x, vertices[i-1,]$y, vz[i-1],
                        vertices[i,  ]$x, vertices[i,  ]$y, vz[i]  ))
      # add points for this line segment to new points dataframe
      z_points.new <- rbind(z_points.new, ls.add(xyz, lmax))
    } # end if not first vertex in polygon
  } # end for i
  # z_points.new <- unique(z_points.new) # keep unique xyz points
  return(z_points.new) # return the extended list
} #end function z_points.add

# --------------------------------------------------------------
# Load required libraries
  library(sf)         # For handling shapefiles
  library(raster)     # For raster operations
  library(gstat)      # For spatial interpolation
  library(dplyr)      # For sorting function arrange
  
# Prepare data
  m_sf <- marsh_sf # copy of marsh shape file
  nm <- ncell-ncanal # number of marsh cells
  m_sf$z <- rep(NA,nm) # initialize attribute z for plotting
  m.geom <- m_sf$geometry
  
# Find polygon xy and their elevations
  coords <- matrix(nrow = nm, ncol = 2) # initialize matrix of polygon xy values
  for (i in (ncanal+1):ncell) { # set xy of points for each marsh polygon
    j <- m_sf$Id[m_sf$icell==i] # Id value
    # xy for polygon icell==i, use plotting point if UseCentroid == FALSE
    coords[j,] <- cell.xy(i, centroid = UseCentroid) 
  }
  
  # add z attribute to m_sf  
  for (i in 1:nm) { # loop through marsh cell Id
    # j is the cell number, look up from marsh cell Id
    j <- Id2icell[Id2icell$type=='m' & Id2icell$Id==i,]$icell 
    m_sf$z[m_sf$icell==j] <- z[j] # put z into m_sf
  }
  
  # create value dataframe
  zs <- m_sf$z # zs is z values rearranged to correspond to m_sf$Id
  
  # Create a dataframe containing xyz data for interpolation
  z_points <- data.frame(x = coords[,1], y = coords[,2], z = zs)

# optionally, add boundary xyz points to z_points
  if (boundary.add) { # add points along polyfon boundaries
    # add coordinates and values at polygon vertices
    z_points <- z_points.add(raster.length, z_points, z) # add boundary points
    zs <- z_points$z # update zs with added values
  } # end if boundary.add

# create the plotting objects  
  # Create a Raster Grid
  # Define the extent and resolution
  region_bbox <- st_bbox(m_sf) # bounding box 
  r <- raster(
    xmn = region_bbox["xmin"], xmx = region_bbox["xmax"],
    ymn = region_bbox["ymin"], ymx = region_bbox["ymax"],
    res = raster.length  # raster resolution, adjust as needed in function arg
  )
  
  # Interpolate Elevation Values
  # Convert dataframes to SpatialPoints DataFrames
  coordinates(z_points) <- ~x+y # specifies columns in z_points that are the xy
  
  # IDW (inverse distance weighting)  interpolation
  #  create a gstat object z_gs
  #  simple kriging uses the formula z~1
  #  nmax is the number of nearest observations that should be used
  #  idp is the inverse distance weighting power
  z_gs <- gstat(formula = zs ~ 1, locations = z_points, nmax = 7, 
                set = list(idp = idp))
  # create RasterLayer with interpolated values using a fitted model object
  z_raster <- interpolate(r, z_gs) # object=r, z_gs=model
  
  # Mask Raster to Marsh
  # Rasterize the polygon region to use as a mask
  m_sf_sp <- as(m_sf, "Spatial")
  # mask z_raster
  z_raster <- mask(z_raster, m_sf_sp)
  
# Draw contour map
  if(plot.do) {
  # plot the raster map, then the cells, then the contours
  plot(z_raster, main = paste(cplot.title, cplot.date), zlim = zlimit)
  plot(m_sf_sp, add = TRUE, border = "yellow")
  contour(z_raster, add = TRUE)
  } # end if plot.do
  
  # save the plot as object cplot
  # note: use coordinates function to get xyz values from the returned
  #   SpatialPointsDataFrame object z_points
  if(plot.return) { # return the points data and plot object
    cplot <- recordPlot()
    return(list(xyz=z_points, cmap=cplot)) 
  } else { # return only the points data
    return(list(xyz=z_points)) 
  }
} # end MarshMap.contour