ls.add <- function(xyz, lmax) { # add points along a line segment
  # xyz is a 2x3 matrix of beginning and ending coordinates
  # lmax is a maximum final distance for any ls point from a xy point
  d <- dist.p2p(xyz[1:2,1], xyz[1:2,2]) # total length of line segment
  nadd <- trunc(d/(2*lmax)) -1 # number of points to add
  ntot <- nadd+2 # final number of points including end points
  xyz.new <- matrix(nrow = ntot, ncol = 3) # define the result matrix
  xyz.new[1,] <- xyz[1,]
  xyz.new[ntot,] <- xyz[2,]
  dxyz <- (xyz[2,]-xyz[1,])/(nadd+1) # linear increments
  if (nadd>0) {
    for (i in 1:(nadd)) { # loop through new points
      xyz.new[(i+1),] <- xyz[1,] + (i*dxyz)
    } # end for i
  } # end if
  return(xyz.new)
} # end function ls.add

vertices.add <- function(lmax) { # add vertices to long segments
  # lmax is a maximum length
  # global vertices
  # returns verts.new, dataframe with added vertices
  verts <- vertices # verts is a dataframe of polygon information
  np <- max((verts$Id)) # number of polygons
  verts.new <- verts[FALSE,] # create empty dataframe with verts structure
  
  if (FALSE) { # ------------------------------------------------------
    for (i in 1:np) { # loop through all of the polygons
      pg <- verts[verts$Id==i,] # the ith polygon
      # number of line segments is one less than munber of vertices
      ls <- length(pg$Id)-1  
      for (j in 1:ls) { # loop through line segments in pg
        lstart <- pg[pg$n==j,]     # first vertex of segment
        lend   <- pg[pg$n==(j+1),] # last vertex of segment
        xyz <- matrix(nrow=2, ncol=3) # coordinates of strart and end of segment
        xyz[1,] <- c(lstart$x, lstart$y, lstart$z) # coordinates of start of segment
        xyz[2,] <- c(  lend$x,   lend$y,   lend$z) # coordinates of end of segment
        xyz.new <- ls.add(xyz, lmax)
        n.new <- length(xyz.new$x)
        for (k in 1:(n.new)) {
          pnext <- pg[1,]
          pnext$x <- xyz[k,1]
          pnext$y <- xyz[k,2]
          pnext$z <- xyz[k,3]
          verts.new <- rbind(verts.new, pnext)
        }
        verts.new <- rbind(verts.)
        
        
        
        
        verts.new <- rbind(verts.new, pg[pg$n==j,]) # first vertex of segment
        verts.new <- rbind(verts.new, pg[pg$n==(j+1),]) # last vertex of segment
        
        next <- pg[pg$n==(j+1),] # end of line segment
        xy2 <- c(next$x, next$y) # coordinates of end of segment
        d <- lmax # initialize d for loop
        while (d>=lmax) { # loop until d<lmax
          d <- dist.p2p(xy1,xy2)   # length of segment
          
        }
        
        
        
        if (d<lmax) { # length if too long, add a point
          # add another vertex on this line segment
        }
        
        # until d<lmax
        
      } # end for j
      
    } # end for i
  } # end if (FALSE) ------------------------------------------------------
  
  
} # end function vertices.add



MarshMap.contour <- function(z, cplot.date = '', cplot.title ='',
                             raster.length = 500,
                             idp =           2.0,
                             zlimit =        NULL,
                             UseCentroid =   FALSE,
                             boundary.add =  TRUE) 
{ # contour plotting function
  # Plot contours of z over the marsh map, returns a plot object
  # z is vector length ncell of values for each polygon by cell number icell
  #   for example, z = stage, depth, soil elevation, ..., in all cells by cell #
  # cplot.date is date for title as either a date object or text
  # cplot.title is text for plot title
  # raster.length is the distance between raster points
  # zlimit is a vector with min max z plotting values, example zlimit = c(0,1.5)
  # idp is the inverse distance weighting power (smaller gives smoother, distant
  # objects have geater weight)
  # UseCentroid = TRUE then use the cell centroid rather than plotting point
  # global marsh_sf, ncell, ncanal
  #   (note: programming this function was initially assisted by Perplexity.ai)
  
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
  
  # Create a dataframes for interpolation
  z_points <- data.frame(x = coords[,1], y = coords[,2], z = zs)
  
  if (boundary.add) { # add points along polyfon boundaries
    # add coordinates and values at polygon vertices
    verts <- vertices # marsh polygon vertices (vertices is saved by sf.read()
    
    # polygons are closed so first and last vertex are the same point
    verts <- vertices.add(raster.length) # add vertices to long segments
    verts <- verts[verts$n != 1,]  # drop the redundant first polygon point n==1 
    # make a matrix of the unique coordinate pairs plus z data
    xyz <- cbind(verts$x,verts$y) # x & y coordinates 
    nxyz <- dim(xyz)[1] # number of points
    xyz <- cbind(xyz, rep(c(), nxyz)) # add z coordinate, initialize to NA
    for (i in 1:nxyz) {
      nc <- verts$b_n[i] # number of cells bordering xy[i]
      zb <- rep(NA, nc) # initialize vector of cell z values bordering xy[i]
      for (j in 1:nc) {
        ic <- verts$b_list[[i]][j]
        zb[j] <- z[ic] # z for jth cell bordering ith vertex
      } # end for j
      xyz[i,3] <- mean(zb)
    } # end for i
    verts$z <- xyz[,3] # save the z value in verts
    # 
    xyz <- unique(xyz)
    minxy <- 20
    nxyz <- dim(xyz)[1]
    verti <- list()
    for (i in 1:nxyz) { # set z = mean of all instances
      verti[i] <- verts[abs(verts$x-xyz[i,1])<minxy & abs(verts$y==xyz[i,2])<minxy, ]
      #verti <- mean(m_sf$z[m_sf$icell])
    }
    # add the boundary points to the z_points dataframe
    xyz <- data.frame(xyz) # convert to dataframe
    names(xyz) <- names(z_points) # names must be identical for rbind
    z_points <- rbind(z_points, xyz) # add boundary xyz values 
    zs <- z_points$z # update zs with added values
  } # end if boundary.add
  
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
  # plot the raster map, then the cells, then the contours
  plot(z_raster, main = paste(cplot.title, cplot.date), zlim = zlimit)
  plot(m_sf_sp, add = TRUE, border = "yellow")
  contour(z_raster, add = TRUE)
  
  # save the plot as object cplot
  cplot <- recordPlot()
  return(list(cplot, z_points)) # plot completed 
} # end MarshMap.contour