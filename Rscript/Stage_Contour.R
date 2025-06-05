# Plot contours of marsh stage or depth.
#   (programming assisted by Perplexity.ai)

# Load required libraries
  library(sf)         # For handling shapefiles
  library(raster)     # For raster operations
  library(gstat)      # For spatial interpolation
  library(ggplot2)    # For plotting (optional)

# Prepare data
  cplot.date <- as.Date('1999-01-01')
  cplot.day <- Date2Day(cplot.date)
  
  # extract marsh depth and stage for plotting
  cplot.Depth <- sim.Depth[cplot.day, ((ncanal+1):ncell)]
  cplot.Stage <- sim.Stage[cplot.day, ((ncanal+1):ncell)]
  
  m_sf <- marsh_sf # copy of marsh shape file
  nm <- ncell-ncanal # number of marsh cells
  m_sf$Depth <- rep(NA,nm)
  m_sf$Stage <- rep(NA,nm) 
  
  # Extract polygon centroids and their elevations
  centroids <- st_centroid(m_sf)
  coords <- st_coordinates(centroids)
  
  # add depth and stage attributes to m_sf  
  for (i in (ncanal+1):ncell) { #loop through marsh cell number
    # put depth and stage into m_sf
    j <- i-ncanal 
    m_sf$Depth[m_sf$icell==i] <- cplot.Depth[j]
    m_sf$Stage[m_sf$icell==i] <- cplot.Stage[j]
  }
  
  # create depth and stage dataframes
  Depths <- m_sf$Depth
  Stages <- m_sf$Stage
  # Create a dataframes for interpolation
  Depth_points <- data.frame(x = coords[,1], y = coords[,2], Depth = Depths)
  Stage_points <- data.frame(x = coords[,1], y = coords[,2], Stage = Stages)

# Create a Raster Grid
  # Define the extent and resolution
  region_bbox <- st_bbox(m_sf) # bounding box 
  r <- raster(
    xmn = region_bbox["xmin"], xmx = region_bbox["xmax"],
    ymn = region_bbox["ymin"], ymx = region_bbox["ymax"],
    res = 100  # Adjust resolution as needed
  )
 
# Interpolate Elevation Values
  # Convert dataframes to SpatialPoints DataFrames
  coordinates(Depth_points) <- ~x+y
  coordinates(Stage_points) <- ~x+y
  
  # IDW interpolation, inverse distance weighting interpolation
  Depths_gs <- gstat(formula = Depths ~ 1, locations = Depth_points, nmax = 7, 
              set = list(idp = 2.0))
  Depth_raster <- interpolate(r, Depths_gs)
  
  Stages_gs <- gstat(formula = Stages ~ 1, locations = Stage_points, nmax = 7, 
                     set = list(idp = 2.0))
  Stage_raster <- interpolate(r, Stages_gs)
  
#  Mask Rasters to Marsh Polygon Region
  # Rasterize the polygon region to use as a mask
  m_sf_sp <- as(m_sf, "Spatial")
  Depth_raster <- mask(Depth_raster, m_sf_sp)
  Stage_raster <- mask(Stage_raster, m_sf_sp)
  
# Draw contour maps
  plot(Depth_raster, main = "Depth Contour Map")
  plot(m_sf_sp, add = TRUE, border = "yellow")
  contour(Depth_raster, add = TRUE)

  plot(Stage_raster, main = "Stage Contour Map")
  plot(m_sf_sp, add = TRUE, border = "yellow")
  contour(Stage_raster, add = TRUE)  
  