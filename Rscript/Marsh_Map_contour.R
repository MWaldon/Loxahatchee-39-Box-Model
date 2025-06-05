MarshMap.contour <- function(z, cplot.date = '', cplot.title ='', 
                             LinkPoints = FALSE, UseCentroid = FALSE){
# Plot contours of z over the marsh map, 
  # z is vector length ncell of values for each polygon by cell number icell
  # cplot.date is date for title as either a date object or text
  # cplot.title is text for plot title
  # LinkPoints = TRUE then additional points are added at link-cell boundary xy
  # UseCentroid = TRUE then use the cell centroid rather than plotting point
  # global marsh_sf, ncell, ncanal
#   (programming was assisted by Perplexity.ai)

# Load required libraries
  library(sf)         # For handling shapefiles
  library(raster)     # For raster operations
  library(gstat)      # For spatial interpolation
  library(dplyr)      # For sorting function arrange

# Prepare data
  m_sf <- marsh_sf # copy of marsh shape file
  nm <- ncell-ncanal # number of marsh cells
  m_sf$z <- rep(NA,nm) # initialize attribute for plotting
  m.geom <- m_sf$geometry

# Extract polygon centroids and their elevations
  # centroids <- st_centroid(m_sf)
  # coords <- st_coordinates(centroids)
  coords <- matrix(nrow = nm, ncol = 2)
  for (i in (ncanal+1):ncell) { # set coordinates of points
    j <- m_sf$Id[m_sf$icell==i]
    # use plotting point for cell (centroid = FALSE)
    coords[j,] <- cell.xy(i, centroid = UseCentroid) 
  }

# add z attribute to m_sf  
for (i in 1:nm) { #loop through marsh cell Id
  j <- Id2icell[Id2icell$type=='m' & Id2icell$Id==i,]$icell # j is the cell number
  m_sf$z[m_sf$icell==j] <- z[j] # put z into m_sf
}
if (LinkPoints) { # link point section not yet implemented or tested
# interpolate additional points using links
  for (i in 1:nlink) {
    # only add points on marsh-marsh links
    if (link$type[i]=='mm') { # marsh-marsh link
      u <- link$up[i] # upstream cell number
      d <- link$dn[i] # downstream cell number
      xyu <- cell.xy(u, centroid = FALSE) # upstream coordinates
      xyd <- cell.xy(d, centroid = FALSE) # downstream coordinates
      #pg <- marsh_sf$geometry[marsh_sf$icell==d][[1]][[1]][[1]] # downstream polygon
      #pg <- m.geom[[Id2icell$Id[d]]]
      pg <- m_sf$geometry[[Id2icell$Id[Id2icell$icell==d]]]
      pg <- as.matrix(pg)
      xy3 <- find_polygon_line_intersection(pg, xyd, xyu)
      coords <- rbind(coords, xy3) # add new row
      } # end if
    
    } # end for i
} # end if LinkPoints

# add coordinates and values at polygon vertices
  verts <- verticies.list(marsh_sf) # get all marsh polygon verticies
  # polygons are closed so first and last vertex are the same point
  verts <- verts[verts$n != 1,]  # drop the redundant first polygon point n==1 
  # make a matrix of the unique coordinate pairs
  xyz <- cbind(verts$x,verts$y)
  xyz <- cbind(xyz, rep(NA, dim(xyz)[1])) # add z coordinate, initialize to NA
  xyz <- unique(xyz)
  minxy <- 20
  nxyz <- dim(xyz)[1]
  verti <- list()
  for (i in 1:nxyz) { # set z = mean of all instances
    verti[i] <- verts[abs(verts$x-xyz[i,1])<minxy & abs(verts$y==xyz[i,2])<minxy, ]
    #verti <- mean(m_sf$z[m_sf$icell])
  }
   
# create value dataframe
  zs <- m_sf$z # zs is z values rearranged to correspond to m_sf$Id

# Create a dataframes for interpolation
  z_points <- data.frame(x = coords[,1], y = coords[,2], z = zs)

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
  coordinates(z_points) <- ~x+y

# IDW interpolation, inverse distance weighting interpolation
  z_gs <- gstat(formula = zs ~ 1, locations = z_points, nmax = 7, 
                   set = list(idp = 2.0))
  z_raster <- interpolate(r, z_gs)

# Mask Raster to Marsh Polygon Region
# Rasterize the polygon region to use as a mask
  m_sf_sp <- as(m_sf, "Spatial")
  z_raster <- mask(z_raster, m_sf_sp)

# Draw contour map
  plot(z_raster, main = paste(cplot.title, cplot.date))
  plot(m_sf_sp, add = TRUE, border = "yellow")
  contour(z_raster, add = TRUE)

  return('plot finished') # plot completed 
} #end MarshMap.contour