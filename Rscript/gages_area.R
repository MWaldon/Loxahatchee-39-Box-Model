  gages.area <- function(gages.name, gages_sf.xy, bound_sf) {
    # return area associated with each gage
    # gages.name is a text vector of gage identifiers
    # gages_sf.xy is a multipoint sfg object of gage xy pairs
    # bound_sf is the boundary polygon 
    # This function was written by chatgpt.com on 8/8/2025
    #   https://chatgpt.com/share/68961416-44e4-800a-8941-ec8616d7c8a9
    library(sf)
    
    ngages <- length(gages.name) # number of gages
    
    # Convert multipoint sfg to sf object with separate point geometries
    gages_sf <- st_sf(
      name = gages.name,
      geometry = st_cast(st_sfc(gages_sf.xy, crs = st_crs(bound_sf)), "POINT")
    )
    
    # Create Voronoi polygons from the points
    #   st_veronoi returns a list of length ngages of list of polygon XYs
    voronoi_geom <- st_voronoi(st_union(gages_sf))
    
    # Extract polygons from the Voronoi geometry
    voronoi_polys <- st_collection_extract(voronoi_geom, "POLYGON")
    
    # Convert to sf
    voronoi_sf <- st_sf(geometry = voronoi_polys)
    
    # find the index in gages.name that corresponds to each Voroni polygon
      # initialize index to gage names for polygons
      pg.index <- c(rep(0,ngages)) 
      # get the coordinates for all the polygons (there are ngages polygons)
      voronoi_polys.xy <- st_coordinates((voronoi_polys)) # colnames=X,Y,L1,L2
      for (i in 1:ngages) { # i is gage name index
        for (j in 1:ngages) { # j is polygon index
          # select XYs for jth polygon
          pg.xyj <- voronoi_polys.xy[voronoi_polys.xy[,4]==j,] 
          is_in <- 
            is.in_polygon(gages_sf.xy[i,1], gages_sf.xy[i,2], # xy of  ith gage
                          pg.xyj[,1], pg.xyj[,2])              # xy of jth polygon
          if (is_in) {pg.index[j] <- i} # jth polygon has ith name
        }      
      }
      
    # Clip Voronoi polygons to boundary
      clipped_sf <- st_intersection(voronoi_sf, st_sfc(bound_sf, crs = st_crs(bound_sf)))
    
    # Calculate areas
    areas <- st_area(clipped_sf)
    names(areas) <- gages.name[pg.index]
    
    # Calculate centroids
    centr <- st_coordinates(st_centroid(clipped_sf$geometry))
    rownames(centr) <- gages.name[pg.index]
    
    # Return named list
    # areas_vec <- as.numeric(areas)
    # names(areas_vec) <- gages.name[pg.index]
    return(list(areas = areas, clipped_polygons = clipped_sf, 
                points = gages_sf, centroids = centr))
  }
  
  if (FALSE) { # skip the example
  # -------------------------------------------------------
  # Example with dummy data
  # -------------------------------------------------------
  
  # Create dummy rain gage names
  gage_names <- c("Gage_A", "Gage_B", "Gage_C", "Gage_D")
  
  # Create multipoint sfg object for gage coordinates
  gage_coords <- matrix(
    c(1, 1,
      4, -1, # 1,
      1, 3,
      5, 4), 
    ncol = 2, byrow = TRUE
  )
  rownames(gage_coords) <- gage_names
  gages_multipoint <- st_multipoint(gage_coords)
  rownames(gages_multipoint) <- gage_names
  
  # Create rectangular boundary polygon sfg
  boundary_coords <- matrix(
    c(0, 0,
      6, -1, #0,
      6, 5,
      0, 5,
      0, 0), 
    ncol = 2, byrow = TRUE
  )
  boundary_polygon <- st_polygon(list(boundary_coords))
  
  # Run function
  result <- gages.area(gage_names, gages_multipoint, boundary_polygon)
  
  # Print areas and centroids
  print(result$areas)
  print(result$centroids)
  
  
  # -------------------------------------------------------
  # Plot results
  # -------------------------------------------------------
  plot(st_geometry(st_sfc(boundary_polygon)), col = NA, border = 'black', lwd = 2, main = "Voronoi Polygons Clipped to Boundary")
  axis(side = 1)
  axis(side = 2)
  plot(st_geometry(result$clipped_polygons), add = TRUE, col = c("#FF9999","#99FF99","#9999FF","#FFFF99"))
  points(st_geometry(result$points), col = "black", pch = 19, cex = 1.5)
  points(result$centroids[,1], result$centroids[,2], col = 'red')
  text(gage_coords[,1], gage_coords[,2], labels = gage_names, pos = 3)
  text(result$centroids[,1], result$centroids[,2], 
       labels = names(result$centroids[,1]), pos = 3)
  
  } # end if
  