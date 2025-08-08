  gages.area <- function(gages.name, gages_sf.xy, bound_sf) {
    # return area associated with each gage
    # This function was written by chatgpt.com on 8/8/2025
    #   https://chatgpt.com/share/68961416-44e4-800a-8941-ec8616d7c8a9
    library(sf)
    
    # Convert multipoint sfg to sf object with separate point geometries
    gages_sf <- st_sf(
      name = gages.name,
      geometry = st_cast(st_sfc(gages_sf.xy, crs = st_crs(bound_sf)), "POINT")
    )
    
    # Create Voronoi polygons from the points
    voronoi_geom <- st_voronoi(st_union(gages_sf))
    
    # Extract polygons from the Voronoi geometry
    voronoi_polys <- st_collection_extract(voronoi_geom, "POLYGON")
    
    # Convert to sf with gage names
    voronoi_sf <- st_sf(name = gages.name, geometry = voronoi_polys)
    
    # Clip Voronoi polygons to boundary
    clipped_sf <- st_intersection(voronoi_sf, st_sfc(bound_sf, crs = st_crs(bound_sf)))
    
    # Calculate areas
    areas <- st_area(clipped_sf)
    
    # Return named vector
    areas_vec <- as.numeric(areas)
    names(areas_vec) <- clipped_sf$name
    return(list(areas = areas_vec, clipped_polygons = clipped_sf, points = gages_sf))
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
      2, 3,
      5, 4), 
    ncol = 2, byrow = TRUE
  )
  gages_multipoint <- st_multipoint(gage_coords)
  
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
  
  # Print areas
  print(result$areas)
  
  
  # -------------------------------------------------------
  # Plot results
  # -------------------------------------------------------
  plot(st_geometry(st_sfc(boundary_polygon)), col = NA, border = 'black', lwd = 2, main = "Voronoi Polygons Clipped to Boundary")
  plot(st_geometry(result$clipped_polygons), add = TRUE, col = c("#FF9999","#99FF99","#9999FF","#FFFF99"))
  plot(st_geometry(result$points), add = TRUE, col = "black", pch = 19, cex = 1.5)
  text(gage_coords[,1], gage_coords[,2], labels = gage_names, pos = 3)
  
  } # end if
  