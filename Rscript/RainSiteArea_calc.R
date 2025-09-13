# Calculate the area related to each rain gage in the timeseries extension to 2025
source('gages_area.R')

library(sf)
# Create rain gage names
gage_names <- c('S-6','S-39','S-5A','LOXWS','STA1W')

# Create multipoint sfg object for gage coordinates
gage_coords <- matrix(
  c( 555264, 570084, 562922, 577171, 556985,
    2928106,2915323,2951651,2931511,2946124), 
  ncol = 2, byrow = FALSE)

gages_multipoint <- st_multipoint(gage_coords)

# Create rectangular boundary polygon sfg
boundary_coords <- marsh_boundary_sf$geometry[[1]][[1]]
boundary_polygon <- st_polygon(list(boundary_coords))

# Run function
result <- gages.area(gage_names, gages_multipoint, boundary_polygon)

# Print areas
print(result$areas/1000000)

# calculate and print centroids
centr <- st_coordinates(st_centroid(result$clipped_polygons$geometry))
rownames(centr) <- rownames(result$centroids)
print(centr)

# -------------------------------------------------------
# Plot results
# -------------------------------------------------------
plot(st_geometry(st_sfc(boundary_polygon)), col = NA, border = 'black', 
     lwd = 2, main = "Voronoi Polygons Clipped to Boundary")
plot(st_geometry(result$clipped_polygons), add = TRUE, 
     col = c("#FF9999","#99FF99","#9999FF","#BBBB99","#FF1111"))
plot(st_geometry(result$points), add = TRUE, 
     col = "black",
     pch = 19, cex = 1.5)
text(gage_coords[,1], gage_coords[,2], labels = gage_names, 
     pos = c(2,4,2,4,2),
     col = 'black')
text(centr[,1], centr[,2], labels = rownames(centr),
     col = 'black')     


