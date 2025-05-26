# Explore shape files with maps and calculations
library(sf)       # shape files

plot(marsh_sf$cen_X,marsh_sf$cen_Y, col='red')
points(canal_sf$Cen_X,canal_sf$Cen_Y)


# Example to get the polygon coordinates from a shape file 
#  st_coordinates(marsh_sf$geometry[2])
#  lines(st_coordinates(marsh_sf$geometry[2])[,1],st_coordinates(marsh_sf$geometry[2])[,2], col='red')

plot(marsh_sf) 

marsh_sf_temp <- marsh_sf
marsh_sf_temp$Marsh_Depth <- sim.Depth[100,(ncanal+1):ncell]
plot(marsh_sf_temp["Marsh_Depth"])


# exploring shapefile structure (from former file named deleteMe.R)

# Display the link node map in UTM
plot(st_geometry(marsh_sf), # col = "#80dd80", 
     bg = "white", 
     lwd = 0.25, 
     border = 'yellow', 
     # reset = FALSE, 
     axes = TRUE)

x <- st_point_on_surface(canal_sf)
for (i in 1:ncanal) {
  geom <- canal_sf$geometry[[i]][[1]][[1]]
  lines(geom, type='l', col='blue') #, main=i)
  points(canal_sf$Cen_X[i],canal_sf$Cen_Y[i], col='red')
  points(x$geometry, col='green')
} # end for i

x <- st_point_on_surface(marsh_sf)

for (i in 1:(ncell-ncanal)) {
  geom <- marsh_sf$geometry[[i]][[1]][[1]]
  # lines(geom, type='l', col='blue') #, main=i)
  points(marsh_sf$cen_X[i],marsh_sf$cen_Y[i], col='brown')
  points(x$geometry, col='darkgreen')
} # end for i


