# Explore shape files with maps and calculations

plot(marsh_sf$cen_X,marsh_sf$cen_Y, col='red')
points(canal_sf$Cen_X,canal_sf$Cen_Y)


# Example to get the polygon coordinates from a shape file 
#  st_coordinates(marsh_sf$geometry[2])
#  lines(st_coordinates(marsh_sf$geometry[2])[,1],st_coordinates(marsh_sf$geometry[2])[,2], col='red')

plot(marsh_sf) 

marsh_sf_temp <- marsh_sf
marsh_sf_temp$Marsh_Depth <- sim.Depth[100,(ncanal+1):ncell]
plot(marsh_sf_temp["Marsh_Depth"])

