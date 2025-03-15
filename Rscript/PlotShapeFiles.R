
# Read shape file with the sf library.



# Basic plot of this shape file:
par(mar = c(0, 0, 0, 0))
# plot(st_geometry(canal_sf), col = "#9999d0", bg = "skyblue", lwd = 0.25, border = 1)
plot(st_geometry(marsh_sf), # col = "#80dd80", 
     bg = "white", 
     lwd = 0.25, 
     border = 1, 
     # reset = FALSE, 
     # axes = TRUE
     )
plot(st_geometry(marsh_boundary_sf), # col = "#f2f2f2", 
     bg = "skyblue", lwd = 0.25, 
     border = "red", add = TRUE)

summary(canal_sf)


plot(st_geometry(marsh_sf[marsh_sf$Id==1]),  #col = "#80dd80", 
     bg = "white", 
     lwd = 0.25, 
     border = 1, 
     # reset = FALSE, 
     # axes = TRUE
)
