Cell.Depth.calc <- function(v) { # returns depth(m) as a vector 1..ncell
  # uses cell and CanalVS datasets
  d <- rep(NA, ncell)       # create the depth vector
  c <- cell$type=='Canal'
  m <- cell$type=='Marsh'
  # marsh cell depth
    d[m] <- v[m]/cell$area[m] # marsh 
  # canal cell depth
    for (i in 1:ncanal) {
      d[i] <- approx(CanalVS$Vol[CanalVS$Cell==i],
                     CanalVS$Depth[CanalVS$Cell==i],
                     xout=v[i], method = "linear", rule= 2:1)$y
     } # end for
    return(d)
} # end Cell.Depth.calc

Cell.Volume.calc <- function(d) { # returns volume(m^3) as a vector 1..ncell
  # uses cell and CanalVS datasets
  v <- rep(NA, ncell)       # create the volume vector
  c <- cell$type=='Canal'
  m <- cell$type=='Marsh'
  # marsh cell volume
  v[m] <- d[m]*cell$area[m] # marsh 
  # canal cell depth
  for (i in 1:ncanal) {
    v[i] <- approx(CanalVS$Depth[CanalVS$Cell==i],
                   CanalVS$Vol[CanalVS$Cell==i],
                   xout=d[i], method = "linear", rule= 2:1)$y
  } # end for
  return(v)
} # end Cell.Volume.calc