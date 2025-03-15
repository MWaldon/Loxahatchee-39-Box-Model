# Canal Volume-Depth plots
# script make_datasets must run before this script to load canal dataset

source('depth.R') # canal depth function

nV <- 50
vv <- rep(NA,nV)
d <- matrix(nrow=nV, ncol=11) # depth (m)

for (i in 1:nV) {
  vv[i] <- 15000000*(i-1)/nV # cell volume
  v <- rep(vv[i], 39) # cell volume vector
  d[i,] <- depth(v)[1:11]
  } # end for i


for (i in 1:11) {
  plot(CanalVS$Vol[CanalVS$Cell==i],
      CanalVS$Depth[CanalVS$Cell==i], 
      main=i, xlab='Volume (m^3)', ylab='depth (m)', col='blue')
  lines(vv, d[,i], col = "red" )
  } # end for i
