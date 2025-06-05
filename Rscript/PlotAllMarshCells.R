# Plot all marsh cells with centroid (blue) and plotting point (red) 
for (iu in (ncanal+1):ncell) {
  pgu <- vert[vert$icell==iu,]
  plot(pgu$x, pgu$y, type='b', col='brown', main=paste('cell #',iu))
  
  xycentu <- cell.xy(iu, centroid = TRUE)
  #print(c('xy centroid up ', xycentu))
  points(xycentu[1], xycentu[2], col='blue')
  
  xyplotpu <- cell.xy(iu, centroid = FALSE)
  #print(c('xy plot point up ', xyplotpu))
  points(xyplotpu[1], xyplotpu[2], col='red')
  
  print(paste(iu,xycentu,xyplotpu))
}
