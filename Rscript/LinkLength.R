# Explore links

# calculte the length of each link
  llen <- rep(NA,nlink)
  for (i in 1:nlink) {
    u <- link$up[i]
    d <- link$dn[i]
    xyu <- cell.xy(u)
    xyd <- cell.xy(d)
    llen[i] <- sqrt(((xyd['x']-xyu['x'])^2) + ((xyd['y']-xyu['y'])^2))
  } # end for 
  print((llen-link$Radius)/link$Radius*100)

 
  
  