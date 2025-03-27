# Particle tracking - 
#   Drop particle(s), Track advective movement, display position
#   Particles start (are dropped into) in a cell. Then a link 
#   flowing out of the cell is randomly selected weighted by link 
#   flow. Particles travel down the link until reaching a new cell.
#   Particles exit through outflow. Currently ET and groundwater
#   losses are ignored.

#   Particle location is specified by a link number and 
#     distance from the upstream end link$up of the link. This 
#     is then converted to a UTM xy coordinate for plotting.

# dt is time step, use dt=one day

# Functions
# link.xy(linkn, dist) # input distance (m) from upstream, return UTM (x,y)
# link.drop(linkn, dist) # calculate distance moved in link to 24:00
# cell.drop(celln) # determines next link, returns next link number
# part.disp(linkn, dist) # plot the particle location along the link

# cell.xy returns xy for cell ( already defined)

library(plotrix)

# set constants
startcolor <- 'red'
trackcolor <- 'black'
cellcolor <- 'green'
UP <- 1  # upstream end
DN <- 0  # downstream end
OT <- -1 # structure outflow

# Define Functions
link.xy <- function(linkn, dist) {# x, y location along a link
  # linkn is link number, dist is distance along link from upstream end
  # returns x, y, and total link length 
  u <- cell.xy(link$up[linkn]) # xy of upstream cell number
  d <- cell.xy(link$dn[linkn]) # xy of downstream cell number
  ux <- u[1] # upstream x and y
  uy <- u[2]
  dx <- d[1] # downstream x and y
  dy <- d[2]
  a <- dx-ux
  b <- dy-uy
  c <- sqrt((a*a)+(b*b)) # length of link (link$radius)
  x <- ux + (a*dist/c)
  y <- uy + (b*dist/c)
  xy <- c(x,y,c)
  names(xy) <- c('x','y', 'len')
  return(xy)
} # end link.xy

link.drop <- function(linkn, dist) { # distance moved in link to 24:00
  # linkn is link number, dist is beginning position of particle
  # time.sim (global) is the current time of simulation
  # return the distance moved from time.sim to the end of day at 24:00
  
  n <- trunc(time.sim)+1 # time row number, also time at 24:00
  v <- sim.Velocity[n,linkn] # link velocity on this day
  dt <- n-time.sim # time left in the current day
  d <- v*dt # distance moved (may be + or -) 
  return(dist+d)
} # end link.drop

cell.drop <- function(celln) { # determines next link
  # returns next link number and link end (Up or DN)
  # global time.sim, UP, DN, OT, sim.Velocity, sim.Outflow
  
  # find all links currently flowing out of the cell
  n <- trunc(time.sim)+1 # time row number, also time at 24:00
  nout <- 0
  # outlist rows=link number, up or down, outflow discharge
  outlist <- data.frame(0,0,0)
  names(outlist) <- c("link", "updn", "Qout")
  for (i in 1:nlink) { # loop through all links
    # if downstream end of link & negative velocity
    if ((link$dn[i]==celln) & (sim.Velocity[n,i]<0) ) {
      # flow is going out of the cell into the DN end of the link
      nout <- nout+1
      outlist[nout,1:3] <- c(i,DN,-sim.Linkflow[n,i])
    } # end if
    # if upstream end of link & positive velocity
    if ((link$up[i]==celln) & (sim.Velocity[n,i]>0) ) {
      # flow is going out of the cell into the UP end of the link
      nout <- nout+1
      outlist[nout,1:3] <- c(i,UP,sim.Linkflow[n,i])
    } # end if
  } # end for i
  # check for structure outflow
  if (celln <= ncanal) { # outflow only for canal cells
    if (sim.Outflow[n,celln]<0) { # currently flowing out?
      nout <- nout+1
      outlist[nout,1:3] <- c(OT,OT,-sim.Outflow[n,celln])
    } # end if
  } # end if
  # outlist$Qout <-  as.numeric(outlist$Qout) ----------------------delete
  # determine which link, if any, the particle enters
  if(nout==0) { # no current outflows
    linkn <-  0 # stay in cell until end of current day
    updn <- 0 # neither UP nor DN, no outflow from cell
  } else {
    weights <- outlist$Qout/sum(outlist$Qout) # weighted probability
    n <- sample(1:nout, size = 1, replace = TRUE, prob = weights)
    linkn <- outlist[n,1]
    updn <-  outlist[n,2]
  } # end else
    values <- c(linkn, updn)
    names(values) <- c('link', 'updn')
    return(values)
} # end cell.drop

link.route <- function(linkn, dist) { # route the particle along the link 
  # global time.sim, sim.Velocity, track
  # continue until the particle leaves the link
  # return next cell number
  d <- dist
  in.link <- TRUE # TRUE if dist is positive and less than link length
  link.len <- link.xy(linkn,0)['len']
  while (in.link) { # particle between link ends
    # take a step
    time.next <-trunc(time.sim)+1 # time at end of day, also row number   
    dt <- time.next - time.sim  # time step
    v <- sim.Velocity[time.next, linkn] # velocity (m/day)
    dx <- dt*v # distance moved (m), positive=downstream, negative=upstream
    d <- d+dx
    # test if it is still inside the link
    in.link <- (d<link.len) & (d>0)
    if (in.link) {
      time.sim <<- time.next # update the global time variable 
      xy <- link.xy(linkn, d)
      points(xy['x'], xy['y'], col=trackcolor)
      # add point to global track dataframe
      ntracks <- length(track[,1])+1 # new track point
      track[ntracks,] <<- c(time.sim, xy['x'], xy['y'], linkn, d)
      
      # print(c('time = ', time.sim)) # -------------------------------
    } # end if
  } # end while
  
  # particle stepped out of the link
  # set d back to old value before the last move, then move exactly to
  #   the correct end with time set to time to reach that end.
  dold <- d-dx 
  # set dx to distance to move to get to the end
  if (d<=0) { # moved into the upstream cell
    dx <- dold
    nextcell <- link$up[linkn]
  } else { # moved into the downstream cell
    dx <- link.len-dold 
    nextcell <- link$dn[linkn]
    }
  dt <- dx/abs(v) # time to move to the end
  # update time, this will not go to a next day because dx is shortened
  # new time.sim is smaller than time.next 
  time.sim <<- time.sim + dt # set the global global time using "<<-"
  xy <- cell.xy(nextcell)
  points(xy['x'], xy['y'], col=cellcolor, cex=2) # plot a point in the new cell
  return(nextcell) 
} # end link.route

# ------------------------Main -------------------------------------
# Display the link node map in UTM
plot(st_geometry(marsh_sf), # col = "#80dd80", 
     bg = "white", 
     lwd = 0.25, 
     border = 'yellow', 
     # reset = FALSE, 
       axes = TRUE)
for (i in 1:nlink) { # plot links
  xyup <- cell.xy(link$up[i])
  xydn <- cell.xy(link$dn[i])
  lines(c(xyup['x'], xydn['x']), c(xyup['y'], xydn['y']), col='gray')
} # end for i

nmax <- 100 # maximum number of cells entered before stop
# Ask user for start date, default = Start.Date
#  for now assume start time
time.sim <- 0 # time (days)

# Ask user for cell number to "drop" particle
nstart <- readline('Enter a starting cell number: ')
nstart <- as.numeric(nstart) # convert to a number
xy <- cell.xy((nstart))
nc <- nstart # nc is the current cell number 

points(xy['x'], xy['y'], col=startcolor, cex=3) # Display initial particle

# initialize track dataframe
track <- data.frame(time.sim, xy['x'], xy['y'], 0, 0)
names(track) <- c('time', 'x', 'y', 'link', 'dist')

# Loop until particle reaches the outflow (or maximum days is reached)
  for (i in 1:nmax) { # main loop
    xy <- cell.xy(nc)
    cdrop <- cell.drop(nc)
    linkn <- cdrop[1]
    if (linkn==-1) { # outflow
      print(paste('outflow of particle from cell ', nc, ' at time = ', time.sim))
      lines(track$x, track$y, col=startcolor)
      break
      } # end if
    if (linkn==0) {# no outflow
      time.sim <- trunc(time.sim+1) # increment time
      
    } else { # send particle onto linkn
        if (cdrop[2]==UP) { # start particle at upstream end
          dist <- 0 # distance along the link =0
        }
        else { # particle is at the downstream end
          dist <- link.xy(linkn,0)['len'] # at downstream end
             } # end else
        # route the particle along the link
        nc <- link.route(linkn, dist) # returns next cell number as nc
        } # end else
    
  } # end for i

  # if distance moved is less than link length (link$Radius)
    # display the particle
  # else # particle moved past the end of the link into downstream cel
    # cell.drop

