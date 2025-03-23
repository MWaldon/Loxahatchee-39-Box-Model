# Particle tracking - 
#   Drop particle(s), Track advective movement, display position

# dt is time step, use dt=one day

# Functions
# link.xy(link, dist) # return xy at distance (m) from upstream cell
# link.drop(link, time) # calculate distance moved in link
# cell.drop(cell, time) # determines next link, returns next link number
# part.disp(link, dist) # plot the particle location along the link

# Main 
# Display the link node map in UTM
# Ask user for link number
# Ask user for start date, default = Start.Date
# Display initial particle

# Loop until particle reaches the outflow (or maximum days is reached)
  # link.drop 

  # if distance moved is less than link length (link$Radius)
    # display the particle
  # else # particle moved past the end of the link into downstream cel
    # cell.drop

