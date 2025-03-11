# plot calculated S10 discharge
s <- seq(4.7,5.7,0.01)
Q <- QCalcOutS10(s)
summary(Q)
plot(s,Q, type="l", )
# data for S10 discharge function
# units of x = feet, units of y = 10^3 cubic meters^3/day 
x <- c(-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
y <- c(0,32,113,64,94,148,311,238,175,328,288,450,765,957,1323,2274,3188,2832,4469,5901,7406,6602,6356,8116)
plot(x,y, type="b", xlab = 'Stage-A1 (ft)', ylab = '10^3 m^3/day')


## quick plot of results
# depth
x <- CellDepth(state1[1,2:40])
xx <- CellDepth(state1[2,2:40])
plot(x, col='red', ylab = 'Depth (m)', ylim = c(0,10))
points(xx, col='green')

# stage

y <- cell$E0 + x
yy <- cell$E0 + xx
plot(y, col='red', ylab = 'Stage (m)', ylim = c(-1,20))
points(yy, col='green')

plot(y, col='red', ylab = 'Stage (m)', ylim = c(4,6))
points(yy, col='green')

# compare stage to Berkeley Madonna simulation and Observed values
 # import BM stage simulation and observations
  library(readxl)
  BMSimObsStage <- read_excel("../DataSets/BMSimObsStage.xlsx", 
                              sheet = "Export", skip = 1)
  # View(BMSimObsStage)
  
  # test the CellVolume function using BM output
  x <- 1:39
  y <- as.numeric(BMSimObsStage[1,54:92])
  plot(x, y, type='l', col='blue', 
            ylab='Init BM Volume (m^3)')
  yy <- CellVolume(BMSimObsStage[1,15:53] - cell$E0)
  lines(x,yy, col='red')
  
  # Initial Volume
  plot(1:39, BMSimObsStage[1,54:92], type='l', col='blue', 
       ylab='Init BM Volume (m^3)')
  lines(1:39,CellVolume(Einit$elev - cell$E0), col='red')
  
  # Initial Stage
  plot(1:39, BMSimObsStage[1,15:53], type='l', col='blue', ylim=c(4,5.5),
       ylab='Init BM Stage (m)')
  lines(1:39, BMSimObsStage[2,15:53], col='blue')
  lines(1:39,sim.Stage[1,], type='l', col='red') 
  lines(1:39,sim.Stage[2,], type='l', col='red')
  
  # 1-8C
  x <- 1:(STOPDAY-STARTDAY+2)
  plot(x, BMSimObsStage$E9[x], type='l', col='blue', ylim=c(4,5.5),
       ylab='1-8C Stage (m)')
  lines(x,BMSimObsStage$obs_18c[x], type='l', col='green')
  lines(x,sim.Stage[x,9], type='l', col='red') 
  # North
  plot(x, BMSimObsStage$E35[x], type='l', col='blue', ylim=c(4,5.5),
       ylab='North Stage (m)')
  lines(x,BMSimObsStage$obs_north[x], type='l', col='green')
  lines(x,sim.Stage[x,35], type='l', col='red') 
  # 7-9
  plot(x, BMSimObsStage$E37[x], type='l', col='blue', ylim=c(4,5.5),
       ylab='7-9 avg Stage (m)')
  lines(x,BMSimObsStage$obs_aves7s9[x], type='l', col='green')
  lines(x,sim.Stage[x,37], type='l', col='red') 
  # south
  plot(x, BMSimObsStage$E38[x], type='l', col='blue', ylim=c(4,5.5),
       ylab='South Stage (m)')
  lines(x,BMSimObsStage$obs_south[x], type='l', col='green')
  lines(x,sim.Stage[x,38], type='l', col='red') 
  
  # import BM flows from run using CalcQRo=0, historic flows
    BMInOutHistoric <- read_excel("../DataSets/BMInOutHistoric.xlsx", 
                                  sheet = "Qinouthist", skip = 1)
    # View(BMInOutHistoric)
    plot(x,BMInOutHistoric$S39_out[x], type='l', col='blue')
    
    plot(x,BMInOutHistoric$S10A_out[x], type='l', col='blue')
    plot(x,Outflow$S10A_C[x+STARTDAY-1], type='l', col='red')

    # ET
    plot(x,BMInOutHistoric$ET[x], type='l', col='blue', ylab='ET')
    lines(x,PET$ET[x+STARTDAY-2], col='red') # ET in PET is shifted by 2 days!
    
    lines(x,BMInOutHistoric$ETm39[x], type='l', col='brown', ylab='ET')
    lines(x,PET$ET[x+STARTDAY-2], col='red') # ET in PET is shifted by 2 days!
    
    