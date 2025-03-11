# Calculate the daily total flows and total flow to date
library(dplyr)  # for function select
library(scales) # for function alpha (color transparency)

Qout <- rowSums(sim.Outflow)
Qin <- c(rowSums(select(Inflow,-c('DAY','DATE'))),0) # total daily inflow
SurfaceA <- sum(cell$area) # total area (m^2)
# SurfaceAcres <- SurfceA/4046.856422 # total area in acres (not used)
P <- c(PET$P*SurfaceA,0) # precipitation in (m/day)

ET <- rep(0, length(Qout))
FetDepth <- matrix(Fet(sim.Depth), nrow = nrow(sim.Depth))
for (i in 1:length(PET$ET)) { # loop through days
  ET[i] <- 0
  for (j in 1:ncell) { # loop through cells
    ET[i] <- ET[i] - (FetDepth[i,j]*cell$area[j]*PET$ET[i]) # (m^3/day)
  } # end for j
} # end for i


GW <- rep(0, length(Qout))  # seepage loss to groundwater

# Seepage to groundwater
for (i in 1:length(PET$ET)) { # loop through days
  GWloss <- seep*(sim.Stage[i,]-Eb)  # cell loss to groundwater (m/day)
  GW[i] <- - sum(cell$area*GWloss) # seepage (negative) (m^3/day)
} # end for i

Qtot <- Qin + P + Qout + ET + GW
Qcum <- cumsum(Qtot)

V0 <- sum(sim.Volume[1,])    # total initial volume (m^3)
dV <- rowSums(sim.Volume) - V0  # change in total volume (m^3)
Balance <- (Qcum - dV) /SurfaceA
  
DailyQ <- data.frame(Dates, Qout, Qin, P, ET, GW, Qtot)

plot(Dates, Qout/SurfaceA, type='l', col='blue', 
     ylim = c(min(Qout/SurfaceA), max(Qin/SurfaceA, P/SurfaceA)),
     ylab = 'm/day')
lines(Dates, ET/SurfaceA, col='gray')
lines(Dates, GW/SurfaceA, col='black')
lines(Dates, Qin/SurfaceA, col='blue')
lines(Dates, P/SurfaceA, col = alpha("green", 0.5)) # 50% transparency


plot(Dates, cumsum(Qin+P)/SurfaceA, type = 'l', lwd = 2,
     ylab = 'Cumulative flow (m)')
lines(Dates, Qcum/SurfaceA, col='red')
lines(Dates,Balance, type='l', col = 'blue')

par(mfrow = c(1, 2))
# water sources pie chart
pie(c(mean(Qin), mean(P)), angle = -45,
    labels = c('Inflow', 'Precip'), 
    main = 'Sources')
# water losses pie chart
pie(c(mean(-Qout), mean(-GW), mean(-ET)), angle = -45,
    labels = c('Outflow', 'Groundwater', 'ET'), 
    main = 'Losses')
par(mfrow = c(1, 1)) # reset

