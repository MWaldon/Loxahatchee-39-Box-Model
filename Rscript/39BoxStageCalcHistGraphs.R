# plot stage graphs with calculated and historic outflow

library(readxl)   # read Excel worksheets
library(lubridate)# date management functions

# Load required datasets 
  load(file="../Datasets/39-Box-Datasets.Rdata") # created by make_datasets.R
  
  # Create standard group of graphics
  USGS_Stages <- read_excel("../DataSets/Water_Level_timeseries.xlsx", 
                            sheet = "Timeseries", range = "Q4:V5299")
  USGS_Stages[USGS_Stages == -999] <- NA # replace -999 stage with NA
  USGS_Stages$DATE <- as.Date(as.Date('1995-01-01'):as.Date('2009-06-30'))
  # add datum correction estimate
  USGS_Stages$North <- USGS_Stages$North + 0.48
  USGS_Stages$South <- USGS_Stages$South + 0.48
  
  # Qout calculated
  load(file=paste("../Output/","39BoxBase-1995-2009-Qcalculated", 
                  ".Rdata", sep="")) # output from Qout calculated run
  Qcalc.Stage <- sim.Stage
  Qcalc.Depth <- sim.Depth
  Qcalc.Volume <- sim.Volume
  # Qout historic
  load(file=paste("../Output/","39BoxBase-1995-2009-Qhistoric", 
                  ".Rdata", sep="")) # output from Qout historic run
  Qhist.Stage <- sim.Stage
  Qhist.Depth <- sim.Depth
  Qhist.Volume <- sim.Volume
  


# North
plot(USGS_Stages$DATE, USGS_Stages$North, type='l', col='black',
     ylab='North Stage (m)', ylim = c(4,6), main=run.title, lty='dashed',
     xlab='Date')
lines(USGS_Stages$DATE, rep(cell$E0[35], length(USGS_Stages$DATE)), 
      col='green')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,35], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,35], col='violet')


# South
plot(USGS_Stages$DATE, USGS_Stages$South, type='l', col='black',
     ylab='South Stage (m)', ylim = c(4,6), lty='dashed', xlab='Date')
lines(USGS_Stages$DATE, rep(cell$E0[38], length(USGS_Stages$DATE)), 
      col='green')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,39], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,39], col='violet')

# 7 & 9
plot(USGS_Stages$DATE, USGS_Stages$GS7, type='l', col='black',
     ylab='USGS 7 & 9 Stage (m)', ylim = c(4,6), lty='dashed', xlab='Date')
lines(USGS_Stages$DATE, USGS_Stages$GS9, type='l', col='blue', lty='dashed')
lines(USGS_Stages$DATE, rep(cell$E0[37], length(USGS_Stages$DATE)), 
      col='green')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,37], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,37], col='violet')


# 8T
plot(USGS_Stages$DATE, USGS_Stages$GS8T, type='l', col='black',
     ylab='USGS 8T Stage (m)', ylim = c(4,6), lty='dashed', xlab='Date')
lines(USGS_Stages$DATE, rep(cell$E0[29], length(USGS_Stages$DATE)), 
      col='green')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,29], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,37], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,29], col='violet')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,37], col='violet')

# 8C
plot(USGS_Stages$DATE, USGS_Stages$GS8C, type='l', col='black',
     ylab='USGS 8C Stage (m)', ylim = c(4,6), lty='dashed', xlab='Date')
lines(USGS_Stages$DATE, rep(cell$E0[9], length(USGS_Stages$DATE)), 
      col='green')
lines(as.Date((Start.Date-1):Stop.Date), Qcalc.Stage[,9], col='red')
lines(as.Date((Start.Date-1):Stop.Date), Qhist.Stage[,9], col='violet')

# calculate historic and calculated outflow matricies
  # initialize the matricies
  Qout.hist <- matrix(data=NA, nrow=(Stop.Day -Start.Day +1), ncol=ncanal)
  Qout.calc <- Qout.hist
  Qout.hist.sum <- NA
  Qout.calc.sum <- NA
  Qout.calc.reg <- NA
  Qout.hist.reg <- NA
  for (DAY in seq(Start.Day,Stop.Day, 1)) {
    Qout.hist[DAY,] <- QoutHistoric(DAY) # historic outflow
    Qout.calc[DAY,] <- Qout.hist[DAY,]  # calculated outflow
    A1 <- A1Floor(DAY) # regulation schedule A1 stage used by QCalcOutS10
    # calculate S10 regulatory outflow
    S10Stage  <- Qcalc.Stage[DAY,9] # Stage[9] # calculate using stage at 1-8C
    QoutCalcReg  <- QoutCalcCell(S10Stage) # vector of outflows on this DAY
    Qout.calc[DAY,5] <- (QoutCalcReg[5] + NonReg$S10D[DAY]) # hurricane deviation
    Qout.calc[DAY,6] <- (QoutCalcReg[6] + NonReg$S10C[DAY]) # hurricane deviation
    Qout.calc[DAY,7] <- (QoutCalcReg[7] + NonReg$S10A[DAY]) # hurricane deviation
    Qout.calc[DAY,8] <- (QoutCalcReg[8] + NonReg$S39[DAY])  # S39 water supply
    Qout.hist.sum[DAY] <- sum(Qout.hist[DAY,])
    Qout.calc.sum[DAY] <- sum(Qout.calc[DAY,])
    # save total regulatory outflows, S10s + S39
    Qout.calc.reg[DAY] <- sum(QoutCalcReg[5:8])
    Qout.hist.reg[DAY] <- sum(Qout.hist[DAY,5:8]) -
      (NonReg$S10D[DAY]+NonReg$S10C[DAY]+NonReg$S10A[DAY]+NonReg$S39[DAY])
  } # end of DAY loop

# plot total Qout for hist & calc
  plot(USGS_Stages$DATE,Qout.hist.sum, type='l', col='black',
       ylab='Total outflow (m3/day)', xlab='Date')
  lines(USGS_Stages$DATE, Qout.calc.sum, col='red')
  plot(Qout.hist.sum, Qout.calc.sum, col='black',
       main='Total outflow (m3/day)', xlab='Historic', ylab='Calculated')
  # plot only total regulatory S10 outflows
  plot(Qout.hist.reg, Qout.calc.reg, col='black',
       main='Regulatory outflow (m3/day)', xlab='Historic', ylab='Calculated')
  lines(Qout.hist.reg, Qout.hist.reg, col='green', lwd=2)
  # plot only cumulative total regulatory S10 outflows
  plot(cumsum(Qout.hist.reg), cumsum(Qout.calc.reg), col='black', type='l', lwd=2,
       main='Cumulative Regulatory outflow (m3)', xlab='Historic', ylab='Calculated')
  lines(cumsum(Qout.hist.reg), cumsum(Qout.hist.reg), col='green', lwd=2)
  # yday is a function in the lubridate library
  Day.First <- (yday(USGS_Stages$DATE)==1) # logical vector TRUE on 1/1 of year
  points(cumsum(Qout.hist.reg)[Day.First], cumsum(Qout.calc.reg)[Day.First],
         col='red', pch = 3, cex = 4)
  # lines(cumsum(Qout.hist.reg)[Day.First], cumsum(Qout.calc.reg)[Day.First],
  #       col='red')
  
  # plot the outflow formula
  deltaS <- seq(-0.2,0.8,0.01)
  y <- NA
  for (i in 1:length(deltaS)) {
    y[i] <- QCalcOutS10(deltaS[i],0)
  }
  plot(deltaS, y, type = 'l', lwd=3, 
       xlab = '1-8C Stage  -  A1 floor (m)',
       ylab = 'Total Regulatory Outflow (million m^3/day)')
  lines(deltaS,rep(0, length(deltaS)), lwd = 1, col='blue')
   