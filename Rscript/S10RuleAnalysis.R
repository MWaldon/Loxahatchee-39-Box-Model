# Historic S-10 discharge analysis
# This script compares historic S-10 outflows with the piecewise linear
# function used in the Berkeley Madonna models and the polynomial approximation
# used in the current R implementation.

F <- Date2Day('1995-01-01') # first day of data
L <- Date2Day('2009-06-30') #  last day of data
D <- F:L # a series of days

# Historic flows - sum outflow from S10 gates
# uses dataframes loaded in make.datasets.R
S10Q <- Outflow$S10A+Outflow$S10C+Outflow$S10D+Outflow$S10E # (m^3/day)
S10Q <- S10Q[D] # keep only values within the data range
S10Q <- S10Q/1000000 # adjust scale (10^6 m^3/day)
plot(S10Q, type='l',  ylab='S10 Total (10^6 m^3/day)')
plot(USGS_Stages$GS8C, type='l',  # regulation schedule reference gage
     ylab='Stage 1-8C (m)')
plot(USGS_Stages$GS8C,S10Q, xlim=c(4.5,5.7),
     xlab='Stage 1-8C (m)',ylab='S10 Total (10^6 m^3/day)')

A1all <- A1Floor(D) # array of A1 floor stages (m)
plot(A1all, type='l', ylab='A1 (m)')
dev <- USGS_Stages$GS8C - A1all # deviation around the A1 floor stage (m)
plot(dev, type='l', ylab='Stage-A1 (m)')

# Qx <- 1000*(((2*0.2457)+0.2629)/((3*0.2457)+0.2629))*(196.7/144.8)
NotNA_8C <- !(is.na(USGS_Stages$GS8C[D])) # 8C stages that are not NA
Qc <- QCalcOutS10(USGS_Stages$GS8C[NotNA_8C],A1all[NotNA_8C]) # *Qx no longer used
# Qc <- Qc/1000000 # QCalcOut returns value in 10^6 m^3/day
plot(Qc, type='l', col='red', ylab='S10 Discharge (10^6 m^3/day)')
lines(S10Q[NotNA_8C], coll='black')

for (dd in 0:5) {
  plot(dev[F:(L-dd)],S10Q[(F+dd):L], xlim=c(-0.4,0.4),
       xlab='Stage Deviation (m)',ylab='S10 Total (10^6 m^3/day)',
       main=paste('delay=', dd))
  points(dev[F:L], Qc[F:L], col='red')
}

# piecewise linear regulatory release function from Berkeley Madonna
x <- c(-1.3,-1.2,-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
y <- c(0,32,113,64,94,148,311,238,175,328,288,450,765,957,1323,2274,3188,2832,4469,5901,7406,6602,6356,8116)
x <- x*0.3048 # convert ft to m
y <- y*Qx/1000000 # (10^6 m^3/day)
plot(x,y, type = 'b', col='black')
ymod <- lm(y ~ x + I(x^2))
yfit <- ymod$coefficients[1] + (ymod$coefficients[2]*x) + (ymod$coefficients[3]*x^2)
lines(x, yfit, col='green')
lines(x,predict(ymod), col='blue')
x21 <- x[11:21]
y21 <- y[11:21]
ymod21 <- lm(y21 ~ x21 + I(x21^2))
yfit21 <- ymod21$coefficients[1] + (ymod21$coefficients[2]*x21) + (ymod21$coefficients[3]*x21^2)
lines(x21, yfit21, col='green')
lines(x21,predict(ymod21), col='red')
  # note: the ymod21 model was selected for use in function QCalcOutS10
plot(USGS_Stages$GS8C[NotNA_8C],
     QCalcOutS10(USGS_Stages$GS8C[NotNA_8C],A1all[NotNA_8C]),
     col='red')

plot(Qc, type='l', col='red', ylab='S10 Discharge (10^6 m^3/day)')
lines(S10Q[NotNA_8C], col='black')
