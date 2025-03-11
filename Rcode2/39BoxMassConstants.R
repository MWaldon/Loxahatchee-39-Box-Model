# This R script sets model run constants needed for the 39-Box Mass/Concentration model

# Constituent related constants
evap     = 0.65       # fraction of ET that is evaporation
# water column constituent index values
nconstit   = 5 # number of constituents with simulated mass/concentration 
cl         = 1 # chloride; conservative
so4        = 2 # sulfate; apparent settling
tp         = 3 # total phosphorus, k-c* model
so4eco     = 4 # sulfate; monod relationship; ecolab
dmsta_tp   = 5 # total phosphorus modeled with DMSTA equations
cnames     = c('cl', 'so4', 'tp', 'so4eco', 'dmsta_tp')

# aerial deposition
Precip <- rep(NA,nconstit) # concentration in rainfall (mg/L)
Precip[cl]      = 2.0
Precip[so4]     = 1.0
Precip[so4eco]  = 1.0
Precip[tp]      = 0.010
Precip[dmsta_tp]= 0.010
DD <- rep(NA,nconstit)     # dry deposition (mg/m^2-day)
DD[cl]       = 1136  # (mg/m^2-year)
DD[so4]      = 138.2 # (mg/m^2-year)
DD[so4eco]   = 138.2 # (mg/m^2-year)
DD[tp]       = 40    # (mg/m^2-year)
DD[dmsta_tp] = 40    # (mg/m^2-year)
DD <- DD/365.25 # convert annual values to daily values (mg/m^2-day)

# reaction parameters
# SO4
#   kso4 values 
#   so4 dissapearance rate is cell$kso4 imported by make.datasets.R
#   values are by subtype C=canal, P=perimeter, T=transition, I=interior
#   kso4 subtype C=0, P=0.00137, T=0.00274, I=0.02738 (m/day)

# SO4 eco
khalfSO4      = 0.650927   # (g/m^3)
MaxSO4Removal = 0.074948   # (g/ m^2-d)

#  TP
ktp           = 16.8/365.25  # settling rate (m/day)
cstarm = 0.008               # c* in marsh (mg/L or gm/m3)
cstarc  = 0.080              # c* in canal (mg/L or gm/m3)

#  dmsta_tp
# for details: http://wwwalker.net/dmsta/index.htm
# DMSTA uses two wetland classes for K parameters: 
#   EMG: emergent marsh; or PEW: preexisting wetland
#     EMG: k1=0.1064; k2=0.002; k3=0.3192
#     PEW: k1 = 0.221,k2=0.0042,k3=0.6631
# EMG: please change TP initial below
K1 = (0.106/365.25)*1000    # maximum uptake rate (m3/g-day)
K2 = (0.002/365.25)*1000    # recycle rate (m2/g-day) 
K3 = 0.3191/365.25          # burial rate (1/day)

# PEW: please change TP initial below   
# K1 = (0.221/365.25)*1000    # maximum uptake rate (m3/g-day)
# K2 = (0.0042/365.25)*1000   # recycle rate (m2/g-day)
# K3 = 0.6631/365.25          # burial rate (1/day)
