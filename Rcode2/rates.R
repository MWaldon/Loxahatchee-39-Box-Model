rates <- function() {
  
  # Water volume balance
  
  
  
  
  # Constituent mass balances
  
  
  # Depth Multiplier **unitless
  #Fz[(nc+1)..ncell] = GRAPH(depth[i]) (0,0) (0.4,1) (1,1) (2,0.2)
  
  # Concentration Multiplier **unitless
  # Fc[dmsta_tp, (nc+1)..ncell] = 0.3/(conc[i, j]+0.3)
  ;Uptake - Uptake of Phosphorus 
  Uptake[dmsta_tp, (nc+1)..ncell] = Fc[i, j]*Fz[j]*K1*dmsta_store[i, j]*conc[i, j]*area[j]   ; g/day for all cell area, not per unit as in DMSTA diagram
  
  ;Release - Release (Recycle) of Phosphorus
  Release[dmsta_tp, (nc+1)..ncell] = K2*(dmsta_store[i, j]^2)*area[j]                            ; g/day                
  
  ;Burial - Burial of Phosphorus into the sediment
  Burial[dmsta_tp, (nc+1)..ncell] = K3*dmsta_store[i, j]*area[j]                                      ; g/day
  
  ;UptakePrM2 - Uptake of Phosphorus per sq.meter
  UpPrM2[dmsta_tp, (nc+1)..ncell] = Uptake[i, j]/area[j]                                               ; g/m2/day
  
  ;ReleasePrM3 - Release (Recycle) of Phosphorus per cubic meter
  RelPrM3[dmsta_tp, (nc+1)..ncell] = Release[i, j]/vol[j]                                               ; g/m3/day
  
  ;init dmsta_store[dmsta_tp, (nc+1)..ncell] =  0.1                                                        ;g/m2/day
  init dmsta_store[dmsta_tp, (nc+1)..ncell] =  #dmstastoreemg(starttime, j)                   ; new initial TP storage imported from Mike Flood for jan 1st 2004 Hamid 22nd Oct 2011
    ;init dmsta_store[dmsta_tp, (nc+1)..ncell] =  #dmstastorepew(starttime, j) 
    
    ;Biomass Storage differential equation (Storage is per unit area: g/m2 in DMSTA model)
  d/dt(dmsta_store[dmsta_tp, (nc+1)..ncell]) = upPrM2[i, j] - ((Release[i, j] + Burial[i, j])/area[j])        ;g/m2/day
  
  # inflow loading rates = (g/m^3)*(m^3/d) = (g/d)
  
  
} # end rates function