# Simulate constituent mass and concentration
# ***** This script is under construction *****



# Processes - rates of uptake, release, burial (g/day)
Uptake  <- matrix(data=0, nrow=nconstit, ncol=ncell)
Release <- matrix(data=0, nrow=nconstit, ncol=ncell)
Burial  <- matrix(data=0, nrow=nconstit, ncol=ncell)


# historic inflow loads by structure (g/day)
StructLoadin <- function(DAY){ 
  ls <- length(StructIn[DAY,])
  Cl_Load <- StructIn[DAY,2:ls]*StructCl[DAY,2:ls]
  TP_Load <- StructIn[DAY,2:ls]*StructTP[DAY,2:ls]
  SO4_Load <- StructIn[DAY,2:ls]*StructSO4[DAY,2:ls]
  L <- data.frame(t(Cl_Load), t(TP_Load), t(SO4_Load))
  names(L) = c('Cl', 'TP', 'SO4')
  return(L)
} # end StructLoadin

# historic inflow loads to canal cells (g/day)
Loadin <- function(DAY) {
  # canal inflow loads by constituent and cell on DAY
  L <- matrix(data=0, nrow=3, ncol=nc) # rows are Cl, TP, SO4
  LStr <- data.frame(t(StructLoadin(DAY)))
  #L[,1] 	= 0  # no inflows 
  L[,2]  	= LStr$G301   # G301_in
  L[,3] 	  = LStr$G310 + 
    LStr$G251   # G310_in + G251_in
  L[,4]  	= LStr$S6 + 
    LStr$G338   # S6_in + G338_in
  #L[,5] 	= 0
  #L[,6] 	= 0
  #L[,7] 	= 0   
  L[,8]  	= LStr$G94A   # G94A_in 
  L[,9]  	= LStr$G94C   # G94C_in
  L[,10] 	= LStr$ACME1 + 
    LStr$G94D +   # G94D == ACME2
    LStr$S362   # G94D_in + ACME1_in + S362_in 
  L[,11] 	= LStr$G300 + 
    LStr$S5AS + 
    LStr$S5A   # G300_in + S5AS_in + S5A_in
  return(L) # returns matrix (3,ncanal) with Cl, TP, & SO4 loads
} # end Loadin
