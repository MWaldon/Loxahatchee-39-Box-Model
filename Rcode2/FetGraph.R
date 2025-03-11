# plot ET reduction factor vs depth for user manual
#   (This also tests the function Fet with a vector argument)

source('39BoxFunctions.R') # compile the Fet function if not already available

FetDepthSeq <- seq(from=-0.1, to=0.4, by=0.01)
plot(FetDepthSeq,Fet(FetDepthSeq), 
     type='l', col='blue', lwd=2, ylab='ET factor', xlab='Depth(m)',
     main =paste('Het = ',Het, '(m), ETmin = ', ETmin, ' (m)'))
rm('FetDepthSeq') # cleanup
