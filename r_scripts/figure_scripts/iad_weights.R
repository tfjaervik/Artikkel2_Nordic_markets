library(tikzDevice)
library(fs)
library(tidyverse)

x = seq(0, 1, by = 0.01)
x = x[-c(1, 101)]
y = 1-x

fig <- qplot(x,1/(x*(1-x)), xlab = "x", ylab = "f(x)")


#===============================================================================
#' ## Save figure
#===============================================================================

#' Run entire block in one go

tikzDevice::tikz(file = path("_figures/IAD_weights", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
fig
dev.off()