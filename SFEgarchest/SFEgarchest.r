# ---------------------------------------------------------------------
# Book:         SFE3
# ---------------------------------------------------------------------
# Quantlet:     SFEgarchest
# ---------------------------------------------------------------------
# Description:  SFEgarchest reads the date, DAX index values, stock
#               prices of 20 largest companies at Frankfurt Stock
#               Exchange (FSE), FTSE 100 index values and stock prices
#               of 20 largest companies at London Stock Exchange (LSE)
#               and estimates various GARCH models for the DAX and
#               FTSE 100 daily return procesess from 1998 to 2007
# ---------------------------------------------------------------------
# Usage:        -
# ---------------------------------------------------------------------
# Inputs:       none
# ---------------------------------------------------------------------
# Output:       P - matrix of estimated coefficients
#               T - matrix of t-statistics of estimated coefficients
# ---------------------------------------------------------------------
# Example:      
# ---------------------------------------------------------------------
# Author:       Matlab: Andrija Mihoci 20091019
#               R: Awdesch Melzer 20121008
# ---------------------------------------------------------------------

# close windows, clear history
rm(list=ls(all=TRUE))
graphics.off()

install.packages("tseries")
library(tseries)
install.packages("fGarch")
library(fGarch)
install.packages("rugarch")
library(rugarch)


# Read data for FSE and LSE
DS = read.table("FSE_LSE.dat");
D = DS[,1]                            # date
S = DS[,2:43]                         # S(t)
s = log(S)
end = length(D)                       # log(S(t))
r = s[2:end,] - s[1:(end-1),]         # r(t)
n = length(r)                         # sample size
t = c(1:n)                            # time index, t

# Parameter estimation of various GARCH models

# (1) AR(1)-GARCH(1,1)

#DAX.AR1GARCH11 = garchFit(~ arma(1,0) + garch(1,1), r[,1], trace=T) 
DAX.AR1GARCH11 = garchFit(~ arma(1,0) + garch(1,1), r[,1], trace=F) 
#FTSE.AR1GARCH11 = garchFit(~ arma(1,0) + garch(1,1), r[,22], trace=T) 
FTSE.AR1GARCH11 = garchFit(~ arma(1,0) + garch(1,1), r[,22], trace=F) 

# (2) AR(1)-TGARCH(1,1)

DAX.AR1TGARCH11 = garchFit(~arma(1,0)+aparch(1,1), data = r[,1], delta = 2, include.delta = FALSE,leverage=TRUE)
FTSE.AR1TGARCH11 = garchFit(~arma(1,0)+aparch(1,1), data = r[,22], delta = 2, include.delta = FALSE,leverage=TRUE) 

# (3) AR(1)-EGARCH(1,1)

ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = TRUE), distribution.model = "std")
DAX.AR1EGARCH11 = ugarchfit(data = r[,1], spec = spec, solver = "solnp", solver.control = ctrl)
FTSE.AR1EGARCH11 = ugarchfit(data = r[,22], spec = spec, solver = "solnp", solver.control = ctrl)

# Summary of parameter estimates (P), standard errors (E), t-statistics (T) and p-values (Pvalues)

P = matrix(0,7,6)
P[,1]=c(DAX.AR1GARCH11@fit$matcoef[,1],0,0)
P[,2]=c(FTSE.AR1GARCH11@fit$matcoef[,1],0,0)
P[,3]=c(DAX.AR1TGARCH11@fit$matcoef[,1],0)
P[,4]=c(FTSE.AR1TGARCH11@fit$matcoef[,1],0)
P[,5]=c(DAX.AR1EGARCH11@fit$matcoef[,1])
P[,6]=c(FTSE.AR1EGARCH11@fit$matcoef[,1])

E = matrix(0,7,6)
E[,1]=c(DAX.AR1GARCH11@fit$matcoef[,2],0,0)
E[,2]=c(FTSE.AR1GARCH11@fit$matcoef[,2],0,0)
E[,3]=c(DAX.AR1TGARCH11@fit$matcoef[,2],0)
E[,4]=c(FTSE.AR1TGARCH11@fit$matcoef[,2],0)
E[,5]=c(DAX.AR1EGARCH11@fit$matcoef[,2])
E[,6]=c(FTSE.AR1EGARCH11@fit$matcoef[,2])

T = P/E

Pvalues = matrix(0,7,6)
Pvalues[,1]=c(DAX.AR1GARCH11@fit$matcoef[,4],0,0)
Pvalues[,2]=c(FTSE.AR1GARCH11@fit$matcoef[,4],0,0)
Pvalues[,3]=c(DAX.AR1TGARCH11@fit$matcoef[,4],0)
Pvalues[,4]=c(FTSE.AR1TGARCH11@fit$matcoef[,4],0)
Pvalues[,5]=c(DAX.AR1EGARCH11@fit$matcoef[,4])
Pvalues[,6]=c(FTSE.AR1EGARCH11@fit$matcoef[,4])


P
E
T
Pvalues