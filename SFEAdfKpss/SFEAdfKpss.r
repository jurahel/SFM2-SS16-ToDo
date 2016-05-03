SFEAdfKpss<-function(){
# ---------------------------------------------------------------------
# EBook       SFE
# ---------------------------------------------------------------------
# See_also    adf, kpss, kpssnum
# ---------------------------------------------------------------------
# Macro       SFEAdfKpss
# ---------------------------------------------------------------------
# Description computes the ADF and KPSS test 
#             statistics for german blue chips, 1974 - 1996      
# ---------------------------------------------------------------------
# Usage       SFEAdfKpss()
# ---------------------------------------------------------------------
# Input
#       Parameter   d
#       Definition  Display, used to show messages and results
#       Parameter   mess
#       Definition  String, this message is shown on top of the display
# --------------------------------------------------------------------- 
# Author        R: Joanna Tomanek, 408759
#                 XploRe:    Kleinow 20000411
# ---------------------------------------------------------------------
#Packages	{tseries} {fSeries} {urca}
# ---------------------------------------------------------------------

datax<-read.table("sfm/sfm_pri.txt", header=FALSE)                   
  
stocks = as.matrix(c("all", "allianz", "basf", "bayer", "bmw", "cobank", "daimler", "deutsche bank", "degussa","dresdner bank", "hoechst", "karstadt", "linde", "man", "mannesmann", "preussag", "rwe", "schering", "siemens", "thyssen", "volkswagen"))

s=menu(stocks, graphics = TRUE, title = "Select one")

x=(datax[,s])
st<-stocks[s]

if(s==1)
{
x=(datax[,2:ncol(datax)])
st<-c(stocks[2:21])
}

x=log(x)
x<-as.matrix(x)

#result=matrix(1,ncol=16,nrow=ncol(x),dimnames = list(st,c("ADF.constant.0","p-val","ADF.constant.4","p-val", "ADF.trend.0","p-val","ADF.trend.4","p-val" ,"KPSS.constant.8","p-val", "KPSS.constant.12","p-val","KPSS.trend.8","p-val","KPSS.trend.12", "p-val"))) 

i=1

for(i in 1:ncol(x))		# start calc. table
{     
#******   ADF   **************************
e=adfTest(x[,i],lags = 0, type ="c")
print(e)
e=adfTest(x[,i],lags = 4, type ="c")
print(e)
e=adfTest(x[,i],lags = 0, type ="ct")
print(e)
e=adfTest(x[,i],lags = 4,type ="ct")
print(e)


#****   KPSS   ***************************

e=urkpssTest(x[,i], type = "mu",use.lag = 8, doplot = FALSE)
print(e)
e=urkpssTest(x[,i], type = "tau",use.lag = 12, doplot = FALSE)
print(e)
e=urkpssTest(x[,i], type = "mu",use.lag = 8, doplot = FALSE)
print(e)
e=urkpssTest(x[,i], type = "tau",use.lag = 12, doplot = FALSE)
print(e)
}
#result
}
