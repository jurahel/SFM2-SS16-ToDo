
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fBasics", "tseries", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
datax = read.csv("datasetsfm2.csv", header = TRUE, stringsAsFactors = FALSE)

# select stock names
pattern = c("...TOT.RETURN.IND", "...XET.", "..XET.", "...ORD..0.50", ".PREF")
for (i in pattern){
  colnames(datax) = gsub(pattern = i, replacement = "", x = colnames(datax), fixed = TRUE)
}
colnames(datax) = gsub(pattern = ".", replacement = " ", x = colnames(datax), fixed = TRUE)

msg1 = "This program calculates the first order auto correlation of returns, 
squared returns and absolute returns and skewness, kurtosis and the Bera Jarque 
statistic for german and british blue chips, 2004 - 2014"
print(msg1)


# computing acf of order 1, skewness, kurtosis and bera jarque statistic
acf_fun = function(tmp) {
  return(cor(tmp[-1], tmp[-length(tmp)]))
}
acf_ret = apply(X = ret, MARGIN = 2, FUN = acf_fun)
acf_squ = apply(X = ret_squ, MARGIN = 2, FUN = acf_fun)
acf_abs = apply(X = ret_abs, MARGIN = 2, FUN = acf_fun)
skew    = apply(X = ret, MARGIN = 2, FUN = skewness)
kurt    = apply(X = ret, MARGIN = 2, FUN = kurtosis)
jb_test = function(tmp) {
  jb = jarque.bera.test(tmp)
  return(jb$statistic)
}
jb      = apply(X = ret, MARGIN = 2, FUN = jb_test)

# Output
msg2 = "first order auto correlation of returns, squared returns and absolute returns and skewness, kurtosis and Bera Jarque statistic for german blue chips, 1974 - 1996"
print(msg2)
print("")
result 
