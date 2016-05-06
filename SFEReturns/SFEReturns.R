
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

# computing log returns and its squared and absolute values
x       = as.matrix(datax[, -1])  # extracting all data without the non-numeric date column
ret     = diff(log(x))
ret_squ = ret^2
ret_abs = abs(ret)

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

# resulting Table
tab  = cbind(acf_ret, acf_squ, acf_abs, skew, kurt, jb)
# reordering table by first letter, first German than British with Index above
ord  = c(1, 1 + order(rownames(tab)[2:21]), 22, 22 + order(rownames(tab)[23:42]))  
msg2 = "First order auto correlation of returns, squared returns and absolute returns 
and skewness, kurtosis and Bera Jarque statistic for german and british blue chips, 
2004 - 2014"
print(msg2)
(tab = tab[ord, ])

# Latex output for facts at chapter 11 page 20
acf_max = grep(pattern = max(tab[,"acf_ret"]), x = tab[,"acf_ret"])
acf_min = grep(pattern = min(tab[,"acf_ret"]), x = tab[,"acf_ret"])
print(paste0(rownames(tab)[acf_max]," - highest autocorrelation (",
             round(max(tab[,"acf_ret"]),4),")"))
print(paste0(rownames(tab)[acf_min]," - lowest autocorrelation (",
             round(min(tab[,"acf_ret"]),4),")"))

pos = sum(tab[,"acf_ret"]>=0) # number of stocks with pos. acf
neg = sum(tab[,"acf_ret"]<0)
msg3_1 = ifelse(abs(pos/neg) >= 2, "Majority of", "More")
msg3_2 = ifelse(pos/neg >= 1, "positive", "negative")
paste0(msg3_1," autocorrelations are ", msg3_2," (",pos," pos., ",neg," neg.)")

CI = qnorm((1 + 0.95)/2)/sqrt(dim(ret)[1])
if (sum(tab[,"acf_squ"] > CI) == dim(ret)[2] & sum(tab[,"acf_abs"] > CI) == dim(ret)[2])
  print("Autocorrelation of squared and absolute returns are positive and signifcantly larger than zero")

# Latex output for tables in presentation chapter 11 p. 22-25
roundex = function(x, digits) {
  format(round(x, digits), nsmall = digits)
}
tab2           = cbind(roundex(tab[, 1:3], 4), roundex(tab[, 4:5], 2), roundex(tab[, 6], 1))
# redefining columnames such that they are appropriate for latex
colnames(tab2) = c("$\\hat{\\rho}_1\\left(r_t\\right)$", 
                   "$\\hat{\\rho}_1\\left(r_t^2\\right)$", 
                   "$\\hat{\\rho}_1\\left(|r_t|\\right)$",
                   "$\\hat{S}$", 
                   "$\\widehat{Kurt}$", 
                   "$BJ$")
colnames(tab2) = paste0("\\multicolumn{1}{c}{",colnames(tab2),"}")

latex_output = function(tab) {
  # raw latex table
  tmp      = xtable(tab, align = paste0("l|",paste0(rep("r", dim(tab2)[2]), collapse = "")))
  # adding hline, such that first and last line in table are double lines
  addtorow = list(pos = list(c(-1, dim(tab)[1])), command = "\\hline ")
  # final latex table
  print(tmp, add.to.row = addtorow, sanitize.text.function = function(x) {x})  
}
latex_output(tab2[ 1:21, ])  # latex table for German blue chips
latex_output(tab2[22:42, ])  # latex table for British blue chips 
