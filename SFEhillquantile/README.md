
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEhillquantile** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : SFEhillquantile

Published in : Statistics of Financial Markets

Description : 'Hill quantile estimation based on random numbers drawn from standardized Pareto
distribution.'

Keywords : 'VaR, distribution, estimation, extreme-value, forecast, generalized-pareto-model,
hill-estimator, pareto, quantile, risk, standard'

Author : Joanna Tomanek, Sophie Burgard

```


### R Code:
```r
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fExtremes")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Main computation
x = rgpd(1000, xi=1, mu=0)  # gen. random variables 
x = sort(x, decreasing = TRUE)
n = length(x)
q = 0.99
k = length(x)*0.1


rest  = gpdFit(x, nextremes = k, type = "mle")	# ML-estimation of alpha_H 
gamma = (as.numeric(rest@parameter$u))^-1     

# the Hill-quantile value
xest = x[k] + x[k] * (((n/k) * (1 - q))^(-gamma)-1)	# Hill-quantil estimation

#Output
paste0("Hill ",q, "-quantile estimator: ",round(xest,4))


```
