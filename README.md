#Small Area Estimation: R package of data and demo files for short courses.

This package was used for short courses on Small area. At the 2015 Isi conference and  at the 2016 Aapor conference for the shor course ["Big Data for Small Areas"](http://wapor.org/wp-content/uploads/2016/02/Austin2016.pdf).

##What is in the package.

The installation of the package will also install some packages usefull for SAE (packages `sae`, `R2jags` ...), and the installation of some data packages.

##Installation
execute in R:
```{R}
install.packages("devtools)"
devtools::install_github("DanielBonnery/SaeComputations")
```

##Use
```{R}
library(Aapor2016Sae)
demo(ADM)
demo(EB)
demo(EB2)
demo(MCMC)
```
