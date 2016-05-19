library("dataCountyCropSurveySatellite")
library("INLA")
data(crop)
inla(data=crop)

crop$county=as.factor(crop$county)
attach(crop)

