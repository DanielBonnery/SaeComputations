#1. Load the data package and the data
data(baseball,package="dataBaseball")
attach(baseball)
#2. Estimate via ADM method
psi=hat.P*(1-hat.P)/nrow(baseball)
library("Rgbp")
g<-gbp(hat.P,sqrt(psi),x1,model="gaussian")
#3. Use estimated values to  plot posterior distribution
#3.1 Print the summary
summary(g)
#3.2 Obtain Shrinkage plot
#plot(g)
#3.3 Posterior densities 
#Computation of the posterior distribution of A parameters
#we load this library to get trigammaInverse and digamma functions 
library("TANOVA")
alpha=trigammaInverse(g$a.var)
beta=exp(digamma(alpha)-g$a.new)
A.mean<-alpha/beta
A.var<-alpha/(beta^2)
A.sd<-sqrt(A.var)
A.summary<-c(Mean=A.mean,sd=A.sd, qgamma(c(.5,.025,.975),shape=alpha,rate=beta))
names(A.summary)[3:5]<-c("Median","0.025 Qu.","0.975 Qu.")
#Definition of the x sequences for the plots 
x=seq(-1,1,length.out=200)
X1<-g$beta.new[2,1]+x*3*sqrt(g$beta.var[2,2])
X2<-seq(qgamma(.001,alpha,rate=beta),qgamma(.999,alpha,rate=beta),length.out=200)
X3<-g$post.mean[1,1]+x*3*g$post.sd[1,1]
#plot posterior densityes
library(ggplot2)
plot1<-ggplot(data=data.frame(x=X1,y=dnorm(X1,mean=g$beta.new[2,1],sd=sqrt(g$beta.var[2,2]))),
              aes(x=x,y=y))+ geom_line(colour="blue",size = 1)+ labs(x = "Posterior density of $\\beta_1$",y='Density')
print(plot1)
par(mfrow=c(3,1))
plot(X1,dnorm(X1,mean=g$beta.new[2,1],sd=sqrt(g$beta.var[2,2])),xlab='Posterior density of beta1',ylab='density',type='l',col="black",lwd=3)
plot(X2,dgamma(X2,shape=alpha,rate=beta),xlab='Posterior density of A',ylab='density',type='l',col="black",lwd=3)
abline(v=A.mean,lwd=3,col='gray')
abline(v=A.summary['Median'],lwd=3,col='darkgray')
plot(X3,dnorm(X3,mean=g$post.mean[1,1],sd=g$post.sd[1,1]),xlab='Posterior density of $theta_i$ for Clemente.',ylab='density',type='l',col="black",lwd=3)

table1<-data.frame(Parameter=c("$A$","$\\beta_1$","Clemente"),
                   rbind(A.summary[],
                         c(sqrt(g$beta.var[2,2]),qnorm(c(.5,.5,.025,.975),g$beta.new[2,1],sqrt(g$beta.var[2,2])))[c(2,1,3:5)],
                         signif(c(g$post.mean[1],g$post.sd[1],g$post.mean[1],g$post.intv.low[1],g$post.intv.upp[1]),3)))
