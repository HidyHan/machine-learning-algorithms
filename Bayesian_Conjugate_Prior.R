require("ggplot2")
require("reshape2")
set.seed(273)
samples<-rexp(256,1)
alpha_current<-2
beta_current<-0.2
theta<-seq(0,4,0.001)
q<-function(alpha,beta,theta){
  loglikelihood<-log(theta)*(alpha-1)+alpha*log(beta)-beta*theta-lgamma(alpha)
  return(exp(loglikelihood))
}

df<-data.frame("theta"=theta)

for (i in 1:256){
  ni<-samples[i]
  alpha_current<-alpha_current+1
  beta_current<-beta_current+ni
  if (is.element(i,c(4,8,16,256))){
    values<-q(alpha_current,beta_current,theta)
    df<-cbind(df,values)
  }
}

names(df)<-c("theta","n=4","n=8","n=16","n=256")
dfm<-melt(df,id.vars="theta")

#As n increases, the mean of posterior converges to the mean of
#the exponential likelihood (lambda), which is one. At the same time, the variance of theta decreases.
#The peak is its mean, alpha/beta, or (n+alpha_zero)/(beta_zero+sum(xi)).
ggplot(dfm, aes(x=theta,y=value,colour=variable))+geom_line()
ggsave("posterior.png")
