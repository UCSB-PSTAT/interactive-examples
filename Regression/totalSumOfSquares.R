beta1=2;beta2=1; sigma=10; sampleSize= 30
x=sample(seq(from=1,to=100,by=0.01),size=sampleSize)

# data generation
df<-data.frame(x=x,y=beta1 + beta2*x + rnorm(n=length(x),mean=0,sd=sigma))
fit<- lm(y ~ x, data=df)
df$yhat <- fitted(fit)
TotalSS = sum((df$yhat- df$y)^2)
slope = fit$coefficients[2]

TotalSS = sum((beta1 + -1*df$x- df$y)^2)


slopes = seq(from=-1.0,to=2.0, by=0.02)
SST=sapply(slopes, function(b){
  sum((beta1 + b*df$x- df$y)^2)
  })

plot(x=slopes,y=SST)