mu <- 0
n <- 1000
rep <- 10000

# Repetitions
repMeans<-sapply(1:rep,function(i) mean(rnorm(n,mean=mu,sd=1)))

#Average of repetitions 
step= round(0.05 *rep)
subRep = seq(from=step,to=rep,by=step)
meanAvg <- sapply(1:rep,function(i) mean(repMeans[1:i]))
meanAvg[subRep]




# Histogram of sample means
df<-data.frame(repMeans)
p<-ggplot(df, aes(x=repMeans)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mu),
             color="red", linetype="dashed", size=1)
  
p


# Evolution of Avg of replications
df<-data.frame(list(subRep,meanAvg[subRep]))
colnames(df)<-c('subRep',"meanAvg")
p<-ggplot(df, aes(x=1:dim(df)[1],y=meanAvg)) + 
  geom_line()+ylim(-1*max(abs(df)),max(abs(df)))+
  geom_hline(aes(yintercept=mu),
             color="red", linetype="dashed", size=1)

p



sapply(1:rep,function(i) mean(rnorm(n,mean=mu,sd=1)))