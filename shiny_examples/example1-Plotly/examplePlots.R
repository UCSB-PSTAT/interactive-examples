
maxReps<-500

# storing mydata
runMyData <- function(mu=0, n=10, rep=10, sd=1, maxReps=500){
  #mu <- input$mu
  #n <- input$n
  #rep <- input$rep
  #sd <- input$sd
  
  
  # Initial parameters
  #if (length(params)==1){
  #  params$mu <- mu
  #  params$sd <- sd
  #  params$n <- n
  #}
  
  repMeans<-sapply(1:(maxReps),function(i) mean(rnorm(n),mean=mu,sd=sd))
  df<-data.frame(repMeans)
  df$meanAvg <- sapply(1:(maxReps),function(i) mean(df$repMeans[1:i]))
  df$sdAvg <- sapply(1:(maxReps),function(i) sd(df$repMeans[1:i]))
  df$replications<-1:(maxReps)
  yvaluesMean <- c(abs(df$meanAvg), mu - abs(df$meanAvg-mu))
  pretty_MeanAvg <- pretty(yvaluesMean)[abs(pretty(yvaluesMean)- mu )> 0.01]
  yvaluesSDAvg <- c(abs(df$sdAvg), sd/sqrt(n) - abs(df$sdAvg- sd/sqrt(n)))
  
  pretty_SDAvg <- pretty(yvaluesSDAvg)[abs(pretty(yvaluesSDAvg)- sd/sqrt(n) )> 0.01]
  
  list(df=df,
       mu=mu,
       sd=sd,
       n=n,
       rep=rep,
       pretty_MeanAvg=pretty_MeanAvg,
       pretty_SDAvg=pretty_SDAvg)
}


myData<-runMyData(maxReps = maxReps)
input<-list(rep=10)

p1 <- ggplot(myData$df[1:isolate(input$rep),], aes(repMeans)) + 
  geom_histogram(aes(y = stat(density)),
                 color="darkblue", 
                 fill="lightblue",
                 binwidth = 0.05*myData$sd) +
  xlim(c(myData$mu-3*myData$sd,myData$mu+3*myData$sd)) + 
  xlab(expression(bar(X))) + 
  geom_vline(aes(xintercept=myData$mu),color="red", linetype="dashed", size=1) + 
  stat_function(
    fun = dnorm, args = list(mean = myData$mu, sd = myData$sd/sqrt(myData$n)), 
    lwd = 2,col = 'red', alpha=0.3)

# Add a Normal Curve (Thanks to Peter Dalgaard)
input$rep<-100
x <- myData$df[1:input$rep,]$repMeans

# bae graphics
h<-hist(x, col="red", xlab="xlab",main="main",freq=F,
     breaks=seq(floor(min(myData$df$repMeans)),
                ceiling(max(myData$df$repMeans)),by=0.05*myData$sd))
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=myData$mu,sd=myData$sd)
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

# plotly

library(plotly)

input$rep<-500
x <- myData$df[1:input$rep,]$repMeans
xfit<-seq(min(myData$df$repMeans),max(myData$df$repMeans),length=100)
fit <- dnorm(xfit,mean=myData$mu,sd=myData$sd/sqrt(myData$n))

p1<- plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
  add_trace(x = xfit, y = fit, type = "scatter", mode = "lines", fill = "tozerox", yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))
p1

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
p2 <- plot_ly(data, x = ~myData$df[1:isolate(input$rep),]$replications, 
              y = ~myData$df[1:isolate(input$rep),]$meanAvg, 
              type = 'scatter', mode = 'lines') %>%
  add_segments(name ='mu',
               x = 1, 
               xend = isolate(input$rep), 
               y = myData$mu, yend = myData$mu) %>%
  layout(xaxis = list(title = "# of replications",titlefont = f), 
         yaxis = list(title = "average",titlefont = f)
  )