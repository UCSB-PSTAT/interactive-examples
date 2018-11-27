# One sided-figure (H_0: mu<= mu_0 vs H_A:mu> mu_0) 
library(plotly)
set.seed(1)
mu_0=0
sigma=1 #Fixed
n=10 #Fixed
maxReps=1000
alpha = 0.05
x<-rnorm(n=maxReps,mean=mu_0,sd=sd)
xbar =2.3#mean(x)
z<-seq(-3.4,3.4,length=100)
# Step 1: Density for Null hypothesis: mu=0
x_H0<-z*sigma + mu_0
dnorm_H0<- dnorm(x_H0,mean=mu_0,sd=sigma)
xMin=min(x_H0);xMax=max(x_H0)
p <- plot_ly(alpha = 0.6) %>% 
  add_trace(x = x_H0, y = dnorm_H0, 
            type = "scatter", 
            mode = "lines", 
            line = list(color = 'rgba(34, 145, 232, 0.15)'),
            fillcolor = 'rgba(34, 145, 232, 0.15)',
            fill = "tozeroy", 
            #yaxis = "y2", 
            yaxis = list(title="y",range=c(min(dnorm_H0),max(dnorm_H0))),
            name = "Density") %>%
  layout(xaxis=list(title='X',range = c(xMin, xMax)),
        showlegend = F
      ) 

p

# Step 2: Rejection Region
x_alpha= qnorm(1-alpha/2,mean=mu_0,sd=sigma)
xLeft<-x_H0[x_H0<=x_alpha]
xRight<-x_H0[x_H0>x_alpha]
dnormLeft<-dnorm(xLeft,mean=mu_0,sd=sigma)
dnormRight<-dnorm(xRight,mean=mu_0,sd=sigma)
p <- plot_ly(alpha = 0.6) %>% 
  # Left
  add_trace(x = xLeft, y = dnormLeft, 
            type = "scatter", 
            mode = "lines", 
            line = list(color = 'rgba(34, 145, 232, 0.15)'),
            fillcolor = 'rgba(34, 145, 232, 0.15)',
            fill = "tozeroy", 
            name = "Density") %>%
  # Right
  add_trace(x = xRight, y = dnormRight, 
            type = "scatter", 
            mode = "lines", 
            line = list(color = 'rgba(229, 175, 32, 0.55)'),
            fillcolor = 'rgba(229, 175, 32, 0.55)',
            fill = "tozeroy", 
            name = "Density") %>%
  layout(xaxis=list(title='X',range = c(xMin, xMax)),
         showlegend = F
  ) 

p

# Step 3: Observed Statistic: sample mean
colorBar =  if (xbar<=x_alpha) 'rgba(34, 145, 232, 1)' else 'rgba(229, 175, 32,1)'
p = p %>%   
  add_segments(name ='Statistic', mode='lines',
               line = list(color = colorBar,dash = "dash"),
               yaxis = "y",
               x = xbar,xend=xbar,
               y = 0.001,yend=max(c(dnormLeft,dnormRight)))
p

sprint(cat('jola', 1234))

