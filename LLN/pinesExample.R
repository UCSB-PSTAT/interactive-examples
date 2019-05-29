# Data from pine seedlings planted in 1990  
# source: https://vincentarelbundock.github.io/Rdatasets/doc/Stat2Data/Pines.html
library(Stat2Data)
data(Pines)

id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
id<- sample(id, size=length(id),replace = F)
x<-Pines$Hgt96[id]
xMean<-c()
xSD<-c()
for(i in 1:length(x)){
  xMean<-c(xMean, mean(x[1:i]))
}

for(i in 2:length(x)){
  xSD<-c(xSD, sd(x[1:i]))
}


interval=1:500
plot(x=interval,y=xMean[interval], ylim=c(min(x),max(x)))

plot(x=interval,y=xSD[interval])
     
     , ylim=c(min(x),max(x)))

# scatterplot: representing trees plantation
library(plot3D)
library(Stat2Data)
data(Pines)
id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
id.shuffle<- sample(id, size=length(id),replace = F)
x<-Pines$Hgt96[id.shuffle]
totalPoints<-length(x)
width_floor=floor(sqrt(totalPoints))
width_ceiling =ceiling(sqrt(totalPoints))
x.Grid<-rep(seq(from=0, to=1, length.out=width_ceiling), times=width_ceiling)
y.Grid<-rep(seq(from=0, to=1, length.out=width_ceiling), each=width_ceiling)
df<-data.frame(x=x.Grid,
               y= y.Grid,
               color=as.character(rbinom(n=width_ceiling^2, size=1, prob=0.5)))
if (width_ceiling-width_floor>0){
  df<-df[head(1:totalPoints,totalPoints-width_ceiling^2),]
}
# plot
ax <- list(title = '', zeroline = FALSE, showline = F,
           showticklabels = FALSE,showgrid = FALSE)
plot_ly(data = df, x = ~x, y = ~y, 
        symbol=~color,symbols=c('circle','o'),
        color = I('green')) %>%
  layout(title = '',xaxis = ax, yaxis = ax)%>% 
  style(hoverinfo = 'none')%>% config(displayModeBar = F)







