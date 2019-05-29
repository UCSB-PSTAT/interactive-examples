get_dfGrid<-function(x){
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
  
  return(df)
}

# Example
if(F){
library(plot3D)
library(Stat2Data)
data(Pines)
id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
id<- sample(id, size=length(id),replace = F)
x<-Pines$Hgt96[id]
dfGrid<-get_dfGrid(x)
}
