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

# get_dataCI: 
get_dataCI<-function(sample_size, seedValue=0){
  require(Stat2Data)
  set.seed(seedValue)
  
  data(Pines)
  id<-which(Pines$Spacing==10 & !is.na(Pines$Hgt96))
  id<- sample(id, size=length(id),replace = F)
  pines.height<-Pines$Hgt96[id]
  
  id_shuffle<-sample(1:length(pines.height))
  sampleValues<-lapply(1:floor(length(pines.height)/sample_size), function(i){
    idRange=c((i-1)*sample_size+1,i*sample_size)
    idRange[2]= ifelse(sample_size>length(pines.height)-idRange[2],length(pines.height),idRange[2])
    return(idRange)
  })
  
  
  xbarPop<-mean(pines.height)
  xbar<-sapply(sampleValues,function(y){ 
    values = pines.height[y[1]:y[2]]
    mean(values)
  })
  
  margin.error<-sapply(sampleValues,function(y){ 
    values = pines.height[y[1]:y[2]]
    s = sd(values)
    #margin.error = qt(confidenceLevel,df=sample_size-1) * s /sqrt(sample_size)
    margin.error = 1 * s /sqrt(sample_size)
  })
  
  df<-data.frame(id=1:length(xbar),xbar=xbar, margin.error=margin.error)
  
  mu = mean(pines.height)
  regionColors <-c('green','red')
  df$Color<-ifelse(mu>=df$xbar-df$margin.error & mu<= df$xbar+df$margin.error, regionColors[1],regionColors[2])
  df$sample.size= sample_size
  return(df)
}

# Ex: df= get_dataCI(15)
