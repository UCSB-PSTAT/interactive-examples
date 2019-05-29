


getDirectories<-function(path,full.names=F){
  # this function returns list of subfolders with their names
  names<-list.files(path,rec=F,full.names = full.names)
  full.names <- list.files(path,rec=F,full.names = T)
  folders<-names[file.info(full.names)$isdir]
  return(folders)
}

getData2<-function(){
  library(Stat2Data)
  out<-list(list(),list(),list())
  
  # [[1]] "Newborn Weights"
  out[[1]]$title<-"Newborn Weights"
  out[[1]]$xaxis<-"Newborn Weights (ounces)"
  data(NCbirths)
  out[[1]]$x<-NCbirths$BirthWeightOz[!is.na(NCbirths$BirthWeightOz)]
  out[[1]]$muHat<-mean(out[[1]]$x)
  out[[1]]$sigmaHat<-sd(out[[1]]$x)
  out[[1]]$breaks<-NULL
  
  
  # [[2]] "Pine Heights"
  out[[2]]$title<-"Pine Heights"
  out[[2]]$xaxis<-"Pine Heights (cms)"
  data(Pines)
  out[[2]]$x<-Pines$Hgt97[!is.na(Pines$Hgt97)]
  out[[2]]$muHat<-mean(out[[2]]$x)
  out[[2]]$sigmaHat<-sd(out[[2]]$x)
  out[[2]]$breaks<-NULL
  
  # [[3]] "SAT Scores"
  out[[3]]$title<-"SAT Scores"
  out[[3]]$xaxis<-"Math SAT Scores (points)"
  data(MathPlacement)
  out[[3]]$x<-MathPlacement$SATM[!is.na(MathPlacement$SATM)]
  out[[3]]$muHat<-mean(out[[3]]$x)
  out[[3]]$sigmaHat<-sd(out[[3]]$x)
  out[[3]]$breaks<-NULL
  
  return(out)
  
}

getData<-function(path){
# Values: a list where each entry contains: 
  # dataframe df with observations in column 'x'
  # title of the data
  # description of the data
dataFolders<-getDirectories(path)
out<-lapply(dataFolders, function(x){
  subfolder<-paste0(path,'/',x)  
  # data.rda
  #load(paste0(subfolder,'/data.rda'))
  
  #data from data.txt
  x<-scan(paste0(subfolder,'/','data.txt'))
  
  #muHat
  #muHat<-mean(df[,1])
  muHat<-mean(x)
  
  # sigmaHat
  #sigmaHat<-sd(df[,1])
  sigmaHat<-sd(x)
  
  # breaks
  #breaks<-hist(x)$breaks
  breaks<-NULL
  
  # description.txt
  filename=paste0(subfolder,'/description.txt')
  description<-NULL
  description<-try(readChar(filename,file.info(filename)$size))
  
  # title.txt
  filename=paste0(subfolder,'/title.txt')
  title<-NULL
  title<-try(readChar(filename,file.info(filename)$size))
  
  # units.txt
  filename=paste0(subfolder,'/units.txt')
  units<-NULL
  units<-try(readChar(filename,file.info(filename)$size))
  
  
  #output
  list(title=title,description=description,units=units,
       x=x,muHat=muHat,sigmaHat=sigmaHat,breaks=breaks)
})

return(out)

}



if(F){
setwd('/Users/jzapata/Dropbox/F 18/Shiny Apps/Apps/3_RealLifeGaussianData')
# Importing data
path = '/Users/jzapata/Dropbox/F 18/Shiny Apps/Apps/3_RealLifeGaussianData/datasets'
data<-getData(path)
dictionary<-c(1:length(data))
titles<-sapply(data, function(x) x$title)
names(dictionary)<-titles

# data preprocessing
percentObs=1
data.list<-data[[2]]
x<-data.list$x
muHat<-data.list$muHat
sigmaHat<-data.list$sigmaHat
nObs<-round(percentObs*length(x))


x.obs<-sample(x,size=nObs,replace = F)
#x.fit<-data.list$breaks
x.fit<-muHat+(3*sigmaHat)*seq(-1 ,1,length=100)
density.fit <- dnorm(x.fit,mean=muHat,sd=sigmaHat)

plot_ly(x = x, type = "histogram", name = "Histogram",
            autobinx=F,
            xbins = list(start= muHat-3*sigmaHat,
                         end= muHat+3*sigmaHat,
                         size=6*sigmaHat/100)
) %>% 
  add_trace(x = x.fit, y = density.fit, 
            #nbinsx=floor(80/sqrt(myData$n)),
            type = "scatter", 
            mode = "lines", 
            line = list(color = '#febc15'),
            fillcolor = 'rgba(254, 188, 21, 0.5)',
            fill = "tozeroy", 
            yaxis = "y2", 
            #yaxis = list(title="y2",showline=F, showticklabels = F,showgrid = F, zeroline=F),
            name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right",showline=F,showticklabels = F,showgrid = F),
         yaxis=list(title="Counts", titlefont= list(size=12)),
         legend = list(showlegend = F)) %>%
  config(displayModeBar = F)



xfit<-seq(min(x),,length=100)
fit <- dnorm(xfit,mean=myData[[i]]$mu,sd=myData[[i]]$sd/sqrt(myData[[i]]$n))




nObs<-round(0.30*length(x))
x.obs<-sample(x,size=nObs,replace = F)

hist(x.obs)





}




