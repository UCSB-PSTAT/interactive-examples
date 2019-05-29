get_dataHT<-function(){
  library(Stat2Data)
  data(Pines)
  
  id.10.fertilizer = which(Pines$Spacing==10 & Pines$Fert==1)
  id.10.nofertilizer = which(Pines$Spacing==10 & Pines$Fert==0)
  
  x=na.omit((Pines$Hgt97-Pines$Hgt90)[id.10.fertilizer])
  y=na.omit((Pines$Hgt97-Pines$Hgt90)[id.10.nofertilizer])
  
  # H0: mu_x - mu_y >=0 vs HA: mu_x - mu_y <0
  # Two-sample t-test - equal variance
  
  m = length(x); n = length(y); df = m+n-2;
  xbar = mean(x); ybar = mean(y)
  Sx.sq = var(x); Sy.sq = var(y)
  Spool = ((m-1)*Sx.sq + (n-1)*Sy.sq)/(m+n-2)
  
  t=(xbar-ybar)/(Spool*sqrt(1/m+1/n))

  pvalue = pt(t,df=df)
  
  dataHT=list(x=x,y=y,
              m=m,n=n,df=df,
              xbar=xbar,ybar=ybar,
              Sx.sq=Sx.sq, Sy.sq=Sy.sq,Spool=Spool,
              t=t,pvalue=pvalue)
  
  return(dataHT)
}

