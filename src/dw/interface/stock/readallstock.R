readallstock = function(sman=30,atrn = 30)
{
  require('dplyr')
  print(now())
  e = parent.env(environment())
  lookups = c()
  indexlookups =  1
  path = "D:/data/stock/dest"
  files = dir(path)
  rm(list=files,envir=e)
  lookups = c()
  indexlookups =  1
  for(f in files)
  {
    #print(f)
    fname = file.path(path,f)
    pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
    if(nrow(pricedata) < 500){ next}
    colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
    #time(pricedata)=as.POSIXct(time(pricedata))
    pricedata=as.xts(pricedata)
    pricedata$sma = SMA(Cl(pricedata),n=sman)
    pricedata$atr = ATR(HLC(pricedata),n=atrn)$atr   
    pricedata = na.omit(pricedata)
  #  rs = RS(Cl(shindex),Cl(pricedata))
   # rs[which(rs==Inf | rs == -Inf)] = 0
    
  #  pricedata$rs = rs 
    fname = strsplit(f,'.',fixed=T)[[1]][1]
    fname = substr(fname,3,8)
    
    assign(fname,pricedata,envir=e)
    lookups[indexlookups] =fname
    indexlookups = indexlookups + 1
  } 
  print(now())
  return(lookups)
}

readallpuredata = function(period='days')
{
  require('dplyr')
  print(now())
  e =parent.env(environment())
  lookups = c()
  indexlookups =  1
  path = "D:/data/stock/dest"
  files = dir(path)
  rm(list=files,envir=e)
  lookups = c()
  indexlookups =  1
  
  for(f in files)
  {
    fname = file.path(path,f)
    pricedata = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
    if(nrow(pricedata) < 500){ next}
    colnames(pricedata)<-c("Open","High","Low","Close","Volume","Amount")
    time(pricedata)=as.POSIXct(time(pricedata))
    pricedata=as.xts(pricedata)
    pricedata = to.period(pricedata,period)
    fname = strsplit(f,'.',fixed=T)[[1]][1]
    fname = substr(fname,3,8)
    #print(fname)
    assign(fname,pricedata,envir=e)
    lookups[indexlookups] =fname
    indexlookups = indexlookups + 1
  } 
  print(now())
  return(lookups)
}

readSHindex = function(sman=30,atrn = 30)
{
  path = "D:/data/stock/index/dest"
  #Sys.setenv(TZ="UTC")
  f='SH000001.TXT'
  fname = file.path(path,f)
  shindex = read.zoo(fname,header=FALSE, format = "%m/%d/%Y",sep=",",index.column=1) 
  colnames(shindex)<-c("Open","High","Low","Close","Volume","Amount")
 # time(shindex)=as.POSIXct(time(shindex))
  shindex=as.xts(shindex)
  shindex$sma = SMA(Cl(shindex),n=sman)
  shindex$atr = ATR(HLC(shindex),n=atrn)$atr   
  shindex = na.omit(shindex)
  return(shindex)
}
