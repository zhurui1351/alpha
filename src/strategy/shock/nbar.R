source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface',encoding='utf-8')
source('src/dw/collectdata/collectfromwind.R',encoding='utf-8')
sourceDir('src/algorithm',encoding='utf-8')
source('src/strategy/shock/nbarclass.R',encoding='utf-8')


nbarframework = function()
{
  
  dbname ='china_future_ods_m'
  tbname = 'sqrbmi'
  freq = 5
  orgin_data = getdata(dbname,tbname,freq)
  
  data = orgin_data 
  #data$sma = lag(SMA(data$Close,20),1)
  #data = na.omit(data)
  data$atr = ATR(data,20)$atr
  data = na.omit(data)
  
  nbarstate = NbarState$new()
  position = Position$new()  
  
  winpoint = 15
  losspoint = 15
  n = 3
  
  pren = 1
  pred = data[1:pren,]
  
  prepeakn = 3
  predpeak = data[1:prepeakn,]
  
  for(i in (prepeakn+1):nrow(data))
  {
    #print(i)
    state = list(nbstate =nbarstate,pred = predpeak )
    d = data[i,]
    #atr = floor(as.numeric(d$atr))
    
    position = nbar_strategy(d,position,nbarstate,losspoint=losspoint,winpoint=winpoint,n=n,predpeak)
    position$update(d,state,iswinfirst=T)
    nbarstate$update(d)   
    pred = data[(i-pren+1):i,]
    predpeak = data[(i-prepeakn+1):i,]
  }
  records = position$records
  records = position$getRecords()
  basic_analysis(position$records)
}

