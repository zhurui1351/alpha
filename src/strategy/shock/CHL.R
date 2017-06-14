source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface',encoding='utf-8')
source('src/dw/collectdata/collectfromwind.R',encoding='utf-8')
sourceDir('src/algorithm',encoding='utf-8')
source('src/strategy/shock/nbarclass.R',encoding='utf-8')


CHL_status = R6Class('nbarstate',
                     public = list(
                       precci = 0,
                       uplimit = 100,
                       downlimit = -100,
                       status = 'init',
                       initialize = function(precci=0)
                         {
                            self$precci = as.numeric(precci)
                         },
                       update = function(d)
                       {
                         cci = as.numeric(d$cci)
                         precci = self$precci
                         uplimit = self$uplimit
                         downlimit = self$downlimit
                         
                         if(precci > downlimit && cci < downlimit)
                         {
                           status = 'short'
                         }
                         else if(precci<uplimit && cci > uplimit)
                         {
                           status = 'long'
                         }
                         else{
                           status = 'init'
                         }
                         self$status = status
                         self$precci = cci
                       }
                       
                       )
)
  

CHL_strategy = function(d,position,pred,chlstatus,losspoint=10,winpoint=10,n=1)
{
  time = as.character(index(d))
  open = as.numeric(d$Open)
  cci = as.numeric(d$cci)
  curpostion = position
  
  high = as.numeric(d$High)
  low = as.numeric(d$Low)
  
  len = nrow(pred)
  
  prehigh = max(as.numeric(pred[1:len,]$High)) + n
  prelow = min(as.numeric(pred[1:len,]$Low)) - n 
  
  long_base = max(as.numeric(pred$High)) + 1
  short_base = min(as.numeric(pred$Low)) - 1
  status = chlstatus$status
  
  if(status == 'long' && high > prehigh)
  {
    barstate = EveryBarState$new()
    movefixpoints = MoveFixPoints$new(barstate)
    
    op = ifelse(open > prehigh,open,prehigh)
    stoploss = short_base# op - losspoint 
    stopwin = op + winpoint 
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='long',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=movefixpoints)
    curpostion$add(trade)
  }
  if(status == 'short' && low < prelow)
  {
    barstate = EveryBarState$new()
    movefixpoints = MoveFixPoints$new(barstate)
    
    op = ifelse(open < prelow,open,prelow)
    stoploss = long_base# op + losspoint 
    stopwin = op - winpoint
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='short',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=movefixpoints)
    curpostion$add(trade)
  }
  return(curpostion)
}


CHLframework = function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  
  data = orgin_data
  cci = CCI(data$Cl,10)
  cci = SMA(na.omit(cci),3)
  atr = ATR(data,10)
  atr = atr$atr
  
  data$atr = atr
  data$cci = cci
  data = na.omit(data)
  
  chl_status = CHL_status$new()
  chl_status$uplimit = 100
  chl_status$downlimit = -100
  position = Position$new()  
  
  winpoint = 20
  losspoint = 20
  pren = 3
  
  pred = data[1:pren,]
  
  for(i in (pren+1):nrow(data))
  {
    d = data[i,]
    
    position = CHL_strategy(d,position,pred,chl_status,losspoint=losspoint,winpoint=winpoint,n=0)
    position$update(d,NULL,iswinfirst=F)
    chl_status$update(d)   
    pred = data[(i-pren+1):i,]
  }
  
  records = position$records
  #records = position$getRecords()
  basic_analysis(records)
}