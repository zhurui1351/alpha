source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')


plot_sma = function(data,start='',end='')
{
  perd = paste(start,end,sep='/')
  plot_data = data[perd]
  chartSeries(plot_data)
  addTA(plot_data$sma,on=1)
}

judege_time = function(datetime,expectedtime)
{
  time = substring(as.character(datetime),12,19)
  if(time == expectedtime)
    return(T)
  else
    return(F)
}

find_top = function(ds)
{
  return(max(as.numeric(ds$High)))
}

find_bottom = function(ds)
{
  return(min(as.numeric(ds$Low)))
  
}

openstratyge = function(d,position,pred,losspoint=10,winpoint=10)
{
  time = as.character(index(d))
  opentime = as.character(index(d))
  expectedtime = '09:00:00'
  
  open = as.numeric(d$Open)
  
  curpostion = position
 # print(curpostion)
  prehigh = find_top(pred)
  prelow = find_bottom(pred)
  
  high = as.numeric(d$High)
  low = as.numeric(d$Low)
  
  if(judege_time(opentime,expectedtime))
  {
    if(high > prehigh)
    {
      op = ifelse(open > prehigh,open,prehigh)
      stoploss = op - losspoint 
      stopwin = op + winpoint 
      r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='long',exittype='')
      trade = Trade$new(r,stopwin=defaultstopwin,stoploss=defaultstoploss)
      curpostion$add(trade)
    }
    if( low < prelow)
    {
      op = ifelse(open < prelow,open,prelow)
      stoploss = op + losspoint 
      stopwin = op - winpoint
      r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='short',exittype='')
      trade = Trade$new(r,stopwin=defaultstopwin,stoploss=defaultstoploss)
      curpostion$add(trade)
    }
  }
 # print(curpostion)  
  return(curpostion)
}

openshockframework = function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  
  data = orgin_data
  n = 3
  
  position = Position$new()  
  
  winpoint = 15
  losspoint = 15
  pred = data[(1:n),]
  
  for(i in (n+1):nrow(data))
  {
    d = data[i,]
    position = openstratyge(d,position,pred,losspoint=losspoint,winpoint=winpoint)
    position$update(d) 
    pred = data[(i-n):i,]
  }
  
  records = position$records
  basic_analysis(records)
}

