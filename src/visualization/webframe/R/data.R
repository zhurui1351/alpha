require(quantmod)
require(RMySQL)
getdata = function(data,freq=15,isxts=T)
{
  #dbname = 'china_future_ods_m'
  #tbname = 'dlcmi'
  if(isxts)
  {
    data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
  }
  if(freq != 1)
  {
    data = to_minutes(data,freq)
  }
  data$cci = CCI(HLC(data),20)
  data$sma = SMA(Cl(data),20)
  data = as.data.frame(data)
  data = na.omit(data)
  data$Date = rownames(data)
  return(data)
}

# d like '2015-02'
getMonthEnd = function(d)
{
  require('lubridate')
  nd = paste(d,'01',sep='-') 
  nd = ymd(nd) + months(1)
  nd = nd  - days(1)
  return(as.Date(nd))
}

#大陆期货夜盘，分钟转日处理函数
to_day = function(pricedata)
{
  days = as.character(unique(substr(as.character(index(pricedata)),1,10)))
  day = days[1]
  price = pricedata[day]
  open = as.numeric(price[1,]$Open)
  high = max(as.numeric(price$High))
  low = min(as.numeric(price$Low))
  close = tail(price,1)$Close
  vol = sum(as.numeric(price$Vol))
  oi = tail(price,1)$Oi
  rs = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = as.Date(day))
  for(i in 2:length(days))
  {
    preday = days[i-1]
    day = days[i]
    
    starttime = paste(preday,'21:00:00')
    endtime = paste(day,'15:00:00')
    timespan = paste(starttime,endtime,sep='/')
    price = pricedata[timespan]
    if(nrow(price) == 0) next #可能只有夜盘
    open = as.numeric(price[1,]$Open)
    high = max(as.numeric(price$High))
    low = min(as.numeric(price$Low))
    close = tail(price,1)$Close
    vol = sum(as.numeric(price$Vol))
    oi = tail(price,1)$Oi
    r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = as.Date(day))
    rs = rbind(rs,r)
  }
  return(rs)
}

#处理后对齐的数据
to_minutes = function(pricedata,k=5)
{
  times = index(pricedata)
  i = 1
  newtime =  times[i]
  ps = pricedata[i:(i+k-1)]
  open = as.numeric(ps[1,]$Open)
  high = max(as.numeric(ps$High))
  low = min(as.numeric(ps$Low))
  close = tail(ps,1)$Close
  vol = sum(as.numeric(ps$Vol))
  oi = tail(ps,1)$Oi
  r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = newtime)
  rs = r
  i = i + k
  while(i < length(times))
  {
    # print(i)
    newtime =  times[i]
    ps = pricedata[i:(i+k-1)]
    open = as.numeric(ps[1,]$Open)
    high = max(as.numeric(ps$High))
    low = min(as.numeric(ps$Low))
    close = tail(ps,1)$Close
    vol = sum(as.numeric(ps$Vol))
    oi = tail(ps,1)$Oi
    r = xts(data.frame(Open=open,High=high,Low=low,Close=close,Vol=vol,Oi=oi), order.by = newtime)
    rs = rbind(rs,r)
    i = i + k
  }
  return(rs)
}

#获取交易区间时间
get_day_trade_min_series = function(freq=15)
{
  by = paste(freq,'min')
  time = seq(as.POSIXct('2001-01-01 09:00:00'),as.POSIXct('2001-01-01 15:00:00'),by=by)
  time = as.character(time)
  time = substr(time,12,19)
  return(time)
}