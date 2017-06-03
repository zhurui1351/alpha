source('src/config/include.R',encoding='utf-8')
source('src/dw/interface/stock/readallstock.R',encoding='utf-8')

lookups = readallstock(sman=30,atrn = 30)
date = '2017-06-02'
lowprice = 15
highprice = 25

get_state = function(data)
{
  allstatus = data.frame()
  marketmaStatus = MarketMaStatus$new()
  
  for(i in 1:nrow(data))
  {
    d = data[i,]  
    marketmaStatus$update(d)
    status = marketmaStatus$status
    
    r = data.frame(time = index(d),status=status)
    allstatus = rbind(allstatus,r)
  }
  allstatus = xts(allstatus$status,order.by=allstatus$time)
  return(allstatus)
}

shindex = readSHindex(sman=30,atrn = 30)
shstatus = get_state(shindex)

shindex_w = to.weekly(shindex)
colnames(shindex_w) = c('Open','High','Low','Close','Volume')
shindex_w$sma = SMA(Cl(shindex_w),30)
shindex_w$atr = ATR(shindex_w)$atr
shindex_w = na.omit(shindex_w)
shstatus_w = get_state(shindex_w)


satisfied_stocks = c()
for(code in lookups)
{
  if(substring(code,1,1) == 3) next
  stock = get(code)
  currentprice = stock[date]
  if(nrow(currentprice) == 0 )
    next
  close = as.numeric(currentprice$Close)
  sma =  as.numeric(currentprice$sma)
  atr =  as.numeric(currentprice$atr)
  #过滤价格
  if(!(close >= lowprice && close <= highprice))
    next
  
  stock = na.omit(stock)
  
  len = nrow(stock)
  status = get_state(stock[(len-100):len])
  
  if(status[date] == 'down') next
  
  i = which(index(stock) == date)
  preday_data = stock[(i-1),]
  predate = index(preday_data)
  preday_status = status[predate]
  
  if((sma - close)/atr > 2 || (sma - close)/atr < -2)
    next
  if(preday_status == 'down' && status[date] == 'osi_down')
  {
    satisfied_stocks = c(satisfied_stocks,code)
    
  }
   
}