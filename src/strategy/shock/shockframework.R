source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')

cross_line = function(d,line)
{
  if(as.numeric(d$High) >= line && as.numeric(d$Low) <= line)
    return(T)
  return(F)
}
higher_line = function(d,line)
{
  if(as.numeric(d$Low) > line)
    return(T)
  return(F)
}
lower_line = function(d,line)
{
  if(as.numeric(d$High) < line)
    return(T)
  return(F)
}

find_init_state = function(data)
{
  count = 0
  state = ''
  for(i in 1:nrow(data) )
  {
    if( count == 5) break
    d = data[i,]
    sma = as.numeric(d[,'sma'])
    
    if(cross_line(d,sma))
    {
      count = 0
      state = ''
      next
    }
    else if(state %in% c('','low') && lower_line(d,sma))
    {
      state = 'low'
      count = count + 1
    }
    else if(state %in% c('','high') && higher_line(d,sma))
    {
      state = 'high'
      count = count + 1
    }
    else
    {
      count = 0
      state = ''
    }
  }
  return(list(state=state,start=i))
}


plot_sma = function(data,start='',end='')
{
  perd = paste(start,end,sep='/')
  plot_data = data[perd]
  chartSeries(plot_data)
  addTA(plot_data$sma,on=1)
}

recognize_state = function()
{
  state_const = c('up','down','osc_up','osc_down')
  state = ''
  n = 20
  count = 0
  
  data = orgin_data
  
  atr = ATR(data,n=5)  
  sma = SMA(data$Close,n)
  
  data$atr = atr$atr
  data$sma = sma
  
  data = na.omit(data)
  
  l = find_init_state(data)
  
  state = l[['state']]
  start = l[['start']]
  prestate = state
  
  for(i in start : nrow(data))
  {
    d = data[i,]
    sma = as.numeric(d[,'sma'])
    atr = as.numeric(d$atr)
    
    #trade_judge
    #update_state
  }
}

shockframework = function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  data = orgin_data
  
  n = 20
  
  atr = ATR(data,n=5)
  atr=floor(lag(atr$atr,1))

  sma = SMA(data$Close,n)
  sma = lag(sma,1)
  
  data$atr = atr
  data$sma = sma
  
  data = na.omit(data)
  result = data.frame()
  
  for(i in 1:nrow(data))
  {
    signal = ''
    
    base = 2
    base_shift = 3
    shift = 3
    
    ishold = F
    
    baseline = as.numeric(data[i,]$sma)
    open = as.numeric(data[i,]$Open)
    close = as.numeric(data[i,]$Close)
    high = as.numeric(data[i,]$High)
    low = as.numeric(data[i,]$Low)
    time = as.character(index(data[i,]))
    
    enter_long = baseline + base
    enter_short = baseline - base
    
    stop_long = enter_long + base_shift
    stop_short = enter_short - base_shift
    
    entertime = ''
    outtime = ''
    enterprice = ''
    exitprice = ''
    tradetype = ''
    exittype = ''
    
    signal = ifelse(open > baseline,'short','long')
    
    
    if(ishold)
    {
      if(tradetype == 'long')
      {
        if(low <= stop_long)
        {
          outtime = time
          exitprice = stop_long
          exittype = 'stop_long'         
        }
        if(high >=1)
        {
          
        }
      }
      else
      {
        
      }
    }
    else
    {
      if(signal == 'long' && high >= enter_long)
      {
        tradetype = 'long'
        ishold = T
        entertime = time
        enterprice = enter_log
      }
      if(signal == 'short' && low <= enter_short)
      {
        ishold= T
        tradetype = 'short'
        entertime = time
        enterprice = enter_short
      }
    }
    
    
  }

}

