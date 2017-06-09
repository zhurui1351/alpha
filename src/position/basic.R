defaultstoploss = function(r,d,state=NULL)
{
  stoploss = r$stoploss
  type = r$type
  high = as.numeric(d$High)
  low = as.numeric(d$Low)
  time = as.character(index(d))
  
  newr = r 
  flag = F
  if(type == 'long')
  {
    if(low <= stoploss)
    {
      newr$closetime = time
      newr$close = low#stoploss
      newr$exittype = 'longloss'
      flag = T
    }
  }
  else if(type == 'short')
  {
    if(high >= stoploss)
    {
      newr$closetime = time
      newr$close =  high#stoploss
      newr$exittype = 'shortloss'
      flag = T
    }
  }
  result = list(flag = flag,r = newr)
  return(result)
}

defaultstopwin = function(r,d,state=NULL)
{
  stopwin = r$stopwin
  type = r$type
  high = as.numeric(d$High)
  low = as.numeric(d$Low)
  time = as.character(index(d))
  
  newr = r 
  flag = F
  if(type == 'long')
  {
    if(high >= stopwin)
    {
      newr$closetime = time
      newr$close = stopwin
      newr$exittype = 'longwin'
      flag = T
    }
  }
  else if(type == 'short')
  {
    if(low <= stopwin)
    {
      newr$closetime = time
      newr$close = stopwin
      newr$exittype = 'shortwin'
      flag = T
    }
  }
  result = list(flag = flag,r = newr)
  return(result)
  
}

moveStopByPrePeak = function(r,d,state)
{
  pred = state[['pred']]
  type = r$type
  
  prehigh = max(as.numeric(pred$High))
  prelow =  min(as.numeric(pred$Low))
  
  line = trunc((prehigh+prelow)/2)
  
  if(type == 'long')
  {
    r$stoploss = line - 1
  }
  else if(type == 'short')
  {
    r$stoploss = line + 1
  }
  
  return(r)
}