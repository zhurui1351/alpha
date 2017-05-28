defaultstoploss = function(r,d,state)
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
      newr$close = stoploss
      newr$exittype = 'longloss'
      flag = T
    }
  }
  else if(type == 'short')
  {
    if(high >= stoploss)
    {
      newr$closetime = time
      newr$close = stoploss
      newr$exittype = 'shortloss'
      flag = T
    }
  }
  result = list(flag = flag,r = newr)
  return(result)
}

defaultstopwin = function(r,d,state)
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