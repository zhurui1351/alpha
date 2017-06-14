defaultstoploss = function(r,d,state=NULL)
{
  stoploss = r$stoploss
  type = r$type
  high = as.numeric(d$High) #as.numeric(d$Close)
  low = as.numeric(d$Low)#as.numeric(d$Close)
  time = as.character(index(d))
  
  newr = r 
  flag = F
  if(type == 'long')
  {
    if(low <= stoploss)
    {
      newr$closetime = time
      newr$close = stoploss#stoploss
      newr$exittype = 'longloss'
      flag = T
    }
  }
  else if(type == 'short')
  {
    if(high >= stoploss)
    {
      newr$closetime = time
      newr$close =  stoploss#stoploss
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

stoplossbycci = function(r,d,state=NULL)
{
  stoploss = r$stoploss
  type = r$type
  pred = state
  precci = as.numeric(pred$cci)
  
  op = as.numeric(d$Open)
  
  newr = r 
  flag = F
  if(type == 'long')
  {
    if(precci < 100)
    {
      newr$closetime = time
      newr$close = op#stoploss
      newr$exittype = 'longloss'
      flag = T
    }
  }
  else if(type == 'short')
  {
    if(precci >= -100)
    {
      newr$closetime = time
      newr$close =  op#stoploss
      newr$exittype = 'shortloss'
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
    r$stoploss = prelow-1
  }
  else if(type == 'short')
  {
    r$stoploss = prehigh + 1
  }
  
  return(r)
}


MoveFixPoints =  R6Class('MoveFixPoints',
                         public = list(
                           barstate = NULL,
                           initialize = function(barstate)
                           {
                             self$barstate = barstate
                           },
                           update = function(r,d,state=NULL)
                           {
                             open = r$open
                             type = r$type
                             profit_long = self$barstate$curhigh - open 
                             profit_short = open - self$barstate$curlow
                             if(type == 'long')
                             {
                               if(profit_long>10)
                               {
                                 r$stoploss = open
                                 r$stoploss = r$stoploss + profit_long-9
                                 
                               }
                               
                             }
                             else if(type == 'short')
                             {
                               
                               if(profit_short>10)
                               {
                                 r$stoploss = open
                                 r$stoploss = r$stoploss - profit_short + 9               
                              }                             
                             }
                             self$barstate$update(d)
                             return(r)
                           }
                           )
                         
                         )