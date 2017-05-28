Judge = R6Class('Judge',
                public = list(
                  is_up = function(data)
                  {
                    open = as.numeric(data$Open)
                    close = as.numeric(data$Close)
                    
                    if(close > open)
                      return(T)
                    else
                      return(F)
                  },
                  
                  is_down = function(data)
                  {
                    open = as.numeric(data$Open)
                    close = as.numeric(data$Close)
                    
                    if(close < open)
                      return(T)
                    else
                      return(F)
                  },
                  
                  is_cross = function(data)
                  {
                    open = as.numeric(data$Open)
                    close = as.numeric(data$Close)
                    if(open == close)
                      return(T)
                    else
                      return(F)
                  }
                  
                ))

NbarState = R6Class('nbarstate',
                    public=list(
                      judge = Judge$new(),
                      upcount = 0,
                      downcount = 0,
                      update = function(d){
                        if(self$judge$is_up(d))
                        {
                          self$upcount = self$upcount+1
                          self$downcount = 0
                        }
                        else if(self$judge$is_down(d))
                        {
                          self$upcount = 0
                          self$downcount = self$downcount+1
                        }
                        else if(self$judge$is_cross(d))
                        {
                          self$upcount = 0
                          self$downcount = 0
                        }                       
                      }
                      
                      ))

nbar_strategy = function(d,position,nbarstate,losspoint=10,winpoint=10)
{
  time = as.character(index(d))
  open = as.numeric(d$Open)
  sma = as.numeric(d$sma)
  curpostion = position
  
  if(nbarstate$upcount == 3 )
  {
    #openbuy()
    stoploss = open - losspoint 
    stopwin = open + winpoint 
    r = data.frame(opentime=time,closetime=NA,open=open,close=NA,stopwin=stopwin,stoploss=stoploss,type='long',exittype='')
    trade = Trade$new(r,stopwin=defaultstopwin,stoploss=defaultstoploss)
    curpostion$add(trade)
    
  }
  
  if(nbarstate$downcount == 3 )
  {    
    #opensell()
    
    stoploss = open + losspoint 
    stopwin = open - winpoint
    r = data.frame(opentime=time,closetime=NA,open=open,close=NA,stopwin=stopwin,stoploss=stoploss,type='short',exittype='')
    trade = Trade$new(r,stopwin=defaultstopwin,stoploss=defaultstoploss)
    curpostion$add(trade)
  }
  return(curpostion)
}
