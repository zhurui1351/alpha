

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

nbar_strategy = function(d,position,nbarstate,losspoint=10,winpoint=10,n=3,pred)
{
  time = as.character(index(d))
  open = as.numeric(d$Open)
  sma = as.numeric(d$sma)
  curpostion = position
  high = as.numeric(d$High)
  low = as.numeric(d$Low)
  
  prehigh = max(as.numeric(pred$High))+1 
  prelow = min(as.numeric(pred$Low))-1
  
  if(nbarstate$upcount == n && high > prehigh)
  {
    #openbuy()
    op = ifelse(open > prehigh,open,prehigh)
    stoploss = prelow# op - losspoint 
    stopwin = op + winpoint 
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=NA,stoploss=NA,type='long',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=moveStopByPrePeak)
    curpostion$add(trade)
    
  }
  
  if(nbarstate$downcount == n && low < prelow )
  {    
    #opensell()
    op = ifelse(open < prelow,open,prelow)
    stoploss = prehigh#op + losspoint 
    stopwin = op - winpoint
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=NA,stoploss=NA,type='short',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=moveStopByPrePeak)
    curpostion$add(trade)
  }
  return(curpostion)
}

