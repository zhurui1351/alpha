
EveryBarState = R6Class('EveryBarState',
                        public = list(
                          curhigh = -Inf,
                          curlow = Inf,
                          #newhighflag = F
                          update = function(d)
                          {
                            high = as.numeric(d$High)
                            low = as.numeric(d$Low)
                            if(high > self$curhigh)
                            {
                              self$curhigh = high
                            }
                            if(low < self$curlow)
                            {
                              self$curlow = low
                            }
                            
                          }
                          )
                        )

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
  
  #line = trunc((prehigh+prelow)/2)
 # line = 15
  
  if(nbarstate$upcount == n && high > prehigh)
  {
    barstate = EveryBarState$new()
    movefixpoints = MoveFixPoints$new(barstate)
    #openbuy()
    op = ifelse(open > prehigh,open,prehigh)
    op = op + 1
    stoploss = op - losspoint  #op - losspoint##prelow# op - losspoint 
    stopwin = op + winpoint 
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='long',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=movefixpoints)
    curpostion$add(trade)
    
  }
  
  if(nbarstate$downcount == n && low < prelow )
  {    
    #opensell()
    barstate = EveryBarState$new()
    movefixpoints = MoveFixPoints$new(barstate)
    
    op = ifelse(open < prelow,open,prelow)
    op = op - 1
    stoploss = op + losspoint#op + losspoint#line+1#prehigh#op + losspoint 
    stopwin = op - winpoint
    r = data.frame(opentime=time,closetime=NA,open=op,close=NA,stopwin=stopwin,stoploss=stoploss,type='short',exittype='')
    trade = Trade$new(r,stopwin=NULL,stoploss=defaultstoploss,movestop=movefixpoints)
    curpostion$add(trade)
  }
  return(curpostion)
}

