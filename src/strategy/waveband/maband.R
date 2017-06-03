
MarketMaStatus = R6Class('MarketMaStatus',
                       public = list(
                         status = 'init',
                        # prestatus = 'init'
                         judge = Judge$new(),
                         upcount = 0,
                         downcount = 0,
                         update = function(d)
                         {
                           line = d$sma
                           atr = d$atr
                           #暂不考虑触发
                           #trigger_up = line + atr
                           #trigger_down = line - atr
                           
                           if(self$judge$is_up_line(d,line))
                           {
                             self$upcount = self$upcount+1
                             self$downcount = 0
                             
                             if(self$status == 'down')
                             {
                              self$status = 'osi_down'  
                             }
                             
                             if(self$upcount >= 5)
                             {
                               self$status = 'up'
                             }
                           }
                           else if(self$judge$is_down_line(d,line))
                           {
                             self$upcount = 0
                             self$downcount = self$downcount + 1
                             
                             if(self$status == 'up')
                             {
                               self$status = 'osi_up'  
                             }
                             
                             if(self$downcount >= 5)
                             {
                               self$status = 'down'
                             }
                           }
                           else if(self$judge$is_cross_line(d,line))
                           {
                             self$upcount = 0
                             self$downcount = 0
                             if(self$status =='up' || self$status == 'down')
                             {
                               self$status = paste('osi',self$status,sep='_')
                             }
                           }
                                                  
                         }
                         
                         )
                       )



mabandframework = function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  
  data = orgin_data
  
  sma = SMA(data$Cl,20)
  atr = ATR(data,20)
  atr = atr$atr
  
  data$atr = atr
  data$sma = sma
  
  data = na.omit(data)
  marketmaStatus = MarketMaStatus$new()
  
  allstatus = data.frame()
  
  for(i in 1:nrow(data))
  {
    d = data[i,]  
    marketmaStatus$update(d)
    status = marketmaStatus$status
    
    r = data.frame(time = index(d),status=status)
    allstatus = rbind(allstatus,r)
  }
  allstatus = xts(allstatus$status,order.by=allstatus$time)
}