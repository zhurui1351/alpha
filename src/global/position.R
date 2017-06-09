Position = R6Class('Position',
                   public = list(
                     records = data.frame(),
                     tradeset = list(),
                     
                     add = function(trade)
                     {
                       self$tradeset = append(self$tradeset,trade)
                     },
                     
                    update = function(d,state=NULL,iswinfirst=T,...)
                    {
                      trades = self$tradeset
                      len = length(trades)
                      removeset = c()
                      flag = F
                      
                      if(len == 0)
                        return(T)
                      for(i in 1:len)
                      {
                        trade = trades[[i]]
                        flag = trade$update(d,state,iswinfirst,...)
                        if(flag)
                        {
                          removeset = c(removeset,i)
                          self$records = rbind(self$records,trade$record)
                        }
                      
                      }
                      
                      stayset = setdiff(1:len,removeset)
                      self$tradeset = self$tradeset[stayset]
                      return(T)
                    },
                    getRecords = function()
                    {
                      records = self$records
                      records = xts(records,as.POSIXct(records$opentime,format = '%Y-%m-%d %H:%M:%S'))
                      return(records)
                    }
                   )
                   )
