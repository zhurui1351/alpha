Position = R6Class('Position',
                   public = list(
                     records = data.frame(),
                     tradeset = list(),
                     
                     add = function(trade)
                     {
                       self$tradeset = append(self$tradesset,trade)
                     },
                     
                    update = function(d,state=NULL,...)
                    {
                      trades = self$tradeset
                      len = length(trades)
                      removeset = c()
                      
                      if(len == 0)
                        return(T)
                      for(i in 1:len)
                      {
                        trade = trades[[i]]
                        flag = trade$update(d,state,...)
                        if(flag)
                        {
                          removeset = c(removeset,i)
                          self$records = rbind(self$records,trade$record)
                        }
                      }
                      
                      stayset = setdiff(1:len,removeset)
                      self$tradeset = self$tradeset[stayset]
                      return(T)
                    }
                   )
                   )
