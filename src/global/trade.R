Trade = R6Class('Trade',
                   public = list(
                     record = NULL,
                     stopwin = NULL,
                     stoploss = NULL,
                     movestop = NULL,
                     normalexit = NULL,
                     initialize = function(r,stopwin=NULL,stoploss=NULL,movestop=NULL,normalexit=NULL)
                     {
                       self$record = r
                       self$stoploss = stoploss
                       self$stopwin = stopwin
                       self$movestop = movestop
                       self$normalexit = normalexit
                     },
                     
                     update = function(d,state,...)
                     {
                       r = self$record
                       flag = F
                       
                       if(!is.null(self$stopwin) )
                       {
                         result = self$stopwin(r,d,state)
                         self$record = result[['r']]
                         flag = result[['flag']]
                       } 
                       
                       if(!is.null(self$stoploss) && flag == F  )
                       {
                         result = self$stoploss(r,d,state)
                         self$record = result[['r']]
                         flag = result[['flag']]
                       }
                       
                      
                                             
                        if(!is.null(self$normalexit) && flag == F)
                       {
                         result = self$normalexit(r,d,state)
                         self$record = result[['r']]
                         flag = result[['flag']]
                         
                       }
                        if(!is.null(self$movestop) && flag == F)
                       {
                         self$movestop(r,d,state)
                         
                       }
                       
                       return(flag)
                     }
                   )
)

