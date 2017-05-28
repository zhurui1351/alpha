Trade = R6Class('Trade',
                   public = list(
                     record = NULL,
                     stopwin = NULL,
                     stoploss = NULL,
                     movestop = NULL,
                     
                     initialize = function(r,stopwin=NULL,stoploss=NULL,movestop=NULL)
                     {
                       self$record = r
                       self$stoploss = stoploss
                       self$stopwin = stopwin
                       self$movestop = movestop
                     }
                   )
)

