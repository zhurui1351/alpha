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
                  },
                  
                  is_up_line = function(data,line)
                  {
                    low = as.numeric(data$Close)
                    if(low > line)
                    {
                      return(T)
                    }
                    else
                    {
                      return(F)
                    }
                  },
                  
                  is_down_line = function(data,line)
                  {
                    high = as.numeric(data$High)
                    if(high < line)
                    {
                      return(T)
                    }
                    else
                    {
                      return(F)
                    }
                  },
                  
                  is_cross_line = function(data,line)
                  {
                    high = as.numeric(data$High)
                    low = as.numeric(data$Close)
                    
                    if(high >= line && low<=line)
                    {
                      return(T)
                    }
                    else
                    {
                      return(F)
                    }
                  }
                  
                ))