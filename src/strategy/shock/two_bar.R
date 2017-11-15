source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface',encoding='utf-8')
sourceDir('src/algorithm',encoding='utf-8')
source('src/strategy/shock/nbarclass.R',encoding='utf-8')
sourceDir('src/dw/interface/stock',encoding='utf-8')


TwobarState = R6Class('TwobarState',
                    public=list(
                      upcount = 0,
                      prehigh = Inf,
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

orgin_data = readSHindex()
