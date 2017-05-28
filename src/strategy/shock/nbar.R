source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')
source('src/strategy/shock/nbarclass')


nbarframework = function()
{
  
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  data = orgin_data
  
  nbarstate = NbarState$new()
  position = Position$new()
  
  for(i in 1:nrow(data))
  {
    #print(i)
    d = data[i,]
    
    position = nbar_strategy(d,position,nbarstate)
    position$update(d,nbarstate)
    nbarstate$update(d)   
  }
}

records = position$records
basic_analysis(records)
