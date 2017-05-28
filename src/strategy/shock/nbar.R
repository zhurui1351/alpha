source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface',encoding='utf-8')
source('src/dw/collectdata/collectfromwind.R',encoding='utf-8')
sourceDir('src/algorithm',encoding='utf-8')
source('src/strategy/shock/nbarclass.R',encoding='utf-8')


nbarframework = function()
{
  
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  orgin_data = getdata(dbname,tbname,freq)
  data = orgin_data
  
  #data$sma = lag(SMA(data$Close,20),1)
  #data = na.omit(data)
  
  nbarstate = NbarState$new()
  position = Position$new()  
  
  winpoint = 30
  losspoint = 35
  
  for(i in 1:nrow(data))
  {
    #print(i)
    d = data[i,]
    
    position = nbar_strategy(d,position,nbarstate,losspoint=losspoint,winpoint=winpoint)
    position$update(d,nbarstate)
    nbarstate$update(d)   
  }
}

records = position$records
basic_analysis(records)
