source('src/config/include.R',encoding='utf-8')
sourceDir('src/utilities/',encoding='utf-8')
source('src/dw/collectdata/utility.R',encoding='utf-8')


collectdatafromtaobao = function(path,pattern = 'DLAMI*',nighttradetime)
{
  #path = 'D:/BaiduYunDownload/data'
  files = list.files(
    path,pattern = pattern ,recursive = T,ignore.case = T,full.names = T
  )
  #日期，时间，开盘，最高，最低，收盘 ，成交量，持仓量
  f = files[1]
  da = read.csv(f,header = F,sep = ",")
  colnames(da) = c('Date','Time','Open','High','Low','Close','Vol','Oi')
  da$datetime = paste(da[,1],da[,2])
  da$Vol = 0
  da = xts(da[,3:8],order.by = as.POSIXct(da$datetime))
  pricedata = da
  
  for (f in files[2:length(files)])
  {
    da = read.csv(f,header = F,sep = ",")
    colnames(da) = c('Date','Time','Open','High','Low','Close','Vol','Oi')
    da$datetime = paste(da[,1],da[,2])
    da = xts(da[,3:8],order.by = as.POSIXct(da$datetime))
    
    pricedata = rbind(pricedata,da)
  }
  #去重
  pricedata = remove_duplicated(pricedata)
  newpricedata = NULL
  #去除间隔 白天交易
  basedates = as.character(unique(as.Date(index(pricedata))))
  basetime = getbasetime_day()
  p = fill_missing_value(basetime,pricedata)
  newpricedata = rbind(newpricedata,p)
  
  #处理夜盘，夜盘交易周期在不断调整
  night_trade_time = nighttradetime
  if(!is.null(night_trade_time))
  {
    for(i in 1:length(nighttradetime))
    {
      tradetime = nighttradetime[[i]]
      startdate = tradetime[[1]]
      enddate = tradetime[[2]]
      starttime = tradetime[[3]]
      endtime = tradetime[[4]]
      perid = paste(startdate,enddate,sep='/')
      perioddata = pricedata[perid]
      basetime = getbasetime(starttime,endtime) 
      p = fill_missing_value(basetime,perioddata)
      newpricedata = rbind(newpricedata,p)
    }
    
  }
  
  return(newpricedata)
}
