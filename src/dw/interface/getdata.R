source('src/config/include.R',encoding='utf-8')

getdata = function(dbname,tbname,freq=15)
{
  #dbname = 'china_future_ods_m'
  #tbname = 'dlcmi'
  data = getTableData(dbname,tbname)
  data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
  data = to_minutes(data,freq)
  return(data)
}

