source('src/config/include.R',encoding='utf-8')

getdata = function(dbname,tbname,freq=15,isxts=T)
{
  #dbname = 'china_future_ods_m'
  #tbname = 'dlcmi'
  data = getTableData(dbname,tbname)
  if(isxts)
  {
    data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
    fst = substring(as.character(index(data)[1]),12,19)
    if(fst != '09:00:00')
    {
      days = unique(as.character(as.Date(index(data))))
      data = data[days[2:length(days)]]
    }
    
  }
  if(freq != 1)
  {
    data = to_minutes(data,freq)
  }
  return(data)
}

