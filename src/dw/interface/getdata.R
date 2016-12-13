source('src/config/include.R',encoding='utf-8')

getdata = function(dbname,tbname,freq=15)
{
  dbname = 'china_future_ods_m'
  tbname = 'dlymi'
  data = getTableData(dbname,tbname)
  data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
  data = to_minutes(data,15)
  
  time = as.character(index(data))
  time = substr(time,12,19)
  votile = as.data.frame(data$Close - data$Open)
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))
  times = get_min_series()
  votile = subset(votile,time %in% times)
  xx = dcast(votile,day ~ time,value.var='change')
  xx = xx[,2:ncol(xx)]
  fit = kmeans(xx,10,iter.max = 100)
  windows(800,800)
  p = par(mfrow=c(5,1))
  centers = fit$centers
  for( i in 1:10)
  {
    plot(centers[i,])
    
  }
 
 labels = fit$cluster
 index = which(labels == 5)
 plot(centers[1,],type='l',col = 'green')
 points(as.numeric(xx[index[1],]),type='l',col = 'red')
 points(as.numeric(xx[index[2],]),type='l',col = 'blue')
}