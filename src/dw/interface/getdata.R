source('src/config/include.R',encoding='utf-8')

getdata = function(dbname,tbname,freq=15)
{
  dbname = 'china_future_ods_m'
  tbname = 'dlcmi'
  data = getTableData(dbname,tbname)
  data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
  data = to_minutes(data,15)
  
  time = as.character(index(data))
  time = substr(time,12,19)
  votile = as.data.frame(data$High - data$Open)
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))

  xx = dcast(votile,day ~ time,value.var='change')
  xx = xx[,2:ncol(xx)]
  fit = kmeans(xx,5,iter.max = 100)
 # windows(800,800)
  p = par(mfrow=c(5,1))
  centers = fit$centers
  for( i in 1:5)
  {
    plot(centers[i,])
    
  }
 
 labels = fit$cluster
 index = which(labels == 3)
 plot(centers[3,],type='l',col = 'green')
 points(as.numeric(xx[index[1],]),type='l',col = 'red')
 points(as.numeric(xx[index[2],]),type='l',col = 'blue')
}