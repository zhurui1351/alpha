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
  times = get_min_series()
  
  data_open = data[time %in% c('09:00:00'),]
  data_open_votile = (data_open$Close - data_open$Open)/data_open$Open
  votile = as.data.frame(Delt(data$Close))
  votile[time %in% c('09:00:00'),] = data_open_votile
#  votile[1,1] = 0
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))
  votile = subset(votile,time %in% times)
  xx_dcast = dcast(votile,day ~ time,value.var='change')
  xx = xx_dcast[,2:ncol(xx_dcast)]
  xx = na.omit(xx)
  pre_xx = xx
  mean_xx = lag(apply(pre_xx,1,mean),1)
  sd_xx = lag(apply(pre_xx,1,sd),1)
  pre_xx = xx - mean_xx
  pre_xx = pre_xx / sd_xx
  xx = na.omit(pre_xx)
  xx[xx == Inf] = 0
  xx[xx == -Inf] = 0
  #pre_xx$mean = lag(mean_xx,1)
  #pre_xx$sd = lag(sd_xx,1)

  y = t(xx)
  y_scaled = scale(y)
  y_t = na.omit(t(y_scaled))
  xx_scaled = scale(xx)
  n = 10
  fit = kmeans(xx,n,iter.max = 100)
  centers = fit$centers
  labels = fit$cluster

  #centers_unscaled = unscale(centers,xx_scaled)  
  windows(1000,1000)
  #p = par(mfrow=c(5,2))
  #plot(centers[1,],type='l',ylim = range(-0.01,0.01),xlab='',xaxt = 'n') #ylim = range(-6,6)
  plot(centers[1,],type='l',ylim = range(max(centers),min(centers)),xlab='',xaxt = 'n') #ylim = range(-6,6)

  axis(1, 1:length(centers[1,]),names(centers[1,]))
  for( i in 2:n)
  {
    points(centers[i,],type='l',col=i)
  }
 


 cluster_n = 3
 index = which(labels == cluster_n)
 
 p = par(mfrow=c(2,1))
 m = index[10]
 date = xx_dcast[m,]$day
 plot(as.numeric(data[date]$Close),type = 'l',xlab='',xaxt = 'n')
 axis(1, 1:length(centers[1,]),names(centers[1,]))
 plot(centers[cluster_n,],type='l',col = 'green',xlab='',xaxt = 'n')
 axis(1, 1:length(centers[1,]),names(centers[1,]))
 par(p)



for( i in 1:n)
{
  plot(centers[i,],type='l',ylim = range(max(centers),min(centers)),xlab='',xaxt = 'n',col=i) #ylim = range(-6,6)  
  axis(1, 1:length(centers[1,]),names(centers[1,]))
  tmp = scan()
}
}

compute_distance = function(centers,k,v)
{
   sample_centers = apply(centers,MARGIN = 1 ,function(x,k){return(x[1:k])},k)
   sample_centers = t(sample_centers)
   
   distance = apply(sample_centers,MARGIN = 1 ,function(x,v){dist(rbind(x,v))},v)
   min_index = which.min(distance)
   
   plot(as.numeric(centers[min_index,]),col = 'blue')
   points(as.numeric(as.numeric(v)),col='red')
   abline(h = 0,col='yellow')
}