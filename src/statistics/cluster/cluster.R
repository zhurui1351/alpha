source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')

flat_time_data = function(data,diffclose=T,freq=15)
{
  time = as.character(index(data))
  time = substr(time,12,19)
  
  times = get_day_trade_min_series(freq)
  data_open = data[time %in% c('09:00:00'),]
  data_open_votile = (data_open$Close - data_open$Open)
  if(diffclose)
  {
    votile = as.data.frame(diff(data$Close))
    votile[time %in% c('09:00:00'),] = data_open_votile
    
  }
  else
  {
    votile = as.data.frame(data$Close - data$Open)
    
  }  
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))
  votile = subset(votile,time %in% times)
  xx_dcast = dcast(votile,day ~ time,value.var='change')
  return(xx_dcast)
  
}

cluster = function(xx,center_num=10,isplot=F)
{
  
  xx_scaled = scale(xx)
  fit = kmeans(xx_scaled,center_num,iter.max = 100)
  centers = fit$centers
  labels = fit$cluster
  
  centers_unscaled = unscale(centers,xx_scaled) 
  centers = centers_unscaled
  if(isplot)
  {
    windows(1000,1000)
    
    plot(centers[1,],type='l',ylim = range(max(centers),min(centers)),xlab='',xaxt = 'n') #ylim = range(-6,6)
    
    axis(1, 1:length(centers[1,]),names(centers[1,]))
    for( i in 2:n)
    {
      points(centers[i,],type='l',col=i)
    }    
  }  
  
  return(list(centers=centers,labels=labels ))
}

compute_distance = function(v,centers,k=9,isplot = F)
{
  vv = v[1:k]
  sample_centers = apply(centers,MARGIN = 1 ,function(x,k){return(x[1:k])},k)
  sample_centers = t(sample_centers)
  
  distance = apply(sample_centers,MARGIN = 1 ,function(x,v){dist(rbind(x,v))},vv)
  min_index = which.min(distance)
  
  if(isplot)
  {
    cen = as.numeric(centers[min_index,])
    plot(v,col = 'blue',xlim=c(1,16),xaxt='n')
    points(cen,col='red')
    abline(h = 0,col='yellow')
    axis(1, 1:length(centers[1,]),names(centers[1,]))
  }
  
  return(min_index)
  
}

basic_stats = function()
{
  dbname = 'china_future_ods_m'
  tbname = 'dlcmi'
  freq=15
  data = getdata(dbname,tbname,freq)
  
  cdata = data
  adata = data
  
  xx_dcast = flat_time_data(data,diffclose=F,freq=freq)
  xx = xx_dcast[,2:ncol(xx_dcast)]
  
  
  
  num_centers = 15
  result = cluster(xx,num_centers)
  
  centers = result[['centers']]
  labels = result[['labels']]
  
  k = 9
  prlabels = apply(xx,MARGIN = 1,compute_distance,centers,k)
  point_total = label_data[,(k+1):numcol]
  point_total_sum = apply(point_total,2,sum)
  point_total_ratio = apply(point_total,2,function(x){sum(x>=0)/length(x)})
  
  dt_sep = data.frame()
  dt = data.frame()
  numcol = ncol(centers)
  for( i in 1:num_centers)
  {
    cluster_n = i
    index = which(prlabels == cluster_n)
    label_data = xx[index,]
    count = length(index)
    point = label_data[,(k+1):numcol]
    
    point_col_sum = apply(point,2,sum)
    point_col_ratio = apply(point,2,function(x){sum(x>=0)/length(x)})
    
    point_col_sum = cbind(as.data.frame(t(point_col_sum)))
    point_col_ratio = cbind(as.data.frame(t(point_col_ratio)))
    
    colnames(point_col_sum) = paste(colnames(point_col_sum),'sum',sep='_')
    colnames(point_col_ratio) = paste(colnames(point_col_ratio),'ratio',sep='_')
    point_col = cbind(point_col_ratio,point_col_sum)
    point_col = cbind(data.frame(center=i,count=count),point_col)
    dt_sep = rbind(dt_sep,point_col)
    
    point_sum = apply(point,1,sum)
    total = sum(point_sum)
    ratio = sum(point_sum>0)/length(point_sum)
    pvalue = prop.test(count*ratio,count)
    pvalue = pvalue$p.value
    r = data.frame(center=i,sum = total,ratio = ratio,count,pvalue=pvalue)
    dt = rbind(dt,r)
  }
  
  good_dt = subset(dt,dt$pvalue < 0.05 & dt$count > 100)
  print(dt_sep)
  print(good_dt)
  
  index_center = which(prlabels == 11)
  
  
}

