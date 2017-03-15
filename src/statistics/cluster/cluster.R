source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')
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
    votile = as.data.frame(data$Close)
    #votile[time %in% c('09:00:00'),] = data_open_votile
    
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


predict_center = function(v,centers,k=10,ht,x,isplot = F)
{
  vv = x[k]
  ht_pre = ht[ht<=vv]
  kl = length(ht_pre)
  sample_centers = apply(centers,MARGIN = 1 ,function(x,k){return(x[1:k])},kl)
  sample_centers = t(sample_centers)
  
  xv = x[1:k]
  y1 = scale(v[1:k])
  y2 = scale(v)
  fm_predict = lm(y1 ~ bs(xv, df = 5))
  fm_predict_value = predict(fm_predict, data.frame(xv = ht_pre))
  
  distance = apply(sample_centers,MARGIN = 1 ,function(x,v){dist(rbind(x,v))},fm_predict_value)
  min_index = which.min(distance)
  
  if(isplot)
  {
    cen = as.numeric(centers[min_index,])
    plot(x,y2,col = 'blue')
    lines(ht_pre,fm_predict_value,col='green')
    lines(ht,cen,col='red')
  }
  
  return(min_index)
  
}

basic_stats = function(xx,num_centers=15,k=9,centers,labels)
{
  
  numcol = ncol(centers)
  prlabels = apply(xx,MARGIN = 1,predict_center,centers,k)
   
  dt_sep = data.frame()
  dt = data.frame()
  for( i in 1:num_centers)
  {
    cluster_n = i
    index = which(prlabels == cluster_n)
    label_data = xx[index,]
    count = length(index)
    point = label_data[,(k+1):numcol]
    
    point_col_sum = apply(point,2,sum)
    point_col_ratio = apply(point,2,function(x){sum(x>0)/sum(x!=0)})
    
    point_col_sum = cbind(as.data.frame(t(point_col_sum)))
    point_col_ratio = cbind(as.data.frame(t(point_col_ratio)))
    
    colnames(point_col_sum) = paste(colnames(point_col_sum),'sum',sep='_')
    colnames(point_col_ratio) = paste(colnames(point_col_ratio),'ratio',sep='_')
    point_col = cbind(point_col_ratio,point_col_sum)
    point_col = cbind(data.frame(center=i,count=count),point_col)
    dt_sep = rbind(dt_sep,point_col)
    
    point_sum = apply(point,1,sum)
    total = sum(point_sum)
    
    upratio = sum(point_sum>0)/sum(point_sum!=0)
    
    pvalue = prop.test(count*upratio,count,0.55,alternative='greater')
    uppvalue = pvalue$p.value
    
    
    r = data.frame(center=i,sum = total,upratio = upratio,count,uppvalue=uppvalue)
    dt = rbind(dt,r)
  }
  
  return(list(dt,dt_sep))  
  
}

run =function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlcmi'
  freq = 15
  data = getdata(dbname,tbname,freq)
  
  num_centers = 10
  seed = 2134
  
  xx_dcast = flat_time_data(data,diffclose=T,freq=freq)
  xx = xx_dcast[,2:ncol(xx_dcast)] 
  
  indices = which(apply(xx,MARGIN=1,function(x)(all(as.numeric(x)==x[1]))))
  xx=xx[-indices,]
  xx_dcast =xx_dcast[-indices,]
    
  numcol = ncol(xx)
  point_total = xx[,1:numcol]
  point_total_ratio_up = apply(point_total,2,function(x){sum(x>0)/sum(x!=0)})
  point_total_ratio_down = apply(point_total,2,function(x){sum(x<0)/sum(x!=0)})
  
  result = cluster(xx,num_centers,seed=seed)
  centers = result[['centers']]
  labels = result[['labels']]
  
  
  xx_dcast = flat_time_data(data,diffclose=F,freq=freq)
  xx = xx_dcast[,2:ncol(xx_dcast)]

  numcol = ncol(xx)
  point_total = xx[,1:numcol]
  point_total_ratio_up_op = apply(point_total,2,function(x){sum(x>0)/sum(x!=0)})
  point_total_ratio_down_op = apply(point_total,2,function(x){sum(x<0)/sum(x!=0)})
  
  result = cluster(xx,num_centers,seed=seed) 
  centers_op = result[['centers']]
  labels_op = result[['labels']]
  
  
  k = 9#length(v1)
  
  dt_diffclose = basic_stats(xx,num_centers=num_centers,k=k,centers=centers,labels=labels)
  dt_dc = dt_diffclose[[1]]
  dt_dc_sep = dt_diffclose[[2]]
  
  good_dt = subset(dt_dc, dt_dc$uppvalue < 0.05 )# & dt_dc$count > 100)
   
  dt_open = basic_stats(xx,num_centers=num_centers,k=k,centers=centers_op,labels=labels_op)
  dt_op = dt_open[[1]]
  dt_op_sep = dt_open[[2]]
  
  #获取实时数据并计算    
  pricedata = getWindData()
  y = diff(pricedata$close)
  y[1] = (pricedata[1,]$close - pricedata[1,]$open)
  v1 = as.numeric(y8k786.f)
  
  y = pricedata$close - pricedata$open
  v2 = as.numeric(y)
  predict_center(v1,centers,k)
  predict_center(v2,centers_op,k)
  
  
}

strategy_test = function()
{
  record = data.frame()
  k = 9 
  prlabels = apply(xx,MARGIN = 1,predict_center,centers,k)
  long_data_label = which(prlabels == 6)
  days = as.character(xx_dcast[long_data_label,1])
  
  stop_point = 15
  profit_point = 15
  
  type = 'long'
  for(day in days)
  {
    dayinfo = data[day]
    
    info = dayinfo[(k+1),]
    open_price = as.numeric(info$Open)
    
    open_time = as.character(index(info))
    
    stop_price = open_price - stop_point
    profit_price = open_price + profit_point
    
    clear_out = F
    for(i in (k+1) : nrow(dayinfo))
    {
      currentbar = dayinfo[i,]
      high = as.numeric(currentbar$High)
      low = as.numeric(currentbar$Low)
      open = as.numeric(currentbar$Open)
      close = as.numeric(currentbar$Close)
      
      time = as.character(index(currentbar))
      
      #止损
      if(low < stop_price)
      {
        out = stop_price
        clear_out = T
        cleartime = time
        break
      }
      #止盈
      if(high > profit_price)
      {
        out = profit_price
        clear_out = T
        cleartime = time
        break
      }
        
    }
    #是否定时出场
    if(!clear_out)
    {
      out = close
      cleartime = time
    }
    
    r = data.frame(opentime=open_time,open=open_price,cleartime=cleartime,close=out,type = type)
    record = rbind(record,r)
  }
  
  record$profit = record$close - record$open 
  sum(record$profit)
  sum(record$profit > 0) /length(record$profit)
  
  
  xx_scaled = apply(xx,MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
  xx_scaled = t(xx_scaled)
  d = dist(xx_scaled,function(x,y){return(hausdorff_dist(x,y))})
  d = dist(xx_scaled,function(x,y){d=dtw(x,y) 
                                   return(d$distance)})
  d = dist(xx_scaled)
  d = dist(xx_scaled[1:100,],function(x,y){#x1 = matrix(c(1:15,x),ncol=2)
                                   #y1 = matrix(c(1:15,y),ncol=2)
                                   return(frechet_diss(x,y))})
  Sys.time()
  d = as.matrix(d)
  
  n = 15
  clust = pam(d,n,diss=T)
  centers_index = clust$id.med
  centers = xx_scaled[centers_index,]
  labels = clust$clustering
  
  }

