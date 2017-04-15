source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')

get_splines_sep = function(freq=15,n=225)
{
  x = 1:freq
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = n)
  return(list(x,ht))
}

plot_centers = function(centers_scale)
{
  windows(3000,3000)
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  p =par(mfrow=c(3,5))
  for(i in 1:n)
  {
    plot(ht,centers_scale[i,])
  }
}
flat_time_data = function(data,diffclose='cl',freq=15)
{
  time = as.character(index(data))
  time = substr(time,12,19)
  
  times = get_day_trade_min_series(freq)
  data_open = data[time %in% c('09:00:00'),]
  data_open_votile = (data_open$Close - data_open$Open)
  if(diffclose == 'cl')
  {
    votile = as.data.frame(data$Close)    
  }
  else if(diffclose =='diffcl' )
  {
    votile = as.data.frame(diff(data$Close))
    votile[time %in% c('09:00:00'),] = data_open_votile
  }
  else if(diffclose == 'diffco')
  {
    votile = as.data.frame(data$Close - data$Open)  
  } 
  else if(diffclose == 'median')
  {
    votile = as.data.frame((data$High - data$Low)/2 + data$Low)   
  }
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))
  votile = subset(votile,time %in% times)
  xx_dcast = dcast(votile,day ~ time,value.var='change')
  return(xx_dcast)
  
}

train_svm = function(xx,labels,k)
{
  scaled = apply(xx[,1:k],MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
  scaled = t(scaled)
  ll = as.factor(labels)
  all = data.frame(scaled,ll)
  all = na.omit(all)
  
  ll = all[,ncol(all)]
  scaled = all[,1:(ncol(all)-1)]
  m = svm(scaled,ll)
  
  l = predict(m)
  print(sum(l == ll)/length(l))
  return(m)
}


predict_center_svm = function(m,v,k=9,isstrategy = F)
{
  v_scale = scale(v[1:k])
  center = predict(m,as.data.frame(t(v_scale)))
  if(isstrategy)
    print(center_strategy(centers_scale,center,k,threshold=0.5))
  return(center)
  
}

predict_center = function(v,centers,centers_scale,k=9,isplot = F)
{
  v = as.numeric(v)
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  
  vv = x[k]
  ht_pre = ht[ht<=vv]
  kl = length(ht_pre)
  xv = x[1:k]
  
  sample_centers = apply(centers,MARGIN = 1 ,function(x,k,xv,ht_pre){
    y =scale(x[1:k])
    fm = lm(y ~ bs(xv,df=5))
    return(predict(fm, data.frame(xv = ht_pre)))
    
  },k,xv,ht_pre)
  sample_centers = t(sample_centers)
  
  y1 = scale(v[1:k])
  y2 = as.numeric(scale(v))
  fm_predict = lm(y1 ~ bs(xv, df = 5))
  fm_predict_value = predict(fm_predict, data.frame(xv = ht_pre))
  
  distance = apply(sample_centers,MARGIN = 1 ,function(x,v){dist(rbind(x,v))},fm_predict_value)
  min_index = which.min(distance)
  #print(min_index)
  if(isplot)
  {
    cen = as.numeric(centers_scale[min_index,])
    windows(500,500)
    plot(x,c(y2,rep(0,(length(x)-length(y2)))),col = 'blue')
    lines(ht_pre,fm_predict_value,col='green')
    lines(ht,cen,col='red')
  }
  
  return(min_index)
  
}

basic_stats = function(xx,num_centers=15,k=9,centers,labels)
{
  
  prlabels = c()
  num = c()
  for(i in 1:nrow(xx))
  {
    v = xx[i,]
    predict_point = 12
    tmp = tryCatch(predict_center(v,centers,centers_scale,predict_point,F),error=function(e) e)
    if(inherits(tmp, "error") || length(tmp) == 0 )
    {
      num = c(num,i)
      next
    }
    prlabels = c(prlabels,tmp)
  }
  ll = labels[-num]
  table(ll,prlabels)
  
  numcol = ncol(centers)
  prlabels = apply(xx,MARGIN = 1,predict_center,centers,centers_scale,k=12)
   
  return(list(dt,dt_sep))  
  
}

transform_bs = function(xx_scaled,df=5)
{
  result = data.frame()
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  for(i in 1:nrow(xx_scaled))
  {    
    nsample =i   
    y=xx_scaled[nsample,1:15]
    fm = lm(y ~ bs(x, df = df))
    points = predict(fm, data.frame(x = ht))
    result = rbind(result,points)
  }
  return(result)
}

center_strategy = function(centers_scale,i,k,threshold=0.5)
{
  strategy = 'normal'
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  piindex = max(which(ht<=x[k]))
  ratio = centers_scale[i,length(ht)]-centers_scale[i,piindex]
  if(ratio > threshold)
    strategy = 'long'
  else if(ratio < -threshold)
    strategy = 'short'
  else
    strategy = 'normal'
  return(strategy)    
}

strategy_test = function(xx,centers,centers_scale,predict_point=9,threshold=0.5,m,stopratio =2,
                         profitratio = 1)
{
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  points_result = data.frame()
  for(i in 1:nrow(xx))
  {
    start_point = predict_point + 1
    v = as.numeric(xx[i,])
    day = xx_dcast[i,]$day  
    #tmp = tryCatch(predict_center(v,centers,centers_scale,predict_point,F),error=function(e) e)
    
    tmp = tryCatch(predict_center_svm(m,v,predict_point),error=function(e) e)
    
    if(inherits(tmp, "error") || length(tmp) == 0 ) next
    center_pr = tmp
    strategy = center_strategy(centers_scale,center_pr,predict_point,threshold=threshold)
    if(strategy == 'normal') next
    
    day_data = data[day]
    open = as.numeric(day_data[1,]$Open)
    
    op = as.numeric(day_data[start_point,]$Open)
    cl = as.numeric(day_data[15,]$Close)
    
    hi = max(as.numeric(day_data[start_point:15,]$High))
    low = min(as.numeric(day_data[start_point:15,]$Low))
    
    sd = sd(as.numeric(day_data[1:predict_point,]$Close))
    atr = as.numeric(ATR(HLC(day_data[1:predict_point,]),n=5)$atr)
    
    atr = round(atr[length(atr)])
    sd = round(sd)
    
    var = sd
    
    stop_long = round(op - stopratio*var)
    stop_short = round(op + stopratio*var)
    profit_long = round(op + profitratio*var)
    profit_short = round(op - profitratio*var)
    
    type = 'normal'
    
    for(j in start_point:15)
    {
      curbar = day_data[j,]
      
      curhigh = as.numeric(curbar$High)
      curlow = as.numeric(curbar$Low)
      
      if(strategy == 'short' && curhigh >= stop_short)
      {
        cl = curhigh
        type = 'stopshort'
        break
      }
      else if(strategy == 'short' && curlow <= profit_short)
      {
        cl = curlow
        type = 'profitshort'
        break
      }
      else if(strategy == 'long' && curlow <= stop_long)
      {
        cl = curlow
        type = 'stoplong'
        break
      }
      else if(strategy == 'long' && curhigh >= profit_long)
      {
        cl = curhigh
        type = 'profitlong'
        break
      }    
    }
    profit = ifelse(strategy == 'long',(cl-op),(op-cl))
    r = data.frame(i=i,day=day,center = center_pr,open=open,op=op,hi = hi,low=low,close=cl,profit=profit,var=sd,strategy=strategy,type=type)
    points_result = rbind(points_result,r)
  }
  return(points_result)
}

stat_orgin_centers = function(xx_dcast,k=9,n=15,threshold=0.5)
{
  result = data.frame()
  startpoint = k + 1  
  for( i in 1:n)
  {
    class_n =  xx_dcast[which(labels == i),]
    
    days = class_n[,'day']
    
    points = sapply(days,function(d){
      
      day_data = data[d]
      op = as.numeric(day_data[startpoint,]$Close)
      cl = as.numeric(day_data[15,]$Close)
      gap = cl - op
      return(gap)
    }
    )
    strategy = center_strategy(centers_scale,i,k,threshold=0.5)
    
    totalpoints = sum(points)
    len = length(points)
    ratio = length(points[points>0])/length(points)
    r = data.frame(center=i,strategy=strategy,points=totalpoints,len=len,ratio=ratio)
    result = rbind(result,r)
  }
  return(result)
}

run =function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  data = getdata(dbname,tbname,freq)
  
  num_centers = 10
  
  xx_dcast = flat_time_data(data,diffclose='cl',freq=freq)
  xx = xx_dcast[,2:ncol(xx_dcast)] 
  
  indices = which(apply(xx,MARGIN=1,function(x)(all(as.numeric(x)==x[1]))))
  if(length(indices) > 0 )
  {
    xx=xx[-indices,]
    xx_dcast =xx_dcast[-indices,]
  }
  
  xx_scaled = apply(xx,MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
  xx_scaled = t(xx_scaled)
  
  result = transform_bs(xx_scaled,df=5)
  
  n = 15
  set.seed(1234)
  clust = pam(result,n)
  centers_index = clust$id.med
  centers_scale = result[centers_index,]
  labels = clust$clustering
  
  centers = xx[centers_index,]
  plot_centers(centers_scale)
  predict_point = 12
  
  stat_orgin_centers(xx_dcast,k=predict_point,n=n,threshold=0.5)
  
  m = train_svm(xx,labels,k=predict_point)

  points_result = strategy_test(xx,centers,centers_scale,predict_point=predict_point,threshold=0.5,m=m,stopratio =5,
                             profitratio = 5)
    
  profit = points_result$profit
  sum(profit)
  length(profit)
  length(profit[profit>0])/length(profit)
  aggregate(profit,by = list(points_result$center),sum)
  
  aggregate(profit,by = list(points_result$center),function(x){return(c(length(x[x>0])/length(x)))})
  aggregate(profit,by = list(points_result$center),function(x){return(length(x))})
  
}



