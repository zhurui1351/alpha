source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')
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


predict_center = function(v,centers,centers_scale,k=10,ht,x,isplot = F)
{
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

center_strategy = function(centers_scale,i,ht,k,threshold=0.5)
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

strategy_test = function(xx,centers,centers_scale,predict_point=9,threshold=0.5,stopratio =2,
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
    tmp = tryCatch(predict_center(v,centers,centers_scale,predict_point,ht,x,F),error=function(e) e)
    if(inherits(tmp, "error")) next
    center_pr = tmp
    strategy = center_strategy(centers_scale,center_pr,ht,predict_point,threshold=threshold)
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
      
      curhigh = as.numeric(curbar$Close)
      curlow = as.numeric(curbar$Close)
      
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
    r = data.frame(i=i,day=day,center = center_pr,open=open,op=op,hi = hi,low=low,close=cl,profit=profit,atr=atr,strategy=strategy,type=type)
    points_result = rbind(points_result,r)
  }
  return(points_result)
}


run =function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlcmi'
  freq = 15
  data = getdata(dbname,tbname,freq)
  
  num_centers = 10
  seed = 2134
  
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
  
  result = transform_bs(xx_scaled,df=7)
  
  n = 15
  set.seed(1234)
  clust = pam(result,n)
  centers_index = clust$id.med
  centers_scale = result[centers_index,]
  labels = clust$clustering
  
  centers = xx[centers_index,]
  
 windows(3000,3000)
 x = 1:15
 x = as.vector(scale(x))
 ht <- seq(min(x), max(x), length.out = 225)
 p =par(mfrow=c(3,5))
 for(i in 1:n)
 {
   plot(ht,centers_scale[i,])
 }

    points_result = strategy_test(xx,centers,centers_scale,predict_point=9,threshold=0.5,stopratio =5,
                             profitratio = 5)
    
  profit = points_result$profit
  sum(profit)
  length(profit)
  length(profit[profit>0])/length(profit)
  aggregate(profit,by = list(points_result$center),sum)
  
  aggregate(profit,by = list(points_result$center),function(x){return(c(length(x[x>0])/length(x)))})
  aggregate(profit,by = list(points_result$center),function(x){return(length(x))})
  
}


