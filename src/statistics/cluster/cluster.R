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

plot_centers = function(centers_scale,n=15,k=9)
{
  windows(1000,1000)
  #x = 1:15
  #x = as.vector(scale(x))
  #ht <- seq(min(x), max(x), length.out = 225)
  n_row = round(n/5) + 1
  p =par(mfrow=c(n_row,5))
  
  for(i in 1:n)
  {
    #plot(ht,centers_scale[i,])
    plot(spline(as.numeric(centers_scale[i,])),type='l')
    abline(v=k)
  }
  
  par(p)
  #plot(spline(as.numeric(h)),type='l')
  #windows()
}

plot_clusters = function(xx_scaled,n)
{
  y_max = max(xx)
  y_min = min(xx)
}

flat_time_data = function(data,diffclose='cl',freq=15)
{
  time = as.character(index(data))
  time = substr(time,12,19)
  
  times = get_day_trade_min_series(freq)
  data_open = data[time %in% c('09:00:00'),]
  data_open_votile = (data_open$Close - data_open$Open)
  data_open_votile_log = log(data_open$Close) - log(data_open$Open)
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
  else if(diffclose == 'ln')
  {
    votile = as.data.frame(diff(log(data$Close)))
    votile[time %in% c('09:00:00'),] =log(data_open$Close) - log(data_open$Open)
  }
  colnames(votile) = 'change'
  votile$time = time
  votile$day = as.character(as.Date(rownames(votile)))
  votile = subset(votile,time %in% times)
  xx_dcast = dcast(votile,day ~ time,value.var='change')
  
  if(diffclose == 'cl')
  {
    xx_dcast$Open = data_open$Open
    colnames = colnames(xx_dcast)
    colindex =c(1,length(colnames),2:(length(colnames)-1))
    xx_dcast = xx_dcast[,colindex]
  }

  return(xx_dcast)
  
}

correct_ratio = function(label,prlabel)
{
  tb = table(prlabel,label)
  tbratio = apply(tb,1,function(x){x/sum(x)})
  ratio = diag(tbratio)
  return(ratio)
}

train_svm = function(xx,labels,k,algorithm=randomForest)
{
  set.seed(1234)
  scaled = apply(xx[,1:k],MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
  scaled = t(scaled)
  ll = as.factor(labels)
  all = data.frame(scaled,ll)
  all = na.omit(all)
  
  ll = all[,ncol(all)]
  scaled = all[,1:(ncol(all)-1)]
  scaled = as.matrix(scaled)
  #m = svm(scaled,ll)
  m = algorithm(scaled,ll)
  l = predict(m)
  #print(sum(l == ll)/length(l))
  #print(correct_ratio(ll,l))
  return(m)
}


predict_center_svm = function(m,v,k=9,isstrategy = F)
{
  v_scale = scale(v[1:k])
  v_scale = as.matrix(t(v_scale))
  
  center = predict(m,v_scale)
  if(isstrategy)
    print(center_strategy(centers_scale,center,k,threshold=0.5))
  return(center)
  
}

predict_center = function(v,centers,k=10,isscalecenter=T,isstrategy = F)
{
 if(isscalecenter)
 {
   sample_centers = apply(centers,MARGIN = 1 ,function(x,k){
     xv = 1:k
     y =scale(x[1:k])
     fm = lm(y ~ bs(xv,df=5))
     return(predict(fm, data.frame(xv = xv)))
     
   },k)
   sample_centers = t(sample_centers)
 }
 else
 {
   sample_centers = centers[,1:k]
 }
  
  
  y1 = as.numeric(scale(v[1:k]))
  
  distance = apply(sample_centers,MARGIN = 1 ,function(x,v){dist(rbind(x,v))},y1)
  min_index = which.min(distance)
  #print(min_index) 
 if(isstrategy)
   print(center_strategy(centers,min_index,k,threshold=0.5))
 
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
  
  numcol = ncol(centers)
  prlabels = apply(xx,MARGIN = 1,predict_center,centers,centers_scale,k=12)
  
 # table(ll,prlabels)
  
  return(list(dt,dt_sep))  
  
}

transform_bs = function(xx_scaled,interpolation=1,df=3)
{
  pointnum = ncol(xx_scaled)
  result = data.frame()
  x = 1:pointnum
#  x = as.vector(scale(x))
  totalpoints = interpolation * pointnum
  ht <- seq(min(x), max(x), length.out = totalpoints)
  for(i in 1:nrow(xx_scaled))
  {    
    nsample =i   
    y=xx_scaled[nsample,1:pointnum]
    fm = lm(y ~ bs(x, df = df))
    points = predict(fm, data.frame(x = ht))
    result = rbind(result,points)
  }
  return(result)
}

spline_vector = function(v,interpolation=1,df=5)
{
  y = as.numeric(v)
  pointnum = length(y)
  x = 1:pointnum
  totalpoints = interpolation * pointnum
  ht <- seq(min(x), max(x), length.out = totalpoints)
  fm = lm(y ~ bs(x, df = df))
  points = predict(fm, data.frame(x = ht))
  return(points)
}

center_strategy = function(centers_scale,i,k,threshold=0.5)
{
  strategy = 'normal'
  #x = 1:15
  #x = as.vector(scale(x))
  #ht <- seq(min(x), max(x), length.out = 225)
  #piindex = max(which(ht<=x[k]))
  piindex = k
  ratio = centers_scale[i,ncol(centers_scale)]-centers_scale[i,piindex]
  if(ratio > threshold)
    strategy = 'long'
  else if(ratio < -threshold)
    strategy = 'short'
  else
    strategy = 'normal'
  return(strategy)    
}

strategy_test = function(xx_dcast,method='svm',isspline=F,xx,centers,predict_point=9,threshold=0.5,m,stopratio =2,
                         profitratio = 1,df=3,isscalecenter=T)
{
  #print(nrow(xx_dcast))
  points_result = data.frame()
  for(i in 1:nrow(xx))
  {
    
    start_point = predict_point + 1
    v = as.numeric(xx[i,])
    if(isspline)
    {
      v = spline_vector(v[1:predict_point],df=df)
      
    }
    day = xx_dcast[i,]$day  
    if(method=='dist')
    {
      tmp = tryCatch(predict_center(v,centers,predict_point,isscalecenter=T,F),error=function(e) e)      
    }
    else if(method=='svm')
    {
      tmp = tryCatch(predict_center_svm(m,v,predict_point),error=function(e) e)
    }   
    
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
    r = data.frame(i=i,day=day,center = center_pr,open=open,op=op,hi = hi,low=low,close=cl,profit=profit,var=sd,strategy=strategy,type=type)
    points_result = rbind(points_result,r)
  }
  return(points_result)
}

stat_orgin_centers = function(xx_dcast,centers_scale,labels,k=9,n=15,threshold=0.5)
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

trading_result_analysis = function(points_result)
{
  profit = points_result$profit
  totalprofit = sum(profit)
  totallen = length(profit)
  winratio = length(profit[profit>0])/length(profit)
  result_total = data.frame(profit = totalprofit,len=totallen,winratio=winratio)
  
  center_profit = aggregate(profit,by = list(points_result$center),sum)
  colnames(center_profit) = c('id','profit')
  center_win_ratio = aggregate(profit,by = list(points_result$center),function(x){return(c(length(x[x>0])/length(x)))})
  colnames(center_win_ratio) = c('id','winratio')
  center_len = aggregate(profit,by = list(points_result$center),function(x){return(length(x))})
  colnames(center_len) = c('id','len')
  result = merge(merge(center_profit,center_win_ratio),center_len)
  
  result$ptest = apply(result,1,function(x){p = prop.test(as.numeric(x['winratio']) * as.numeric(x['len']),as.numeric(x['len']),0.55,alternative='greater')
                               return(p$p.value)})
  
  return(list(result_total,result[order(result$id),]))
}


common_centers = function(center_set,nclust=3,isscaled=T,algo = 'kmeans')
{
  set.seed(1234)
  if(isscaled)
  {
    set_scaled = apply(center_set,MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
    set_scaled = t(set_scaled)
  }
  else
  {
    set_scaled = center_set
  }
    
  
  if(algo=='kmeans')
  {
    clust = kmeans(train_xx,nclust,iter.max = 1000)
    centers = clust$centers
  }
  else if(algo=='pam')
  {
    clust = pam(set_scaled,nclust)
    centers_index = clust$id.med
    centers_scale = set_scaled[centers_index,]
    labels = clust$clustering
    centers = set_scaled[centers_index,]
  }

  plot_centers(centers,nclust) 
  return(centers)
}

filter_centers = function(com_centers,centers,threshold=0.95)
{
  result = c()
  for(i in 1:nrow(centers))
  {
    center = centers[i,]
    sim = apply(com_centers,MARGIN = 1,function(v,center){return(cor(as.numeric(v),as.numeric(center)))},center)
    l = length(which(sim >= threshold))
    if(l > 0)
    {
      result = c(result,i)
    }
  }
  return(result)
}

filter_invalid_data = function(xx_dcast)
{
  xx = xx_dcast[,2:ncol(xx_dcast)] 
  
  indices = which(apply(xx,MARGIN=1,function(x)(all(as.numeric(x)==x[1]))))
  if(length(indices) > 0 )
  {
    xx_dcast =xx_dcast[-indices,]
  }
  return(xx_dcast)
}

if_else = function(cond,v1,v2)
{
  if(cond)
    return(v1)
  else
    return(v2)
}

scale_data = function(xx,func = scale)#identity返回
{
  xx_scaled = apply(xx,MARGIN=1,function(x){return(rbind(as.numeric(func(as.numeric(x)))))})
  xx_scaled = t(xx_scaled)
  return(xx_scaled)
}


check_invalid_days = function(xx_dcast)
{
    days = substring(xx_dcast$day,1,4)
    return(aggregate(days,by=list(days),length))
}

run =function()
{
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  data = getdata(dbname,tbname,freq)
  
  all_year = unique(substring(as.character(index(data)),1,4))
  test_year = c('2014')
  train_year = setdiff(all_year,test_year)
  
  train_data = data[train_year]
  test_data = if_else(length(test_year) == 0,data[0,],data[test_year])
  
  ########
  train_xx_dcast = flat_time_data(train_data,diffclose='diffcl',freq=freq)
  train_xx_dcast = filter_invalid_data(train_xx_dcast)
  train_xx = train_xx_dcast[,2:ncol(train_xx_dcast)] 
  
  train_xx_dcast_center = flat_time_data(train_data,diffclose='cl',freq=freq)
  train_xx_dcast_center = filter_invalid_data(train_xx_dcast_center)
  train_xx_center = train_xx_dcast_center[,2:ncol(train_xx_dcast_center)] 
  
  #check_invalid_days(train_xx_dcast)
  
  train_xx_scaled = scale_data(train_xx,func = scale)
  train_xx_center_scaled = scale_data(train_xx_center,func = scale)
  
  result = transform_bs(train_xx_scaled,interpolation=1,df=3)
  
  center_set = data.frame()
  
  nsampe = nrow(train_xx_scaled)
  
  n = 15
  
  turns = 100
  
 # pre_samples = 1:2000
  
  n_satisfied = c()
  for(i in 1:turns)
  {
    print(i)
    samples = sample(1:nsampe,2000)
    #set.seed(1234)  
    #print(length(intersect(pre_samples,samples)))
    #pre_samples = samples
    
    train_xx_for_train = train_xx_center[samples,]
    train_xx_for_train_scaled = scale_data(train_xx_for_train,func = scale)
    
    set.seed(1234)
    clust = kmeans(train_xx_for_train_scaled,n,iter.max = 1000)
    centers = clust$centers
    centers_scale = clust$centers
    labels = clust$cluster
    
    predict_point = 10
    #plot_centers(centers_scale,n=n,k=predict_point)
    
    train_yy_for_train = labels
    train_xx_dcast_for_train = train_xx_dcast[samples,]
    
    
    m = train_svm(train_xx_dcast_for_train,train_yy_for_train,k=predict_point,algorithm=randomForest)
    
    #stat_orgin_centers(train_xx_dcast_for_train,centers,labels,k=predict_point,n=n,threshold=0.5)
    
    test_xx_for_train = train_xx[-samples,]
    test_xx_dcast_for_train = train_xx_dcast[-samples,]
    points_result = strategy_test(test_xx_dcast_for_train,method='svm',isspline=F,test_xx_for_train,centers,predict_point=predict_point,threshold=0.3,m=m,stopratio =5,
                                  profitratio = 5,df=3,isscalecenter=F)
    
    trading_result = trading_result_analysis(points_result)
    center_result = trading_result[[2]]
    satisfied_center = subset(center_result,winratio>0.55 & len>50 )
    if(nrow(satisfied_center) == 0) 
    {
      print('fault')
      next
    }
    centers = as.data.frame(centers)
    satisfied_center = centers[satisfied_center$id,]
    n_satisfied = c(n_satisfied,nrow(satisfied_center))
    colnames(satisfied_center) = 1:ncol(centers)
    center_set = rbind(center_set,satisfied_center)
  }
  com_centers = common_centers(center_set,nclust=3,algo='pam')
  
  
  #get best train center
  set.seed(1234)
  clust_train = kmeans(train_xx_scaled,n,iter.max = 1000)
  centers_train = clust_train$centers
  labels_train = clust_train$cluster
    
  train_yy = labels_train  
  train_m = train_svm(train_xx,train_yy,k=predict_point,algorithm=randomForest)
  
  train_centers = filter_centers(com_centers,centers_train,threshold=0.85)
  #test
  xx_dcast_for_test = flat_time_data(test_data,diffclose='cl',freq=freq)
  xx_dcast_for_test = filter_invalid_data(xx_dcast_for_test)
  xx_for_test = xx_dcast_for_test[,2:ncol(xx_dcast_for_test)] 
  
  points_result_test = strategy_test(xx_dcast_for_test,method='svm',isspline=F,xx_for_test,centers_train,predict_point=predict_point,threshold=0.3,m=train_m,stopratio =5,
                                profitratio = 5,df=3,isscalecenter=F)
  
  
  trading_result_test = trading_result_analysis(points_result_test)
  
}



