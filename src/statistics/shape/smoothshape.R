xx_scaled = apply(xx,MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
xx_scaled = t(xx_scaled)
#bline regression
index = 1:15
x = 1:15
x = as.vector(scale(x))
x = x[index]
nsample =780

y=xx_scaled[nsample,index]
fm = lm(y ~ bs(x, df = 5))
ht <- seq(min(x), max(x), length.out = 200)

plot(x,xx_scaled[nsample,])    
lines(ht, predict(fm, data.frame(x = ht)))

#x = scale(x)
lm = lm(y~x)
lines(ht, predict(lm, data.frame(x = ht)))

index = 1:9
x = x[index]
y=xx_scaled[nsample,index]

fm_predict = lm(y ~ bs(x, df = 5))
fm_predict_value = predict(fm_predict, data.frame(x = x))

ht <- seq(min(x), max(x), length.out = 200)
lines(ht, predict(fm_predict, data.frame(x = ht)))

lm_predict = lm(y~x)
lm_predict_value = predict(lm_predict, data.frame(x = x))

lines(ht, predict(lm_predict, data.frame(x = ht)))

diff_cross = fm_predict_value - lm_predict_value 

ang = atan(lm_predict$coefficients[2])/pi * 180
deriv_n = (predict(fm_predict, data.frame(x = max(x)+0.001)) - predict(fm_predict, data.frame(x = max(x))))/0.001

is_spoonshape(diff_cross)

transform_bs = function(xx_scaled)
{
  result = data.frame()
  ht <- seq(min(x), max(x), length.out = 225)
  x = 1:15
  x = as.vector(scale(x))
  for(i in 1:nrow(xx_scaled))
  {    
    nsample =i   
    y=xx_scaled[nsample,1:15]
    fm = lm(y ~ bs(x, df = 5))
    points = predict(fm, data.frame(x = ht))
    result = rbind(result,points)
  }
}

is_spoonshape = function(points)
{
  l = length(points)
  spoon = all(points[2:(l-1)]<0) & diff_cross[l]>0
  return(spoon)
}

result = c()
for(i in 1 : nrow(xx_scaled))
{
  index = 1:9
  x = 1:15
  x = as.vector(scale(x))
  x = x[index]
  nsample = i 
  y=xx_scaled[nsample,index]
  
  x = x[index]
  y=xx_scaled[nsample,index]
  
  fm_predict = lm(y ~ bs(x, df = 5))
  fm_predict_value = predict(fm_predict, data.frame(x = x))
    
  lm_predict = lm(y~x)
  lm_predict_value = predict(lm_predict, data.frame(x = x))
  
  
  diff_cross = fm_predict_value - lm_predict_value 
  
  if(is_spoonshape(diff_cross))
  {
    result = c(result,i)
  }
}

n = 15
set.seed(1234)
clust = pam(result,n)
centers_index = clust$id.med
centers_scale = result[centers_index,]
labels = clust$clustering

centers = xx[centers_index,]

windows(3000,3000)
p =par(mfrow=c(3,5))

for(i in 1:n)
{
  plot(ht,centers_scale[i,])
}

class_n =  xx_dcast[which(labels == 3),]

days = class_n[,'day']

points = sapply(days,function(d){
  
  day_data = data[d]
  op = as.numeric(day_data[11,]$Open)
  cl = as.numeric(day_data[15,]$Close)
  gap = cl - op
  return(gap)
}
  )