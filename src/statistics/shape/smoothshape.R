#ang = atan(lm_predict$coefficients[2])/pi * 180
#deriv_n = (predict(fm_predict, data.frame(x = max(x)+0.001)) - predict(fm_predict, data.frame(x = max(x))))/0.001


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




within = function(value,limit,ratio)
{
  return(value<=((1+ratio) * limit) && value>= ((1-ratio)*limit))
}

is_HS = function(indices,extrem,y,ratio = 0.2)
{
  #ratio = 1.2
  len = length(indices)
  if(len<5) return(F)
  indices_sub = indices[(len-4):len]
  e = extrem[indices_sub]
  if(e[1] != 'maxmum') return(F)
  E = y[indices_sub]
  m15 = mean(E[c(1,5)])
  m24 = mean(E[c(2,4)])
  
  cond15 =within(E[1],m15,ratio) && within(E[5],m15,ratio)
  cond24 =within(E[2],m24,ratio) && within(E[4],m24,ratio)
  
  cond = E[3]>E[1] && E[3]>E[5]
  return(cond15 && cond24 && cond)
}

derive = function(v,fm,plus=0.0001)
{
  deriv = (predict(fm,data.frame(x=(v+plus)))-predict(fm,data.frame(x=v)))/plus
  return(sign(deriv))
}

flag_extrem = function(ds)
{
  i = 1
  flags = c()
  len = length(ds) - 1
  while(i <= len)
  {
    v1 = ds[i]
    v2 = ds[i+1]
    if(v1 == 1 && v2 == -1 )
    {
      flags[i] = 'maxmum'
    }
    else if(v1 == -1 && v2 == 1)
    {
      flags[i] = 'minum'
    }
    else if(v1 == 0)
    {
      j = ifelse(i==1,0,(i-1))
      while(ds[i] == 0 && i<=len)
      {
        flags[i] = 'norm'
        i = i +1
      }
      if(j != 0)
      {
        
        m1 = ds[j]
        m2 = ds[i]
        if(m1 ==-1 && m2 ==1)
          flags[(j+1)] = 'minum'
        else if(m1 ==1 && m2 ==-1)
          flags[(j+1)] = 'maxmum'
      }
      next
    }
    else
    {
      flags[i] = 'norm'
    }
    i = i + 1
  }
  return(flags)
}


allcl = Cl(data)
result = c()
allcl = Cl(sh)
n = nrow(allcl)
nsample = 30
x = 1: nsample
df = 10
for( i in (nsample+1):n)
{
  y = as.numeric(allcl[(i-nsample+1):i])
  fm = lm(y ~ bs(x, df = df))
  y_predict = predict(fm,data.frame(x=x)) 
  
  derives = sapply(x,derive,fm)
  extrem = flag_extrem(derives)
  indices = which(extrem != 'norm')
  #plot(y)
  #lines(y_predict)
  
  if(is_HS(indices,extrem,y_predict,ratio=0.02) && indices[length(indices)]==(nsample-1))
  {
    print('found ')
    print(i)
    result = c(result,i)
    
  }
}

result_bak = result
pre = result[1]
result = c(pre)
for(i in 2:length(result))
{
  cur = result[i]
  if((cur-pre)>2)
    result = c(result,cur)
  
  pre = cur
}

measure = c()
for(j in 1:length(result))
{
  i = result[j]
  start = i+nsample
  y = as.numeric(allcl[(i-nsample+1):i])
  fm = lm(y ~ bs(x, df = df))
  y_predict = predict(fm,data.frame(x=x)) 
  
  derives = sapply(x,derive,fm)
  extrem = flag_extrem(derives)
  indices = which(extrem != 'norm')
  #plot(y)
  #lines(y_predict)
  tmp = tryCatch(
  {
    as.numeric(allcl[start+3,])-as.numeric(allcl[start,])
  },error = function(e) e
    )
  if(inherits(tmp, "error")) next
  measure[j] = tmp
}
