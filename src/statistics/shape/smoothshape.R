xx_scaled = apply(xx,MARGIN=1,function(x){return(rbind(as.numeric(scale(as.numeric(x)))))})
xx_scaled = t(xx_scaled)
#bline regression
index = 1:15
x = 1:15
x = as.vector(scale(x))
x = x[index]
nsample =381

y=xx_scaled[nsample,index]
fm = lm(y ~ bs(x, df = 5))
ht <- seq(min(x), max(x), length.out = 200)

plot(x,xx_scaled[nsample,])    
lines(ht, predict(fm, data.frame(x = ht)))

#x = scale(x)
lm = lm(y~x)
lines(ht, predict(lm, data.frame(x = ht)))

index = 1:10
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
deriv_n = (predict(fm1, data.frame(x = max(x)+0.001)) - predict(fm1, data.frame(x = max(x))))/0.001
