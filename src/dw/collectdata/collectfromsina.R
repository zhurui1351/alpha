require('RCurl')
require('rjson')
rawdata_jason  = getURL('http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesMiniKLine15m?symbol=C0')
rawdata = fromJSON(rawdata_jason)
data = do.call("rbind",args=rawdata)
data = as.data.frame(data)
colnames(data) = c('DateTime','Open','High','Low','Close','Oi')
pricedata = as.data.frame(apply(data[,2:6],2,as.numeric))

pricedata = xts(pricedata,order.by = as.POSIXct(data$DateTime))


x = pricedata['2016-12-26']

y = Delt(x$Close)
y[1] = (x[1,]$Close -  x[1,]$Open)/ x[1,]$Open
