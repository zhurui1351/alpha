require('RCurl')
require('rjson')
require('xts')
rawdata_jason  = getURL('http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesMiniKLine15m?symbol=C1705')
rawdata = fromJSON(rawdata_jason)
data = do.call("rbind",args=rawdata)
data = as.data.frame(data)
colnames(data) = c('DateTime','Open','High','Low','Close','Oi')
pricedata = as.data.frame(apply(data[,2:6],2,as.numeric))

pricedata = xts(pricedata,order.by = as.POSIXct(data$DateTime))

date = as.character(Sys.Date())
x = pricedata[date]

y = diff(x$Close)
y[1] = (x[1,]$Close -  x[1,]$Open)
v = as.numeric(y)
