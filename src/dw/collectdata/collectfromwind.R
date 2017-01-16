require(WindR)
require(quantmod)
w.start()

date = as.character(Sys.Date())
opentime = paste(date,'09:00:00',sep=' ')
closetime = paste(date,'15:15:00',sep=' ')
w_wsi_data<-w.wsi("C1705.DCE","open,high,low,close,volume,amt",opentime,closetime,"BarSize=15")
#查询主力合约
#w_wss_data<-w.wss('C.DCE','trade_code')
wsi_data = w_wsi_data[[2]]
pricedata = xts(wsi_data[,2:7],order.by = as.POSIXct(wsi_data$DATETIME))

startdata = pricedata[1,]
pricedata = pricedata[2:nrow(pricedata),]
index(pricedata) = index(pricedata) - minutes(15)


y = diff(pricedata$close)
#y = pricedata$close - pricedata$open
y[1] = (pricedata[1,]$close - pricedata[1,]$open)
v = as.numeric(y)

w.stop()
