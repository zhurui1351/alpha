require(WindR)
require(quantmod)
#w.start()

getWindData = function()
{
  date = as.character(Sys.Date())
  opentime = paste(date,'09:00:00',sep=' ')
  closetime = paste(date,'15:15:00',sep=' ')
  w_wsi_data<-w.wsi("C1709.DCE","open,high,low,close,volume,amt",opentime,closetime,"BarSize=15")
  #查询主力合约
  #w_wss_data<-w.wss('C.DCE','trade_code')
  wsi_data = w_wsi_data[[2]]
  pricedata = xts(wsi_data[,2:7],order.by = as.POSIXct(wsi_data$DATETIME))
  
  startdata = pricedata[1,]
  pricedata = pricedata[2:nrow(pricedata),]
  index(pricedata) = index(pricedata) - minutes(15)
  
  return(pricedata)
}



#w.stop()
