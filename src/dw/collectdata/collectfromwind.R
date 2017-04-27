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
  
  
  v = as.numeric(Cl(pricedata))
  length(v)
  sd(v)
  hlc = HLC(pricedata)
  ATR(hlc,n=5)
  x = 1:15
  x = as.vector(scale(x))
  ht <- seq(min(x), max(x), length.out = 225)
  
  v_scale = scale(v[1:9])
  
  predict_center(v,centers,centers_scale,k=9,isplot = T)
  predict_center_svm(m,v,9,T)
  return(pricedata)
}



#w.stop()
