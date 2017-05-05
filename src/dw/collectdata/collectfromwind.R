#w.start()
#w.isconnected
getWindData = function(name,tradingplace,opentime,closetime)
{
  
  contract = paste(name,tradingplace,sep='.')
  
  #main_a = w.wss('A.DCE','trade_code')
  main_a = w.wss(contract,'trade_code')
  main_a_trade_code = main_a$Data$TRADE_CODE
  main_a_trade_code = paste(main_a_trade_code,'DCE',sep='.')
  
  w_wsi_data<-w.wsi(main_a_trade_code,"open,high,low,close,volume,amt",opentime,closetime,"BarSize=15")
  #查询主力合约
  return(w_wsi_data) 
}

trading_intraday = function()
{
  if(!w.isconnected())
  {
    w.start()
  }
  
  date = as.character(Sys.Date())
  opentime = paste(date,'09:00:00',sep=' ')
  closetime = paste(date,'15:15:00',sep=' ')
  
  w_wsi_data = getWindData('A','DCE',opentime,closetime)
  
  wsi_data = w_wsi_data[[2]]
  pricedata = xts(wsi_data[,2:7],order.by = as.POSIXct(wsi_data$DATETIME))
  
  startdata = pricedata[1,]
  pricedata = pricedata[2:nrow(pricedata),]
  index(pricedata) = index(pricedata) - minutes(15)
  
  
  v = as.numeric(Cl(pricedata))
  open = as.numeric(Op(pricedata))[1]
  v = c(open,v)
  #length(v)
  #sd(v)
  #hlc = HLC(pricedata)
  #ATR(hlc,n=5)
  v_scale = scale(v[1:10])
  
  predict_center(v,centers,k=10,isscalecenter = F,isstrategy=T)
  predict_center_svm(m,v,10,isstrategy=T)
  
  w.stop()
}

#w.stop()
