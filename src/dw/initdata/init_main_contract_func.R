source('src/config/include.R',encoding='utf-8')
#dbinfo = 'china_future_info'
#db = 'china_future_ods_day'
#tbname = 'dlcmi'
#tbname = 'dlami'
#tbnameinfo = 'dlc'
#tbnameinfo = 'dla'
#tbs = c('dlc01','dlc03','dlc05','dlc07','dlc09','dlc11')
#tbs = c('dlax01','dlax03','dlax05','dlax07','dlax09','dlax11',
#        'dlay01','dlay03','dlay05','dlay07','dlay09','dlay11')
init_main_contract_day = function(tbs,infodb,db,tbnameinfo,tbname)
{  
  #获取单个合约信息
  for(tb in tbs)
  {
    data = getTableData(db,tb)
    data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.Date(data$datetime))
    assign(tb,data)
  }
  #获取所有的日期
  alldays = sapply(tbs,function(x){m=get(x);return(index(m))})
  alldays = as.character(unique(alldays[[1]]))
  contracts_info = c()
  #main_price = xts()
  #拼接主力合约每天所用的具体合约
  for(day in alldays)
  {
    #获取每天每个合约的持仓量
    dayinfo = lapply(tbs,function(x,day){m=get(x)
                                             info = m[day]
                                             oi = ifelse(nrow(info)==0,0,info$Oi)
                                             return(c(x,oi))},day)
    #按持仓量进行排序
    dayinfo = dayinfo[order(sapply(dayinfo,function(x){as.numeric(x[2])}),decreasing = T)]
    day_contract = dayinfo[[1]][1]
    contract_data = get(day_contract)
    contract_data = contract_data[day]
    contracts_info = c(contracts_info,day_contract)
  }
  contracts_info = lag(contracts_info)
  contracts_info[1] = contracts_info[2]
  main_info =data.frame(date = alldays,contract = contracts_info)
  for(i in 1:nrow(main_info))
  {
    dayinfo = main_info[i,]
    day = as.character(dayinfo$date)
    day_contract = as.character(dayinfo$contract)
    contract_data = get(day_contract)
    contract_data = contract_data[day]
    if(!exists('main_price'))
    {
      main_price = contract_data
    }
    else
    {
      main_price = rbind(main_price,contract_data)
    }
  }
  main_price = as.data.frame(main_price)
  main_price$datetime = rownames(main_price)
  
  writeToMysqltable(main_price,db,tbname,overwrite = T)
  writeToMysqltable(main_info,infodb,tbnameinfo,overwrite = T)
  
  rm('main_price')
}

init_main_force_m = function(tbs,infodb,db,infotb,tbname)
{
  #获取单个合约信息
  for(tb in tbs)
  {
    data = getTableData(db,tb)
    data = xts(data[,c('Open','High','Low','Close','Vol','Oi')],order.by=as.POSIXct(data$datetime,format='%Y-%m-%d %H:%M:%S'))
    assign(tb,data)
  }
  
  main_info = getTableData(infodb,infotb)
  day = main_info[1,]$date
  contract = main_info[1,]$contract
  
  start = paste(day,'09:00:00',sep=' ')
  end = paste(day,'15:00:00',sep=' ')
  period = paste(start,end,sep='/')
  
  contract_data = get(contract)
  main_price = contract_data[period]
   
  for(i in 2:nrow(main_info))
  {
    day = main_info[i,]$date
    preday = main_info[i-1,]$date
    start = paste(preday,'21:00:00',sep=' ')
    end = paste(day,'15:00:00',sep=' ')
    period = paste(start,end,sep='/')
    
    contract = main_info[i,]$contract
    contract_data = get(contract)
    contract_data = contract_data[period]
    main_price = rbind(main_price,contract_data)
    
  }
  main_price = as.data.frame(main_price)
  main_price$datetime = rownames(main_price)
  writeToMysqltable(main_price,db,tbname,overwrite = T)
  
}