source('src/config/include.R',encoding='utf-8')
source('src/dw/initdata/init_main_contract_func.R',encoding='utf-8')

for(i in 1:length(china_future_symbols))
{
  future = china_future_symbols[[i]]
  contracts = future[['contract']]
  main = future[['main']]
  symbol =  future[['symbol']]
  init_main_contract_day(contracts,china_future_info_db,china_future_ods_day,symbol,main)
  init_main_force_m(contracts,china_future_info_db,china_future_ods_m,symbol,main)
}