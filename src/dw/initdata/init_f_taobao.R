source('src/config/include.R',encoding='utf-8')
source('src/config/db_config.R',encoding='utf-8')

mydb = 'china_future_ods_m'
mydb_day = 'china_future_ods_day'
search_path = 'D:/BaiduYunDownload/data'
overwrite = T

corp = list(c('dlc01','DLC01.*'),c('dlc03','DLC03.*'),c('dlc05','DLC05.*'),c('dlc07','DLC07.*'),c('dlc09','DLC09.*'),c('dlc11','DLC11.*'))
soybean = list(c('dlax01','DLAX01.*'),c('dlax03','DLAX03.*'),c('dlax05','DLAX05.*'),c('dlax07','DLAX07.*'),c('dlax09','DLAX09.*'),c('dlax11','DLAX11.*'),
               c('dlay01','DLAY01.*'),c('dlay03','DLAY03.*'),c('dlay05','DLAY05.*'),c('dlay07','DLAY07.*'),c('dlay09','DLAY09.*'),c('dlay11','DLAY11.*'))


symbols = list(corp,soybean)
for(i in 1:length(symbols))
{
  coms = symbols[[i]]
  for(com in coms)
  {
    tbname = com[1]
    pattern = com[2]
    print(com[1])
    data = collectdatafromtaobao(search_path,pattern)
    daydata = to_day(data)
    data = as.data.frame(data)
    data$datetime = rownames(data)
    daydata = as.data.frame(daydata)
    daydata$datetime = rownames(daydata)
    writeToMysqltable(data,mydb,tbname,overwrite)
    writeToMysqltable(daydata,mydb_day,tbname,overwrite)
  }  
}
