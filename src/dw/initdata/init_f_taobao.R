source('src/config/include.R',encoding='utf-8')
source('src/config/db_config.R',encoding='utf-8')
source('src/dw/collectdata/collectfromtaobao.R',encoding='utf-8')
mydb = 'china_future_ods_m'
mydb_day = 'china_future_ods_day'
search_path = 'D:/BaiduYunDownload/data'
overwrite = T

corp = list(c('dlc01','DLC01.*'),c('dlc03','DLC03.*'),c('dlc05','DLC05.*'),c('dlc07','DLC07.*'),c('dlc09','DLC09.*'),c('dlc11','DLC11.*'))
soybean_a = list(c('dlax01','DLAX01.*'),c('dlax03','DLAX03.*'),c('dlax05','DLAX05.*'),c('dlax07','DLAX07.*'),c('dlax09','DLAX09.*'),c('dlax11','DLAX11.*'),
               c('dlay01','DLAY01.*'),c('dlay03','DLAY03.*'),c('dlay05','DLAY05.*'),c('dlay07','DLAY07.*'),c('dlay09','DLAY09.*'),c('dlay11','DLAY11.*'))

soybean_b = list(c('dlb01','DLb01.*'),c('dlb03','DLb03.*'),c('dlb05','DLb05.*'),c('dlb07','DLb07.*'),c('dlb09','DLb09.*'),c('dlb11','DLb11.*'))
#豆粕
soybean_meal = list(c('dlm01','DLm01.*'),c('dlm03','DLm03.*'),c('dlm05','DLm05.*'),c('dlm06','DLm06.*'),c('dlm07','DLm07.*'),c('dlm08','DLm08.*'),c('dlm09','DLm09.*'),c('dlm11','DLm11.*'),c('dlm12','DLm12.*'))
#豆油
soybean_oil = list(c('dly01','DLY01.*'),c('dly03','DLY03.*'),c('dly05','DLY05.*'),c('dly07','DLY07.*'),c('dly08','DLY08.*'),c('dly09','DLY09.*'),c('dly11','DLY11.*'),c('dly12','DLY12.*'))
symbols = list(soybean_a)
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
    index(data) = index(data) - 60
    data = as.data.frame(data)
    data$datetime = rownames(data)
    daydata = as.data.frame(daydata)
    daydata$datetime = rownames(daydata)
    writeToMysqltable(data,mydb,tbname,overwrite)
    writeToMysqltable(daydata,mydb_day,tbname,overwrite)
  }  
}
