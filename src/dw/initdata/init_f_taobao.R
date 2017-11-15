source('src/config/include.R',encoding='utf-8')
source('src/config/db_config.R',encoding='utf-8')
source('src/dw/collectdata/collectfromtaobao.R',encoding='utf-8')
mydb = 'china_future_ods_m'
mydb_day = 'china_future_ods_day'
search_path = 'D:/BaiduYunDownload/data'
overwrite = T
night_time_dl = list(list(startdate='201401',enddate='20150507',starttime='21:01:00',endtime='02:30:00'),
                     list(startdate='20150508',enddate='',starttime='21:01:00',endtime='23:30:00'))

night_time_sq_menta = list(list(startdate='20131220',enddate='',starttime='21:01:00',endtime='01:00:00'))

corp = list(c('dlc01','DLC01.*'),c('dlc03','DLC03.*'),c('dlc05','DLC05.*'),c('dlc07','DLC07.*'),c('dlc09','DLC09.*'),c('dlc11','DLC11.*'))
soybean_a = list(list(c('dlax01','DLAX01.*'),c('dlax03','DLAX03.*'),c('dlax05','DLAX05.*'),c('dlax07','DLAX07.*'),c('dlax09','DLAX09.*'),c('dlax11','DLAX11.*'),
               c('dlay01','DLAY01.*'),c('dlay03','DLAY03.*'),c('dlay05','DLAY05.*'),c('dlay07','DLAY07.*'),c('dlay09','DLAY09.*'),c('dlay11','DLAY11.*')
               ),night_time = night_time_dl)

soybean_b = list(c('dlb01','DLb01.*'),c('dlb03','DLb03.*'),c('dlb05','DLb05.*'),c('dlb07','DLb07.*'),c('dlb09','DLb09.*'),c('dlb11','DLb11.*'),night_time = night_time_dl)
#豆粕
soybean_meal = list(c('dlm01','DLm01.*'),c('dlm03','DLm03.*'),c('dlm05','DLm05.*'),c('dlm06','DLm06.*'),c('dlm07','DLm07.*'),c('dlm08','DLm08.*'),c('dlm09','DLm09.*'),c('dlm11','DLm11.*'),c('dlm12','DLm12.*'),night_time = night_time_dl)
#豆油
soybean_oil = list(c('dly01','DLY01.*'),c('dly03','DLY03.*'),c('dly05','DLY05.*'),c('dly07','DLY07.*'),c('dly08','DLY08.*'),c('dly09','DLY09.*'),c('dly11','DLY11.*'),c('dly12','DLY12.*'),night_time = night_time_dl)

#螺纹钢
rb_steel = list(list(c('sqrb01','SQRB01.*'),c('sqrb02','SQRB02.*'),c('sqrb03','SQRB03.*'),c('sqrb04','SQRB04.*'),c('sqrb05','SQRB05.*'),c('sqrb06','SQRB06.*'),c('sqrb07','SQRB07.*')
                     ,c('sqrb08','SQRB08.*'),c('sqrb09','SQRB09.*'),c('sqrb10','SQRB10.*'),c('sqrb11','SQRB11.*'),c('sqrb12','SQRB12.*')),
                night_time = night_time_sq_menta)
#鸡蛋
jd = list(list(c('dljd01','DLJD01.*'),c('dljd02','DLJD02.*'),c('dljd03','DLJD03.*'),c('dljd04','DLJD04.*'),c('dljd05','DLJD05.*'),c('dljd06','DLJD06.*'),
               c('dljd09','DLJD09.*'),c('dljd10','DLJD10.*'),c('dljd11','DLJD11.*'),c('dljd12','DLJD12.*')),
          night_time = NULL)
#棕榈油
paml_oil = list(list(c('dlp01','DLP01.*'),c('dlp02','DLP02.*'),c('dlp03','DLP03.*'),c('dlp04','DLP04.*'),c('dlp05','DLP05.*'),c('dlp06','DLP06.*'),c('dlp07','DLP07.*'),c('dlp08','DLP08.*')
                     ,c('dlp09','DLP09.*'),c('dlp10','DLP10.*'),c('dlp11','DLP11.*'),c('dlp12','DLP12.*')),night_time = night_time_dl)

symbols = list(paml_oil,jd)
for(i in 1:length(symbols))
{
  syms = symbols[[i]]
  coms = syms[[1]]
  night_time = syms[[2]]
  for(com in coms)
  {
    tbname = com[1]
    pattern = com[2]
    print(tbname)
    data = collectdatafromtaobao(search_path,pattern,night_time)
    daydata = to_day(data)
    index(data) = index(data) - 60
    data = as.data.frame(data)
    data$datetime = rownames(data)
    daydata = as.data.frame(daydata)
    daydata$datetime = rownames(daydata)
    writeToMysqltable(data,mydb,tbname,overwrite)
    writeToMysqltable(daydata,mydb_day,tbname,overwrite)
    print('ok')
  }  
}
