china_future_info_db = 'china_future_info'
china_future_ods_day = 'china_future_ods_day'
china_future_ods_m = 'china_future_ods_m'

#大连交易所
#豆一
soybean_a = list(contract = c('dlax01','dlax03','dlax05','dlax07','dlax09','dlax11',
        'dlay01','dlay03','dlay05','dlay07','dlay09','dlay11'),main='dlami',symbol='dla')
#玉米
corp = list(contract = c('dlc01','dlc03','dlc05','dlc07','dlc09','dlc11'),main='dlcmi',symbol='dlc')
#豆二
soybean_b = list(contract=c('dlb01','dlb03','dlb05','dlb07','dlb09','dlb11'),main='dlbmi',symbol='dlb')


china_future_symbols = list('corp'=corp,'soybean_a'=soybean_a,'soybean_b'=soybean_b)
