dbname ='china_future_ods_m'
tbname = 'dlami'
data = getdata(dbname,tbname,1,T)

times = as.character(data$datetime)
dup = duplicated(times)
dup = which(dup)
data[dup,]

chgdata = data[-dup,]

#writeToMysqltable(data,dbname,tbname,T)
