
remove_duplicated = function(p)
{
  ptimes = as.character(index(p))
  #ptimes = substring(ptimes,12,20)
  #去重
  dp = which(duplicated(ptimes))
  if(length(dp)!=0) #有重复数据
  {
    dptimes = unique(ptimes[dp]) #重复时间
    keepindex = c() #保留数据
    delindex = c() #删除的重复数据
    for(dptime in dptimes)
    {
      day = unique(substring(as.character(index(p)),1,10))
      print(paste(day,dptime,sep=' '))
      dpindex = which(ptimes == dptime)
      reserve = min(dpindex)
      keepindex = c(keepindex,reserve)
      delindex = c(delindex,dpindex[-reserve])
    }
    p = p[-delindex]
  }
  return(p)
}


fill_missing_value = function(basetime,pricedata)
{
  datatimes = as.character(unique(index(pricedata)))
  iscrossday = ifelse(is.element('00:01:00',basetime),T,F)
  basedates = as.character(unique(as.Date(index(pricedata))))
  newpricedata = pricedata
  for (day in basedates)
  {
    #print(day)
    starttime = basetime[1]
    endtime = basetime[length(basetime)]
    daystart = paste(day,starttime)
    dayend = ifelse(iscrossday,paste(as.character(as.Date(day) + 1),endtime),paste(day,endtime))
    dayperd = paste(daystart,dayend,sep='/')
    daydata = pricedata[dayperd]
    p = NULL
    if(nrow(daydata) == 0) next
    daytime = substring(as.character(index(daydata)),12,19)
    missingtime = setdiff(basetime,daytime)
    for (mt in missingtime)
    {
      mttime = as.POSIXct(paste(day,mt))
      #print(mttime)
      i = which(basetime == mt)
      while (!is.element(basetime[i],daytime) && i > 0 )
      {
        i = i - 1
      }
      if(i == 0 )
      {
        j = 1
      }else
      {
        j = which(daytime == basetime[i])
      }
      missp = daydata[j]
      index(missp) = mttime
      missp$High = missp$Open
      missp$Low = missp$Open
      missp$High = missp$Open
      missp$Close = missp$Open
      missp$Vol = 0
      p = rbind(p,missp)
    }
    newpricedata = rbind(newpricedata,p)
  }
  return(newpricedata)
}

getbasetime = function(starttime,endtime)
{
  alltimes = as.character(seq(as.POSIXct('2000-01-01 09:01:00'),as.POSIXct('2000-01-02 09:00:00'),by='min'))
  alltimes = substring(alltimes,12,19)
  starti = which(alltimes==starttime)
  endi = which(alltimes == endtime)
  return(alltimes[starti:endi])
}

getbasetime_day = function()
{
  fst = getbasetime('09:01:00','10:15:00')
  sec = getbasetime('10:31:00','11:30:00')
  third = getbasetime('13:31:00','15:00:00')
  times = c(fst,sec,third)
  return(times)
}

getbasetime_night_crossday_dl = function()
{
  start = as.POSIXct('2000-01-01 21:01:00')
  mytimes = c(start)
  for(i in 1:329)
  {
    temp = start + i*60 
    mytimes = c(mytimes,temp)
  }
  return(substring(as.character(mytimes),12))
}

getbasetime_night = function()
{
  start = as.POSIXct('2000-01-01 21:01:00')
  mytimes = c(start)
  for(i in 1:149)
  {
    temp = start + i*60 
    mytimes = c(mytimes,temp)
  }
  return(substring(as.character(mytimes),12))
}

