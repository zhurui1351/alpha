basic_analysis = function(records)
{
  profit = ifelse(records$type == 'long',(records$close - records$open),(records$open - records$close))
  records$profit = profit
  win_ratio = length(profit[profit>0])/length(profit)
  years = substring(records$opentime,1,4)
  anual_profit = aggregate(records$profit,by=list(years),sum)
  colnames(anual_profit) = c('year','profit')
  anual_len = aggregate(records$profit,by=list(years),length)
  colnames(anual_len) = c('year','len')
  anual_ratio = aggregate(records$profit,by=list(years),function(x){length(x[x>0])/length(x)})
  colnames(anual_ratio) = c('year','ratio')
  anual_record = merge(anual_profit,anual_len)
  anual_record = merge(anual_record,anual_ratio)
  #aggregate(records$profit,by=list(years),length)
  anual_record$trueprofit = anual_record$profit - anual_record$len*0.8 - anual_record$len
  anual_record$virtualprofit = anual_record$profit - anual_record$len*0.8 
  print(sum(anual_record$virtualprofit))
  print(nrow(records))
  print(win_ratio)
  print(anual_record)
  print(sum(anual_record$trueprofit))
}