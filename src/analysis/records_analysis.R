basic_analysis = function(records)
{
  profit = ifelse(records$type == 'long',(records$close - records$open),(records$open - records$close))
  records$profit = profit
  win_ratio = length(profit[profit>0])/length(profit)
  years = substring(records$opentime,1,4)
  anual_profit = aggregate(records$profit,by=list(years),sum)
  aggregate(records$profit,by=list(years),length)
  print(sum(profit))
  print(win_ratio)
  print(anual_profit)
}