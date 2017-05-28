basic_analysis = function(records)
{
  profit = ifelse(records$type == 'long',(records$close - records$open),(records$open - records$close))
  win_ratio = length(profit[profit>0])/length(profit)
  print(sum(profit))
  print(win_ratio)
}