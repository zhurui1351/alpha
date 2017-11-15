dates = as.character(unique(as.Date(index(orgin_data))))
nums=c()
for(day in dates)
{
  x = orgin_data[day]
  nums = c(nums,nrow(x))
}

unique(nums)

d1 = substring(as.character(index(orgin_data['2014-12-30'])),12,19)
d2 = substring(as.character(index(orgin_data['2014-12-29'])),12,19)

