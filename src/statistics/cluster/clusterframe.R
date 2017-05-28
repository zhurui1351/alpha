source('src/config/include.R',encoding='utf-8')
sourceDir('src/dw/interface')
source('src/dw/collectdata/collectfromwind.R')
sourceDir('src/algorithm')


#init_data
#feature engineering
#flag
#classify
#backtest
#mento carlo

cluster_framework = function()
{
  #init data
  dbname ='china_future_ods_m'
  tbname = 'dlami'
  freq = 15
  data = getdata(dbname,tbname,freq)
  
  #seperate data set 
  all_year = unique(substring(as.character(index(data)),1,4))
  all_days = unique(substring(as.character(index(data)),1,10))
  test_year = c('2014')
  train_year = setdiff(all_year,test_year)
  
  train_data = data[train_year]
  test_data = if_else(length(test_year) == 0,data[0,],data[test_year])

  #get train modal data in train data
  train_days = unique(substring(as.character(index(train_data)),1,10))
  test_days = setdiff(all_days,train_days)
  
  model_days = sample(train_days,2000)
  test_model_days = setdiff(train_days,model_days)
  
  model_data = train_data[model_days]
  test_model_data = train_data[test_model_days]
  
  #feture engineer
  model_data_dcast = flat_time_data(model_data,diffclose='diffcl',freq=freq)

  
}