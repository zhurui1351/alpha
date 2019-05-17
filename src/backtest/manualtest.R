get_long_price = function(enterprice,r,init_n=1,first_profit_n=2,sep_n=1,fee=1)
{
  result = data.frame()
  
  no_loss = enter_price + init_n*r
  first_profit_price = enter_price+(sep_n+first_profit_n)*r
  first_profit = first_profit_n*r
  out_price = enter_price+first_profit
  
  row = data.frame(price = no_loss,profit = 0,outprice = enter_price+fee)
  result = rbind(result,row)
  row = data.frame(price = first_profit_price,profit= first_profit,outprice = out_price+fee)
  result = rbind(result,row)
  
  new_price = first_profit_price
  new_profit = first_profit
  
  for(i in 1:10)
  {
    new_price = new_price + sep_n*r
    new_profit = new_profit + sep_n*r
    out_price = out_price+sep_n*r
    row = data.frame(price=new_price,profit=new_profit,outprice=out_price+fee)
    result = rbind(result,row)
  }
  return(result)
}

get_short_price = function(enterprice,r,init_n=1,first_profit_n=2,sep_n=1,fee=1)
{
  result = data.frame()
  
  no_loss = enter_price - init_n*r
  first_profit_price = enter_price-(sep_n+first_profit_n)*r
  first_profit = first_profit_n*r
  out_price = enter_price-first_profit
  
  row = data.frame(price = no_loss,profit = 0,outprice = enter_price-fee)
  result = rbind(result,row)
  row = data.frame(price = first_profit_price,profit= first_profit,outprice = out_price-fee)
  result = rbind(result,row)
  
  new_price = first_profit_price
  new_profit = first_profit
  
  for(i in 1:10)
  {
    new_price = new_price - sep_n*r
    new_profit = new_profit + sep_n*r
    out_price = out_price-sep_n*r
    row = data.frame(price=new_price,profit=new_profit,outprice=out_price-fee)
    result = rbind(result,row)
  }
  return(result)
}

enter_price = 3000
stop_price = 2900
r = enter_price - stop_price
get_long_price(enter_price,r)
get_short_price(enter_price,r)
