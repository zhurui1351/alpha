# enter = 3000
# atr = 6
# n = 50
# stop_atr = atr
# price = enter
# prewin = 0
# test_profit = data.frame()
# allprices = c(3000)
# for(i in 1:50)
# {
#   price = price + ifelse(i==1,2*atr,atr)
#   allprices = c(allprices,price)
#   meanprice = trunc(mean(allprices)) + 1
#   hand = i+1
#   stopprice = meanprice#price - hand*stop_atr
#   #loss = enter - stopprice
#   prewin = prewin + (hand-1)*ifelse(i==1,2*atr,atr)
#   r = data.frame(price=price,hand=hand,stopprice=stopprice,prewin=prewin)
#   test_profit = rbind(test_profit,r)
# }
# test_profit$prewin = test_profit$prewin / atr
# test_profit$price = (test_profit$price - enter) / atr



#固定点位加仓，计算加仓位置、均价、在接收的损失下的出场位置
getStopPriceAddLoss = function(enterprice,atr,hands=3,loss=15,n=50,type='long')
{
    

  stop_atr = atr
  price = enterprice
  prewin = 0
  test_profit = data.frame(price=price,hand=1,meanprice=price,balanceprice=price-loss,prewin=0)
  allprices = c(enterprice)
  for(i in 1:n)
  {
    if(i <= (hands-1))
    {
      price = ifelse(type=='long',price + atr,price - atr)
      allprices = c(allprices,price)
      meanprice = trunc(mean(allprices)) + ifelse(type=='long',-1,1)
      hand = i+1
      balanceprice = ifelse(type=='long',trunc(meanprice - loss/hand) -1,trunc(meanprice + loss/hand) + 1) 
      
      prewin = prewin + (hand-1)*atr
      r = data.frame(price=price,hand=hand,meanprice=meanprice,balanceprice=balanceprice,prewin=prewin)
      test_profit = rbind(test_profit,r)
    }
    else
    {
      price = ifelse(type=='long',price + atr,price - atr)
      #meanprice = meanprice
      #hand = hand
      prewin = prewin + hand*atr
      r = data.frame(price=price,hand=hand,meanprice=meanprice,balanceprice=balanceprice,prewin=prewin)
      test_profit = rbind(test_profit,r)
    }
    
  }
  print(test_profit)
}


getStopPriceAddLoss(enterprice= 100,atr=20,hands=10 ,loss=20,n=10,type='long')


#目的:加仓后，反弹到阻力位置x个atr或者指定点位m，保持不亏或者盈利某个程度，那么加仓点位置应该多少
#公式推导见资金管理
getStopPriceFOrPoint = function(meanenter,out,n,profit)
{
  x = out*(n+1) - profit -meanenter*n
  return(x)
}

getStopPriceFOrPoint(3690,4000,1,0)

#在固定加仓下，要达到某一个利润，同时回撤不超过x，行情需要走多远

# n = (x - enter) %/% atr
# profit1 = 1+2+...n = n(n+1)/2
#profit2 = (x - enter) mode atr
# expect_profit = profit1 + profit2
#求解x
#算法设计，寻找每一个加仓阶段的最大利润，然后看x落在哪个区间，进行求解

getPointFOrProfit = function(enterprice,atr,expectedprofit,type = 'long')
{
  n = expectedprofit %/% atr + 1
  dist = getStopPriceAddLoss(enterprice,atr,n,15,n,type)
  dist$profit = (dist$price - dist$meanprice)*dist$hand
  i = findInterval(expectedprofit,dist$profit)
  
  profit1 = dist$profit[i]
  hand = dist$hand[i]
  price = dist$price[i]
  #long: expectedprofit = profit1 + (x - price)*hand 
  #short: expectedprofit = profit1 + (price-x)*hand 
  # (expectedprofit - profit1)/hand + price = x 
  if(type == 'long')
  {
    x = trunc((expectedprofit - profit1)/hand + price) 
  }
  else
  {
    x = trunc(price - (expectedprofit - profit1)/hand)
  }
  return(c(x,hand))
}

getPointFOrProfit(3000,20,100,type='long')


