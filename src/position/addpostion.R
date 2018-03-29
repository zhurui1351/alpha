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


getStopPrice = function(enterprice,atr,addin=0,n=50)
{
  stop_atr = atr
  price = enterprice
  prewin = 0
  test_profit = data.frame(price=price,hand=1,balanceprice=price,prewin=0)
  allprices = c(enterprice)
  for(i in 1:50)
  {
    price = price + ifelse(i==1,2*atr + addin,atr)
    allprices = c(allprices,price)
    meanprice = trunc(mean(allprices)) + 1
    hand = i+1
    stopprice = meanprice#price - hand*stop_atr
    #loss = enter - stopprice
    prewin = prewin + (hand-1)*ifelse(i==1,2*atr + addin,atr)
    r = data.frame(price=price,hand=hand,stopprice=stopprice,prewin=prewin)
    test_profit = rbind(test_profit,r)
  }
  print(test_profit)
}

getStopPriceLimitHand = function(enterprice,atr,hands=3,addin=0,n=50)
{
  stop_atr = atr
  price = enterprice
  prewin = 0
  test_profit = data.frame(price=price,hand=1,balanceprice=price,prewin=0)
  allprices = c(enterprice)
  for(i in 1:n)
  {
    if(i <= (hands-1))
    {
      price = price + ifelse(i==1,2*atr + addin,atr)
      allprices = c(allprices,price)
      meanprice = trunc(mean(allprices)) + 1
      hand = i+1
      balanceprice = meanprice 
      
      prewin = prewin + (hand-1)*ifelse(i==1,2*atr + addin,atr)
      r = data.frame(price=price,hand=hand,balanceprice=balanceprice,prewin=prewin)
      test_profit = rbind(test_profit,r)
    }
    else
    {
      price = price + ifelse(i==1,2*atr + addin,atr)
      #meanprice = meanprice
      #hand = hand
      prewin = prewin + hand*atr
      r = data.frame(price=price,hand=hand,balanceprice=balanceprice,prewin=prewin)
      test_profit = rbind(test_profit,r)
    }
    
  }
  print(test_profit)
}

getShortAndLongBalance = function(shortprice,longprice,price,shortstopprice,longstopprice,atr=15)
{
  
}

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

hands = (3795-3677)/4

getStopPriceAddLoss(enterprice= 6540,atr=15,hands=10,loss=20,n=25,type='long')

