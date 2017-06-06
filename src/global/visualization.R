visualize_bar = function(data,start='2015-01-01',end='2015-06-01')
{
  
  stock$sma10 = SMA(stock$close,10)
  stock = na.omit(stock)
  dates = as.character(stock$date)
  
  data = as.data.frame(OHLC(stock))
  data = data[,c('open','close','low','high')]
  rownames(data) = NULL
  colnames(data) = NULL
  data = as.matrix(data)
  xx = apply(data,1,as.list)
  
  
  ma = as.numeric(stock$sma10)
  
  option = list(
    backgroundColor = '#21202D',
    legend = list(
      data =  c('日K', 'MA'),
      inactiveColor = '#777',
      textStyle = list(
        color = '#fff'
      )
    ),
    tooltip = list(
      trigger = 'axis',
      axisPointer =  list(
        animation = F,
        type = 'cross',
        lineStyle = list(
          color = '#376df4',
          width = 2,
          opacity = 1
        )
      )
    ),
    xAxis = list(
      type = 'category',
      data = dates,
      axisLine = list( lineStyle = list( color = '#8392A5' ) )
    ),
    yAxis= list(
      scale= T,
      axisLine= list(lineStyle= list( color= '#8392A5' ) ),
      splitLine= list( show= F )
    ),
    grid= list(
      bottom= 80
    ),
    dataZoom= list(list(
      textStyle= list(
        color= '#8392A5'
      ),
      handleIcon= 'M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z',
      handleSize= '80%',
      dataBackground= list(
        areaStyle= list(
          color= '#8392A5'
        ),
        lineStyle= list(
          opacity= 0.8,
          color= '#8392A5'
        )
      ),
      handleStyle= list(
        color= '#fff',
        shadowBlur= 3,
        shadowColor= 'rgba(0, 0, 0, 0.6)',
        shadowOffsetX= 2,
        shadowOffsetY= 2
      )
    ),
    list(type='inside')
    
    ), 
    animation= F,
    series= list(
      list(
        type= 'candlestick',
        name= '日K',
        data= xx,
        itemStyle= list(
          normal= list(
            color= '#FD1050',
            color0= '#0CF49B',
            borderColor= '#FD1050',
            borderColor0= '#0CF49B'
          )
        )
      ),
      list(
        name = 'MA5',
        type = 'line',
        data = ma,
        smooth = T,
        lineStyle = list(
          normal = list(opacity = 0.5)
        )
      )
    )
  )
  echart(option)
}


