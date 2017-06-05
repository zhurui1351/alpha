visualize_bar = function(data,start='2015-01-01',end='2015-06-01')
{
  perd = paste(start,end,sep='/')
  stock = data[perd]
  time = as.character(index(stock))
  stock = data.frame(stock)
  echartR(stock, time, c(Open, Close, Low, High), type='k') %>%
         setXAxis(name='Date', axisLabel=list(rotate=30)) %>%
         setYAxis(name="Price") %>% addML(xx, time, value, type='line')
}



mychart = list(
  option = list(
    backgroundColor = '#eee',
    animation = F,
    legend = list(bottom=10,left='center',data=c('Dow-Jones index', 'MA5', 'MA10', 'MA20', 'MA30'))
    ),
  tooltip =  list(
    trigger = 'axis',
    axisPointer = list(type='cross'),
    backgroundColor = 'rgba(245, 245, 245, 0.8)',
    borderWidth = 1,
    borderColor = '#ccc',
    padding = 10,
    textStyle = list(color='#000'),
    extraCssText = 'width: 170px'
  ),
  axisPointer = list(
    link = list(xAxisIndex='all'),
    label = list(backgroundColor='#777')
    ),
  toolbox = list(
    feature = list(dataZoom=list(yAxisIndex=F),brush = list(type = c('lineX', 'clear'))),
    brush = list(xAxisIndex='all',brushLink='all',outOfBrush=list(colorAlpha=0.1)),
    #grid = list(list(left='10%',right='8%',height='50%'),list(left='10',right='8%',top='63%',height='16%')),
    #xAxis = list()
    
    )  
  )

stock$sma10 = SMA(stock$close,10)
stock = na.omit(stock)
dates = as.character(index(stock))

data = as.data.frame(OHLC(stock))
rownames(data) = NULL
colnames(data) = NULL
data = as.matrix(data)
xx = apply(data,1,as.list)

ma = as.numeric(stock$sma10)

  option = list(
  backgroundColor = '#21202D',
  legend = list(
    data =  c('日K', 'MA5', 'MA10', 'MA20', 'MA30'),
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
  dataZoom= list(
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
  animation= F,
  series= list(
  list(
  type= 'k',
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
