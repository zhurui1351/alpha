



vis_bar = function(data)
{
  tooltip_formatter = JS("function (param) {
    param = param[0];
    return [
                         'Date: ' + param.name + '<br/>',
                         'Open: ' + param.data[0] + '<br/>',
                         'Close: ' + param.data[1] + '<br/>',
                         'Lowest: ' + param.data[2] + '<br/>',
                         'Highest: ' + param.data[3] + '<br/>'
                         ].join('');
}")
tooltip_formatter = gsub('\n','',tooltip_formatter)


axisPointer_formatter = JS("function (params) {
                           var seriesValue = (params.seriesData[0] || {}).value;
                           return params.value
                           + (seriesValue != null
                           ? '\n' + echarts.format.addCommas(seriesValue)
                           : ''
                           );
                           }
                           ")
axisPointer_formatter = gsub('\n','',axisPointer_formatter)

posiont_tooltip = JS("function (pos, params, el, elRect, size) {
                     var obj = {top: 10};
                     obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 30;
                     return obj;
                     }")
posiont_tooltip = gsub('\n','',posiont_tooltip)

  stock = data
  stock$sma10 = SMA(Cl(stock),10)
  stock$cci = CCI(HLC(stock),10)
  stock = na.omit(stock)
  dates = as.character(index(stock))
  
  data = as.data.frame(OHLC(stock))
  data = data[,c('Open','Close','Low','High')]
  rownames(data) = NULL
  colnames(data) = NULL
  data = as.matrix(data)
  xx = apply(data,1,as.list)
  ma = as.numeric(stock$sma10)
  cci = as.numeric(stock$cci)
  
  
  option = list(
    backgroundColor= '#eee',
    animation= F,
    legend= list(
      bottom= 10,
      left= 'center',
      data= c('Dow-Jones index', 'MA5')
    ),
    tooltip= list(
      trigger= 'axis',
      axisPointer= list(
        type= 'cross'
      ),
      backgroundColor= 'rgba(245, 245, 245, 0.8)',
      borderWidth= 1,
      borderColor= '#ccc',
      padding= 10,
      textStyle= list(
        color= '#000'
      ),
      position = posiont_tooltip,
      extraCssText= 'width= 170px'
    ),
    axisPointer= list(
      link= list(xAxisIndex= 'all'),
      label= list(
        backgroundColor= '#777'
      )
    ),
    toolbox= list(
      feature= list(
        dataZoom= list(
          yAxisIndex= F
        ),
        brush= list(
          type= c('lineX', 'clear')
        )
      )
    ),
    brush= list(
      xAxisIndex= 'all',
      brushLink= 'all',
      outOfBrush= list(
        colorAlpha= 0.1
      )
    ),
    grid= list(
      list(
        left= '10%',
        right= '8%',
        height= '50%'
      ),
      list(
        left= '10%',
        right= '8%',
        top= '63%',
        height= '16%'
      )
    ),
    xAxis= list(
      list(
        type= 'category',
        data= dates,#data.categoryData,
        scale= T,
        boundaryGap = F,
        axisLine= list(onZero= F),
        splitLine= list(show= F),
        splitNumber= 20,
        min= 'dataMin',
        max= 'dataMax',
        axisPointer= list(
          z= 100
        )
      ),
      list(
        type= 'category',
        gridIndex= 1,
        data= dates,#data.categoryData,
        scale= T,
        boundaryGap = F,
        axisLine= list(onZero= F),
        axisTick= list(show= F),
        splitLine= list(show= F),
        axisLabel= list(show= F),
        splitNumber= 20,
        min= 'dataMin',
        max= 'dataMax',
        axisPointer= list(
          label= list(
            formatter = axisPointer_formatter
            
          )
        )
      )
    ),
    yAxis= list(
      list(
        scale= T,
        splitArea= list(
          show= T
        )
      ),
      list(
        scale= T,
        gridIndex= 1,
        splitNumber= 2,
        axisLabel= list(show= F),
        axisLine= list(show= F),
        axisTick= list(show= F),
        splitLine= list(show= F)
      )
    ),
    dataZoom= list(
      list(
        type= 'inside',
        xAxisIndex= c(0, 1),
        start= 98,
        end= 100
      ),
      list(
        show= T,
        xAxisIndex= c(0, 1),
        type= 'slider',
        top= '85%',
        start= 98,
        end= 100
      )
    ),
    series= list(
      list(
        name= 'index',
        type= 'candlestick',
        data= xx,
        itemStyle= list(
          normal= list(
            borderColor= null,
            borderColor0= null
          )
        ),
        tooltip= list(
        )
      ),
      list(
        name= 'MA5',
        type= 'line',
        data= ma,
        smooth= T,
        lineStyle= list(
          normal= list(opacity= 0.5)
        )
      ),
      list(
        name= 'Volumn',
        type= 'line',
        xAxisIndex= 1,
        yAxisIndex= 1,
        data= cci
      )
    )
  )
  
  echart(option)
}

