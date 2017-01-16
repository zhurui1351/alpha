require('quantmod')
require('TTR')
require('dygraphs')
require('lubridate')
require('dplyr')
require('data.table')
require('e1071')
require("randomForest")
require('rpart')
require('rpart.plot')
require('reshape2')
require('dplyr')
require('RCurl')
require("rjson")
require('XML2R')
require('rsdmx')
require('RMySQL')
require(DMwR)
require('pracma')

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
source('src/config/db_config.R',encoding='utf-8')
source('src/config/china_future_symbol.R',encoding='utf-8')
sourceDir('src/utilities',encoding='utf-8')

options(scipen =200)
