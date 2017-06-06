
all_packages = c('quantmod','TTR','dygraphs','lubridate','dplyr','data.table','e1071','randomForest','rpart',
                 'rpart.plot','reshape2','dplyr','RCurl','rjson','XML2R','rsdmx','RMySQL','DMwR','TSclust',
                 'pracma','Matrix','arules','arulesSequences','WindR','proxy','cluster','dtw','SimilarityMeasures','splines',
                 'R6','recharts2'
                 )

for(pack in all_packages)
{
  if(!require(pack,character.only=T))
  {
    print('installing:')
    print(pack)
    install.packages(pack)
    require(pack,character.only=T)
  }
}

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
sourceDir('src/global')
sourceDir('src/position')
sourceDir('src/analysis')
source('src/global/visualization.R')
options(scipen =200)
