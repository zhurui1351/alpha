require(RMySQL)


getMysqlCon = function(dbname)
{
  host = '127.0.0.1'
  username="root"
  password = '123456'
  port = 3306
  conn = dbConnect(MySQL(), dbname = dbname, username=username, password=password,host=host,port=port)
  return(conn)
}

writeToMysqltable = function(data,dbname,tbname,overwrite = F)
{
  conn = getMysqlCon(dbname)
  returnvalue = dbWriteTable(conn,tbname,data,overwrite=overwrite,row.names = F)
  dbDisconnect(conn)
  return(returnvalue)
}
