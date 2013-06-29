Dbload <-
function(dsn,dbtable) {
  ch<-odbcConnect(dsn)
  sql="select * from "
  sql=paste(sql,dbtable,sep="")
  stable<-sqlQuery(ch, sql)
  odbcClose(ch)
  return(stable)
}
