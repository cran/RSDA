process.interval.variable <-
function(variableName, conceptColumns) {
  varType <- "$I"
  return (fn$sqldf("SELECT '$varType', MIN($variableName) AS $variableName, MAX($variableName) AS $variableName FROM main.dataTable GROUP BY $conceptColumns ORDER BY $conceptColumns"))
}
