#' Function for plotting one "interval" type value from the symbolic data table
#'
#' @author Andres Navarro
#' @param info The information of one "interval" type value. Use data[num.r,num.col] to get info
#' @param col A specification for the default plotting color.
#' @param border A logical value indicating whether border should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#'
#' @return A plot of one "interval" type value from the symbolic data table.
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex1_db2so)
#' data.sym <- classic.to.sym(ex1_db2so, concept=c("state", "sex"),
#'                            variables=c("county", "group", "age","age","age","age"),
#'                            variables.types=c("$I", "$C", "$C", "$S", "$M","$H"))
#' sym.interval.plot(data.sym[1,1])
#' }
sym.interval.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE){
  #if(info$sym.var.types != "$I")
  #  stop("The data type is wrong, only $I are accepted")
  interval <- as.numeric(info$data[1,])
  name <- paste("[",interval[1],",",interval[2],"]")
  plot(interval+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Interval)","")),yaxt='n')
  rect(interval[1],-1,interval[2],3.5,col=col)
  center <- c(mean(c(interval[1], interval[2])), mean(c(-1, 3.5)))
  size.font <- ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)
  text(center[1], center[2], labels = name,cex=size.font)
  if(border)
    box("figure", col="black")
}
