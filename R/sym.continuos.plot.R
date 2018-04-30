#' Function for plotting one "continuos" type value from the symbolic data table
#'
#' @author Andres Navarro
#' @param info The information of one "continuos" type value. Use data[num.r,num.col] to get info
#' @param col A specification for the default plotting color.
#' @param border A logical value indicating whether border should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#'
#' @return A plot of one "continuos" type value from the symbolic data table.
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex1_db2so)
#' data.sym <- classic.to.sym(ex1_db2so, concept=c("state", "sex"),
#'                            col.names = c(county, group, age,age,age,age),
#'                            variables.types=c(county = type.interval(),
#'                            group = type.continuous(),
#'                            age = type.continuous(),
#'                             age = type.continuous(),
#'                             age = type.modal(),
#'                             age = type.histogram()))
#' sym.continuos.plot(data.sym[1,2])
#' }
sym.continuos.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE){
  if(info$sym.var.types != "$C")
    stop("The data type is wrong, only $C are accepted")
  continuos <- as.numeric(info$data)
  plot(continuos+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Continuos)","")),yaxt='n')
  abline(v=continuos, col=col, lty=2, lwd=2)
  size.font <- ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)
  text(continuos, 2, labels = as.character(continuos),cex=size.font)
  if(border)
    box("figure", col="black")
}
