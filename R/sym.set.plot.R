#' Function for plotting one "set" type value from the symbolic data table
#'
#' @author Andres Navarro
#' @param info The information of one "set" type value. Use data[num.r,num.col] to get info
#' @param col A specification for the default plotting color.
#' @param border A logical value indicating whether border should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#' @param reduce A logical value indicating whether values different from zero should be plotted in modal and set graphics.
#'
#' @return A plot of one "set" type value from the symbolic data table.
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex1_db2so)
#' data.sym <- classic.to.sym(ex1_db2so, concept=c("state", "sex"),
#'                            variables=c("county", "group", "age","age","age","age"),
#'                            variables.types=c("$I", "$C", "$C", "$S", "$M","$H"))
#' sym.set.plot(data.sym[1,4])
#' sym.set.plot(data.sym[1,4], reduce = TRUE)
#' }
sym.set.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE,reduce=FALSE){
  if(info$sym.var.types != "$S")
    stop("The data type is wrong, only $S are accepted")
  mt <- info$data
  mt[,mt > 0] <- 1/sum(mt > 0)
  names <- colnames(info$data)
  if(reduce){
    select <- colSums(mt) != 0
    mt <- cbind(mt[,select],0)
    names <- c(names[select],"...")
    colnames(mt)<-names
  }
  mt <- as.matrix(mt)
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Set)","")), xlab="", ylab="",
          names.arg=names,ylim = c(0,ifelse(max(mt)<0.5,0.5,1)), beside=TRUE, col=col,cex.names=.8)
  if(border)
    box("figure", col="black")
}
