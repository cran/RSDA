#' Function for plotting one "histogram" type value from the symbolic data table
#'
#' @author Andres Navarro
#' @param info The information of one "histogram" type value. Use data[num.r,num.col] to get info
#' @param col A specification for the default plotting color.
#' @param border A logical value indicating whether border should be plotted.
#' @param ylab A logical value indicating whether the label of y axis has to be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#'
#' @return A plot of one "histogram" type value from the symbolic data table.
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
#'                            age = type.continuous(),
#'                            age = type.modal(),
#'                            age = type.histogram()))
#' sym.hist.plot(data.sym[1,6])
#' }
sym.hist.plot <- function(info,col=c("blue"),border=FALSE, ylab=TRUE,show.type = TRUE){
  if(info$sym.var.types != "$H")
    stop("The data type is wrong, only $H are accepted")
  dataset <- info$data
  namesC <- colnames(dataset)
  matches <- regmatches(namesC, gregexpr("[[:digit:]]+", namesC))
  dataTemp <- c()
  for (index in 1:length(matches)) {
    interval <- as.numeric(matches[[index]])
    if(dataset[1,index]!=0)
      dataTemp <- c(dataTemp, runif(100*dataset[1,index], interval[1], interval[2]))
  }

  breaks <- unique(as.numeric(unlist(matches)))
  hist(dataTemp, breaks = breaks,
       freq = T, xlim = c(min(breaks),max(breaks))+c(-1,1),ylim = c(0,100),
       border = "black",col = col,
       ylab = ifelse(ylab,"% Percentage",""),xlab = "",
       main = paste(info$sym.var.names,ifelse(show.type," (Histogram)","")))
  if(border)
    box("figure", col="black")
}
