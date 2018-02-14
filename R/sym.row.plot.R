#' Function for plotting one row or one column from the symbolic data table
#'
#' @author Andres Navarro
#' @param data The row of the symbolic data table.
#' @param col A specification for the default plotting color.
#' @param matrix.form A vector of the form c(num.rows,num.columns)
#' @param border A logical value indicating whether border should be plotted.
#' @param size The magnification to be used for each graphic.
#' @param title A logical value indicating whether title should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#' @param reduce A logical value indicating whether values different from zero should be plotted in modal and set graphics.
#'
#' @return A plot of one row from symbolic data table.
#' @export
#'
#' @examples
#' \dontrun{
#' data(oils)
#' sym.row.plot(oils[3,])
#' sym.row.plot(oils[3,],matrix.form = c(2,2),border=TRUE)
#' }
sym.row.plot <- function(data, col=NA,matrix.form=NA,border=FALSE,size = 1, title=TRUE,show.type = FALSE,reduce =FALSE){
  #El tipo de dato es el correcto
  if(!("sym.data.table" %in% class(data))){
    stop("The data type is wrong, only sym.data.table are accepted")
  }

  #if(!(1 %in% c(data$M,data$N))){
  #  stop("Wrong dimensions on symbolic data table. Try: data[num.row,] or data[,num.col]")
  #}

  # No se ingresaron colores
  if(any(is.na(col)))
    col <- brewer.pal(n = 12, name = "Set3")

  #obtener la cantidad de variables
  num.col  <- data$M
  byColm <- FALSE
  if(data$N > 1 && num.col ==1){
    num.col <- data$N
    title <- FALSE
    byColm <- TRUE
    matrix.form <- c(num.col,1)
  }
  # Si se tiene matrix.form, el numero de espacios tiene que ser igual o superior al de variables
  if(any(!is.na(matrix.form)) ){
    if(!is.vector(matrix.form) || length(matrix.form)!=2)
      stop("Wrong format on matrix.form")
    if(prod(matrix.form) < num.col)
      stop("Wrong dimensions on matrix.form")
  }else{
    matrix.form <- c(1,num.col)
  }

  def.par <- par(no.readonly = TRUE)
  if(matrix.form[1] > 1)
    par(mfrow = matrix.form)
  else
    layout(matrix(1:matrix.form[2], matrix.form[1], matrix.form[2], byrow = TRUE))

  size.factor <- ifelse(is.numeric(size),1.70*(1/size),1.70)
  par(mar=c(0,0,1,0))
  if(!byColm){
    par(pin = (par()$din/(rep(matrix.form[2],2)*size.factor)) )
  } else{
    par(pin = (par()$din/(rep(matrix.form[1],2)*size.factor)) )
  }
  par(omi = c(matrix.form[2]*0.3242857,0,matrix.form[2]*0.3242857,0))

  #Grafica las variables
  for (index in 1:num.col) {
    if(byColm)
      var.data <- data[index,]
    else
      var.data <- data[,index]
    switch (var.data$sym.var.types,
            "$I" = sym.interval.plot(var.data,col,border,show.type=show.type),
            "$C" = sym.continuos.plot(var.data,col,border,show.type=show.type),
            "$H" = sym.hist.plot(var.data,col,border,FALSE,show.type=show.type),
            "$M" = sym.modal.plot(var.data,col,border,FALSE,show.type=show.type,reduce =reduce),
            "$S" = sym.set.plot(var.data,col,border,show.type=show.type,reduce =reduce))
  }
  if(title)
    mtext(toupper(data$sym.obj.names), outer = TRUE, cex = 1.5, side = 3)
  par(def.par)
}
