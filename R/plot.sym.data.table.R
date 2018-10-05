#' Function for plotting a symbolic object
#'
#' @author Andres Navarro
#' @param x The symbolic object.
#' @param col A specification for the default plotting color.
#' @param matrix.form A vector of the form c(num.rows,num.columns).
#' @param border A logical value indicating whether border should be plotted.
#' @param size The magnification to be used for each graphic.
#' @param title A logical value indicating whether title should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#' @param font.size The font size of graphics.
#' @param hist.angle.x The angle of labels in y axis. Only for histogram plot
#' @param reduce A logical value indicating whether values different from zero should be plotted in modal and set graphics.
#' @param ... Arguments to be passed to methods.
#'
#' @return A plot of the symbolic data table.
#' @keywords Plot Symbolic data table
#' @export
#' @exportMethod
#' @examples
#' \dontrun{
#' data(oils)
#' plot(oils)
#' plot(oils,border = T,size = 1.3)
#' }
plot.sym.data.table <- function(x, col = NA, matrix.form = NA, border = FALSE, size = 1,
                                title = TRUE, show.type = FALSE, font.size = 1, reduce = FALSE, hist.angle.x = 60, ...) {
  if (!("sym.data.table" %in% class(x))) { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only sym.data.table are accepted")
  }

  if (any(is.na(col))) { # No se ingresaron colores
    col <- distinctColorPalette(max(x$sym.var.length))
  }  # Cantidad de colores correspondiente a la cantidad maxima de variables

  title <- !(x$N > 1 && x$M == 1) # si filas > 1 y columnas == 1 tenemos que recorrer en una columna y no mostrar titulo


  if (any(!is.na(matrix.form))) { # Si se tiene matrix.form
    if (!is.vector(matrix.form) || length(matrix.form) != 2) {
      stop("Wrong format on matrix.form")
    }
    if (prod(matrix.form) < x$M * x$N) { # El numero de espacios tiene que ser igual o superior al de variables
      stop("Wrong dimensions on matrix.form")
    }
  } else { # Si no hay matriz se crea una, segun la orientacion de la fila
    matrix.form <- c(x$N, x$M)
  }

  size.factor <- ifelse(is.numeric(size), 1.74 * (1 / size), 1.74) # Determina un tamaño por defecto de los graficos (proporcion agregada)

  # Guarda la configuracion de par original
  def.par <- par(no.readonly = T)
  # Cambia la configuracion de los graficos
  par(mfrow = matrix.form)
  par(mar = c(0, 0, 1, 0))
  par(pin = (par()$din / (rep(max(matrix.form), 2) * size.factor)))
  # par(cex.axis = 0.7 * font.size)
  par(cex = 0.7 * font.size)

  # Grafica las variables
  for (index.row in 1:x$N) {
    for (index.col in 1:x$M) {
      var.data <- x[index.row, index.col]
      switch(var.data$sym.var.types,
        "$I" = sym.interval.plot(var.data, col, border, show.type),
        "$C" = sym.continuos.plot(var.data, col, border, show.type),
        "$H" = sym.hist.plot(var.data, col, border, show.type, hist.angle.x),
        "$M" = sym.modal.plot(var.data, col, border, show.type, reduce),
        "$S" = sym.set.plot(var.data, col, border, show.type, reduce)
      )
    }
  }

  # Pone el titulo
  if (title) {
    mtext(toupper(x$sym.obj.names), outer = TRUE, cex = 1.5, side = 3)
  }

  tryCatch(expr = {
    par(def.par) # retorna al estado original
  }, error = function(err) {
    suppressMessages(grDevices::dev.off())
    message("The size of the device is too small or the \"size\" parameter needs to be adjusted.")
  }, warning = function(war) {
    message(paste0("ImputeValues: ", war))
  })
}
