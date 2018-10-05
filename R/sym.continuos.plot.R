#' sym.continuos.plot
#' @keywords internal
sym.continuos.plot <- function(info, col=c("blue"), border=FALSE, show.type = TRUE) {
  if (info$sym.var.types != "$C") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $C are accepted")
  }

  continuos <- as.numeric(info$data) # obtiene el valor continuo

  # grafica el plano
  plot(continuos + c(-0.5, 0.5), c(0, 4.1), type = "n", xlab = "", ylab = "", main = paste(info$sym.var.names, ifelse(show.type, " (Continuos)", "")), yaxt = "n")
  abline(v = continuos, col = col, lty = 2, lwd = 2) # agrega la linea vertical con el valor continuo
  text(continuos, 2, labels = as.character(round(continuos, 2)), cex = ifelse(par()$pin[1] <= 1.5, par()$pin[1], 1.5)) # agrega el label con el valor continuo en mitad del plano
  if (border) { # se pone el borde en negro
    box("figure", col = "black")
  }
}
