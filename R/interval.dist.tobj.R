#' Symbolic Objects Distance
#' @name interval.dist.tobj
#' @aliases interval.dist.tobj
#' @author Oldemar Rodriguez Rojas
#' @description Compute a distance between two symbolic objects.
#' @usage interval.dist.tobj(sym.obj.x, sym.obj.y, distance = c('hausdorff',
#' 'centers', 'interscal'), p = 2)
#' @param sym.obj.x First Symbolic Object
#' @param sym.obj.y Second Symbolic Object
#' @param distance Dsitance to be use
#' @param p The p in the Hausdorff distance
#'
#' \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
#'
#' @return Return a real number that is the distance between sym.obj.x and sym.obj.y
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#' @seealso interval.dist
#' @examples
#' data(VeterinaryData)
#' VD <- VeterinaryData
#' interval.dist.tobj(sym.obj(VD,1),sym.obj(VD,2))
#' interval.dist.tobj(sym.obj(VD,1),sym.obj(VD,2),distance='centers')
#' @keywords Symbolic Distance
#' @export
interval.dist.tobj <- function(sym.obj.x, sym.obj.y, distance = c(
                                 "hausdorff", "centers",
                                 "interscal"
                               ), p = 2) {
  distance <- match.arg(distance)
  idn1 <- all(sym.obj.x$var.types == "$I")
  idn2 <- all(sym.obj.y$var.types == "$I")
  if ((idn1 == FALSE) || (idn2 == FALSE)) {
    stop("The two variables have to be interval type")
  }
  if (distance == "hausdorff") {
    pos <- 1
    sum <- 0
    for (j in 1:(sym.obj.x$M)) {
      a <- abs(sym.obj.x$obj.data.vector[pos] - sym.obj.y$obj.data.vector[pos])
      b <- abs(sym.obj.x$obj.data.vector[pos + 1] - sym.obj.y$obj.data.vector[pos +
        1])
      m <- max(a, b)
      pos <- pos + 2
      sum <- sum + m^p
    }
    return(sum^(1 / p))
  }
  if (distance == "centers") {
    pos <- 1
    sum <- 0
    for (j in 1:(sym.obj.x$M)) {
      a <- (sym.obj.x$obj.data.vector[pos] + sym.obj.x$obj.data.vector[pos +
        1]) / 2
      b <- (sym.obj.y$obj.data.vector[pos] + sym.obj.y$obj.data.vector[pos +
        1]) / 2
      m <- abs(a - b)
      pos <- pos + 2
      sum <- sum + m^p
    }
    return(sum^(1 / p))
  }
  if (distance == "interscal") {
    # Daneaux and Masson Distance
    pos <- 1
    suma <- 0
    sumb <- 0
    for (j in 1:(sym.obj.x$M)) {
      a <- (sym.obj.x$obj.data.vector[pos + 1] - sym.obj.x$obj.data.vector[pos] +
        sym.obj.y$obj.data.vector[pos + 1] - sym.obj.y$obj.data.vector[pos] -
        2 * abs((sym.obj.x$obj.data.vector[pos + 1] + sym.obj.x$obj.data.vector[pos]) / 2 -
          (sym.obj.y$obj.data.vector[pos + 1] + sym.obj.y$obj.data.vector[pos]) / 2) -
        abs(sym.obj.x$obj.data.vector[pos + 1] - sym.obj.x$obj.data.vector[pos] +
          sym.obj.y$obj.data.vector[pos + 1] - sym.obj.y$obj.data.vector[pos] -
          2 * abs((sym.obj.x$obj.data.vector[pos + 1] + sym.obj.x$obj.data.vector[pos]) / 2 -
            (sym.obj.y$obj.data.vector[pos + 1] + sym.obj.y$obj.data.vector[pos]) / 2)))^2
      suma <- suma + a
      b <- (sym.obj.x$obj.data.vector[pos + 1] - sym.obj.x$obj.data.vector[pos] +
        sym.obj.y$obj.data.vector[pos + 1] - sym.obj.y$obj.data.vector[pos] +
        2 * abs((sym.obj.x$obj.data.vector[pos + 1] + sym.obj.x$obj.data.vector[pos]) / 2 -
          (sym.obj.y$obj.data.vector[pos + 1] + sym.obj.y$obj.data.vector[pos]) / 2))^2
      sumb <- sumb + b
      pos <- pos + 2
    }
    return(c((1 / 4) * sqrt(suma), (1 / 2) * sqrt(sumb)))
  }
}
