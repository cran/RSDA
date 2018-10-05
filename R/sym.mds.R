#' Symbolic Multidemensional Scaling
#' @name sym.mds
#' @aliases sym.mds
#' @author Oldemar Rodriguez Rojas
#' @description This function execute a multidimensional scaling from a interval symbolic data matrix.
#' @usage sym.mds(sym.data, distance = c('hausdorff', 'centers'), p = 2,
#' method = c('classic', 'INTERSCAL'))
#' @param sym.data The symbolic data matrix.
#' @param distance The distance to be use.
#' @param p The p in the Hausdorff distance
#'
#' \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
#' @param method The method to be used.
#'
#' @return Return the coordanates to plot the graphic.
#' @references Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional
#' scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51,
#' 360-378.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.interval.pca
#' @examples
#' \dontrun{
#' data(oils)
#' res<-sym.mds(oils)
#' plot(res,pch = 23, bg = 'red', xlab = 'Score 1', ylab = 'Score 2')
#' res<-sym.mds(oils,distance='centers')
#' plot(res,pch = 23, bg = 'red', xlab = 'Score 1', ylab = 'Score 2')
#' }
#' @keywords Symbolic MDS
#' @export
#'
sym.mds <- function(sym.data, distance = c("hausdorff", "centers"), p = 2, method = c(
                      "classic",
                      "INTERSCAL"
                    )) {
  distance <- match.arg(distance)
  method <- match.arg(method)
  if (method == "classic") {
    return(cmdscale(interval.dist(sym.data, distance)))
  }
}
