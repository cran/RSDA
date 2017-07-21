#' Symbolic Hierarchical Clustering
#' @name sym.hclust
#' @aliases sym.hclust
#' @author Oldemar Rodriguez Rojas
#' @description This function allows us to execute a symbolic
#' hierarchical clustering to interval variables.
#' @usage sym.hclust(sym.data, distance = c('hausdorff', 'centers'), p = 2,
#' method = c('ward', 'single', 'complete', 'average', 'mcquitty',
#'            'median', 'centroid'), members = NULL)
#' @param sym.data The symbolic data table.
#' @param distance The distance to be use.
#' @param p The p in the Hausdorff distance :
#' \deqn{d(w_{u_1},w_{u_2}) = \left( \sum_{j=1}^m \Phi_j(w_{u_1},w_{u_2})^p  \right)^{1/p}}
#' @param method The method to be use, like in hclust R function.
#' @param members Like in hclust R function.
#'
#' @return Return a dendogram plot structure.
#' @references
#' Carvalho F., Souza R.,Chavent M., and Lechevallier Y. (2006)
#' Adaptive Hausdorff distances and dynamic clustering of symbolic interval data. Pattern
#' Recognition Letters Volume 27, Issue 3, February 2006, Pages 167-179
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @examples
#' data(oils)
#' sh<-sym.hclust(oils)
#' plot(sh)
#' sh<-sym.hclust(oils,'centers')
#' plot(sh)
#' @keywords Symbolic Clustering
#' @export
#'
sym.hclust <- function(sym.data, distance = c("hausdorff", "centers"), p = 2, method = c("ward", 
    "single", "complete", "average", "mcquitty", "median", "centroid"), members = NULL) {
    distance <- match.arg(distance)
    method <- match.arg(method)
    return(hclust(interval.dist(sym.data, distance), method))
}
