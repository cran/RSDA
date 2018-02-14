#' Interscal Method
#' @name interscal
#' @aliases interscal
#' @author Oldemar Rodriguez Rojas
#' @description Execute Interscal Method.
#' @usage interscal(sym.data)
#' @param sym.data The symbolic data matrix.
#'
#' @return The symbolic interval components.
#' @references Groenen, P.J.F., Winsberg, S., Rodriguez, O., Diday, E. (2006). I-Scal: Multidimensional
#' scaling of interval dissimilarities. Computational Statistics and Data Analysis, 51,
#' 360-378.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University
#' @seealso sym.interval.pca
#'
#' @examples
#' \dontrun{
#' data(oils)
#' res<-interscal(oils)
#' class(res$Sym.Components) <- c('sym.data.table')
#' sym.scatterplot(res$Sym.Components[,1], res$Sym.Components[,2],
#'                 labels=TRUE,col='red',main='Interscal CFA Data')
#' sym.scatterplot3d(res$Sym.Components[,1], res$Sym.Components[,2],
#'                   res$Sym.Components[,3],color='blue',
#'                   labels=TRUE,main='Interscal CFA Data')
#' sym.scatterplot.ggplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                        labels=TRUE)
#' }
#' @keywords Interscal
#' @export
interscal <- function(sym.data) {
    sdn <- sym.normalize(sym.data)
    BM <- interscal.B(sdn)
    ds <- eigen(BM)
    ValP <- ds$values
    VecP <- ds$vector
    n <- 2 * sym.data$N
    m <- sym.data$M
    if (n < m)
        nn <- min(n, m) else nn <- m
    prin.com <- matrix(0, n, nn)
    for (i in 1:n) {
        for (j in 1:nn) {
            prin.com[i, j] = sqrt(abs(ValP[j])) * VecP[i, j]
        }
    }
    # Interval Principal Components
    sym.comp <- sym.data
    NObjSimb <- sym.data$N
    nn <- sym.data$M
    Min <- matrix(0, NObjSimb, nn)
    Max <- matrix(0, NObjSimb, nn)
    for (j in 1:nn) {
        for (i in 1:NObjSimb) {
            ii <- 2 * (i - 1) + 1
            max <- prin.com[ii, j]
            min <- prin.com[ii, j]
            for (k in ii:(2 * i)) {
                if (prin.com[k, j] > max)
                  max <- prin.com[k, j]
                if (prin.com[k, j] < min)
                  min <- prin.com[k, j]
            }
            Min[i, j] <- min
            Max[i, j] <- max
        }
    }
    for (i in 1:NObjSimb) {
        posd <- 1
        for (j in 1:nn) {
            sym.comp$meta[i, sym.comp$sym.var.starts[j]] <- Min[i, j]
            sym.comp$meta[i, sym.comp$sym.var.starts[j] + 1] <- Max[i, j]
            sym.comp$data[i, posd] <- Min[i, j]
            sym.comp$data[i, posd + 1] <- Max[i, j]
            posd <- posd + 2
        }
    }
    return(list(Eigenvalues = ValP, Eignvector = VecP, Sym.Components = sym.comp))
}
