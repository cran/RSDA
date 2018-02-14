#' Interval Principal Components Analysis.
#' @name sym.interval.pca
#' @aliases sym.interval.pca
#' @author Oldemar Rodriguez Rojas
#' @description Cazes, Chouakria, Diday and Schektman (1997)
#' proposed the Centers and the Tops Methods to extend the well known principal
#' components analysis method to a particular kind of symbolic objects
#' characterized by multi--values variables of interval type.
#' @usage sym.interval.pca(sym.data, method = c('classic', 'tops','centers',
#' 'principal.curves', 'optimized.distance', 'optimized.variance'))
#' @param sym.data Shoud be a symbolic data table
#' @param method It is use so select the method, 'classic' execute a classical principal component
#' analysis over the centers of the intervals, 'tops' to use the vertices algorithm
#' and 'centers' to use the centers algorithm.
#'
#' @return
#' Sym.Components: This a symbolic data table with the interval principal components. As
#' this is a symbolic data table we can apply over this table any other symbolic data
#' analysis method (symbolic propagation).
#'
#' Sym.Prin.Correlations: This is the interval correlations between the original interval
#' variables and the interval principal components, it can be use to plot the symbolic
#' circle of correlations.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#'
#' Cazes P., Chouakria A., Diday E. et Schektman Y. (1997).  Extension de l'analyse en
#' composantes principales a des donnees de type intervalle, Rev. Statistique Appliquee,
#' Vol. XLV Num. 3 pag. 5-24, France.
#'
#' Chouakria A. (1998)
#' Extension des methodes d'analysis factorialle a des
#' donnees de type intervalle, Ph.D. Thesis, Paris IX Dauphine University.
#'
#' Makosso-Kallyth S. and Diday E. (2012).  Adaptation of interval PCA to symbolic histogram
#' variables, Advances in Data Analysis and Classification July, Volume 6, Issue 2, pp 147-159.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.histogram.pca
#' @examples
#' \dontrun{
#' data(oils)
#' res<-sym.interval.pca(oils,'centers')
#' class(res) <- c('sym.data.table')
#' sym.scatterplot(res$Sym.Components[,1],res$Sym.Components[,1],
#'                 labels=TRUE,col='red',main='PCA Oils Data')
#' sym.scatterplot3d(res$Sym.Components[,1],res$Sym.Components[,2],
#'                   res$Sym.Components[,3],color='blue',main='PCA Oils Data')
#' sym.scatterplot.ggplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                        labels=TRUE)
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' res<-sym.interval.pca(oils,'classic')
#' plot(res,choix='ind')
#' plot(res,choix='var')
#'
#' data(lynne2)
#' res<-sym.interval.pca(lynne2,'centers')
#' class(res$Sym.Components) <- c('sym.data.table')
#' sym.scatterplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                 labels=TRUE,col='red',main='PCA Lynne Data')
#' sym.scatterplot3d(res$Sym.Components[,1],res$Sym.Components[,2],
#'                   res$Sym.Components[,3],color='blue', main='PCA Lynne Data')
#' sym.scatterplot.ggplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                        labels=TRUE)
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' data(StudentsGrades)
#' st<-StudentsGrades
#' s.pca<-sym.interval.pca(st)
#' plot(s.pca,choix='ind')
#' plot(s.pca,choix='var')
#' }
#' @keywords PCA Intervals
#' @export
#'
sym.interval.pca <- function(sym.data, method = c("classic", "tops", "centers", "principal.curves",
    "optimized.distance", "optimized.variance")) {
    idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
    if (idn == FALSE)
        stop("All variables have to be of the same type")
    method <- match.arg(method)
    if (method == "classic") {
        if ((sym.data$sym.var.types[1] != "$C") && (sym.data$sym.var.types[1] != "$I"))
            stop("Variables have to be continuos or Interval")
        if (sym.data$sym.var.types[1] == "$C")
            res <- PCA(sym.data$data, scale.unit = TRUE, ncp = sym.data$M, graph = FALSE) else if (sym.data$sym.var.types[1] == "$I") {
            nn <- sym.data$N
            mm <- sym.data$M
            centers <- matrix(0, nn, mm)
            centers <- as.data.frame(centers)
            rownames(centers) <- sym.data$sym.obj.names
            colnames(centers) <- sym.data$sym.var.names
            for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[i,
                1] + sym.var(sym.data, j)$var.data.vector[i, 2])/2
            res <- FactoMineR::PCA(centers, scale.unit = TRUE, ncp = sym.data$M, graph = FALSE)
        }
        return(res)
    }
    if (method == "centers") {
        nn <- sym.data$N
        mm <- sym.data$M
        centers <- matrix(0, nn, mm)
        centers.stan <- matrix(0, nn, mm)
        min.stan <- matrix(0, nn, mm)
        max.stan <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[i,
            1] + sym.var(sym.data, j)$var.data.vector[i, 2])/2
        # Standarized
        for (i in 1:nn) for (j in 1:mm) centers.stan[i, j] <- (centers[i, j] - mean(centers[,
            j]))/(sd(centers[, j]) * sqrt((nn - 1)/nn))
        # Min-Max
        for (i in 1:nn) {
            for (j in 1:mm) {
                min.stan[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 1] - mean(centers[,
                  j]))/(sd(centers[, j]) * sqrt((nn - 1)/nn))
                max.stan[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 2] - mean(centers[,
                  j]))/(sd(centers[, j]) * sqrt((nn - 1)/nn))
            }
        }
        # Correlation Centers Matrix
        R <- t(centers.stan) %*% centers.stan
        svd <- eigen(R)
        sym.comp <- sym.data
        # Interval Principal Components
        for (i in 1:nn) {
            posd <- 1
            for (j in 1:mm) {
                smin <- 0
                smax <- 0
                for (k in 1:mm) {
                  if (svd$vectors[k, j] < 0)
                    smin <- smin + max.stan[i, k] * svd$vectors[k, j] else smin <- smin + min.stan[i, k] * svd$vectors[k, j]
                  if (svd$vectors[k, j] < 0)
                    smax <- smax + min.stan[i, k] * svd$vectors[k, j] else smax <- smax + max.stan[i, k] * svd$vectors[k, j]
                }
                sym.comp$meta[i, sym.comp$sym.var.starts[j]] <- smin
                sym.comp$meta[i, sym.comp$sym.var.starts[j] + 1] <- smax
                sym.comp$data[i, posd] <- smin
                sym.comp$data[i, posd + 1] <- smax
                posd <- posd + 2
            }
        }
        pos <- 1
        for (j in 1:mm) {
            comp.name <- paste("C", j, sep = "")
            sym.comp$sym.var.names[j] <- comp.name
            comp.name <- paste("Min.C", j, sep = "")
            colnames(sym.comp$data)[pos] <- comp.name
            comp.name <- paste("Max.C", j, sep = "")
            colnames(sym.comp$data)[pos + 1] <- comp.name
            pos <- pos + 2
            comp.name <- paste("Min.C", j, sep = "")
            colnames(sym.comp$meta)[sym.comp$sym.var.starts[j]] <- comp.name
            comp.name <- paste("Max.C", j, sep = "")
            colnames(sym.comp$meta)[sym.comp$sym.var.starts[j] + 1] <- comp.name
        }
        # Interval Principal Correlations
        svdV <- matrix(0, nn, nn)
        for (i in 1:nn) {
            for (j in 1:mm) {
                ss <- 0
                for (k in 1:mm) {
                  ss <- ss + centers.stan[i, k] * svd$vectors[k, j]
                }
                svdV[i, j] <- (1/sqrt(svd$values[j])) * ss
            }
        }
        IPrinCorre <- matrix(0, mm, 2 * mm)
        for (i in 1:mm) {
            pcol <- 1
            for (j in 1:mm) {
                smin <- 0
                smax <- 0
                for (k in 1:nn) {
                  if (svdV[k, j] < 0)
                    smin <- smin + (1/sqrt(nn)) * max.stan[k, i] * svdV[k, j] else smin <- smin + (1/sqrt(nn)) * min.stan[k, i] * svdV[k, j]
                  if (svdV[k, j] < 0)
                    smax <- smax + (1/sqrt(nn)) * min.stan[k, i] * svdV[k, j] else smax <- smax + (1/sqrt(nn)) * max.stan[k, i] * svdV[k, j]
                }
                IPrinCorre[i, pcol] <- smin
                IPrinCorre[i, pcol + 1] <- smax
                pcol <- pcol + 2
            }
        }
        IPrinCorre <- as.data.frame(IPrinCorre)
        rownames(IPrinCorre) <- sym.data$sym.var.names
        class(sym.comp) <- "sym.data.table"
        return(list(Sym.Components = sym.comp, Sym.Prin.Correlations = IPrinCorre))
    }
    if (method == "tops") {
        res <- vertex.pca.j(sym.data)
        return(res)
    }
    if (method == "principal.curves") {
        res <- sym.interval.pc(sym.data, "vertex", 150, FALSE, FALSE, TRUE)
        return(res)
    }
    if (method == "optimized.distance") {
        res <- optim.pca.distance.j(sym.data)
        return(res)
    }
    if (method == "optimized.variance") {
        res <- optim.pca.variance.j(sym.data, num.dimension = 3)
        return(res)
    }
    return(TRUE)
}
