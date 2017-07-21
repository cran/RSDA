#' Symbolic Linear Regression for two variables.
#' @name sym.lm.bi
#' @aliases sym.lm.bi
#' @author Oldemar Rodriguez Rojas
#' @description The function build a symbolic regression for two interval or continuos variables.
#' @usage sym.lm.bi(sym.var.x, sym.var.y, method = c('mid-points', 'tops', 'inf-sup',
#' 'billard'))
#' @param sym.var.x The firth symbolic variable.
#' @param sym.var.y The second symbolic variable.
#' @param method The thirth symbolic variable.
#'
#' @return
#' This function return a regression structure as folllows:\cr
#'
#' $Intercept \cr
#' [1] 38.64236\cr
#' \cr
#' $Beta1\cr
#' [1] 0.3081313\cr
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#'
#' @examples
#' data(example3)
#' sym.data<-example3
#' class(sym.data) <- c('sym.data.table')
#' lm.mod<-sym.lm.bi(sym.var(sym.data,1),sym.var(sym.data,4))
#' sym.scatterplot(sym.data[,1],sym.data[,4],col='blue',
#'                 main='Linear Regression')
#' abline(lm.mod,lwd=3)
#'
#' lm.mod<-sym.lm.bi(sym.var(sym.data,2),sym.var(sym.data,6))
#' sym.scatterplot(sym.data[,2],sym.data[,6],
#'                 col='blue',main='Linear Regression')
#' abline(lm.mod,lwd=3)
#'
#' data(lynne1)
#' sym.data<-lynne1
#' class(sym.data) <- c('sym.data.table')
#' lm.mod<-sym.lm.bi(sym.var(lynne1,2),sym.var(lynne1,1))
#' sym.scatterplot(sym.data[,2],sym.data[,1],labels=TRUE,
#'                 col='red',main='Linear Regression')
#' abline(lm.mod,lwd=3,col='blue')
#'
#' lm.mod<-sym.lm.bi(sym.var(lynne1,2),sym.var(lynne1,1),method='inf-sup')
#' sym.scatterplot(sym.data[,2],sym.data[,1],labels=TRUE,
#'                 col='red',main='Linear Regression')
#' abline(lm.mod$inf,lwd=3,col='blue')
#' abline(lm.mod$sup,lwd=3,col='blue')
#'
#' lm.mod<-sym.lm.bi(sym.var(lynne1,2),sym.var(lynne1,1),method='tops')
#' sym.scatterplot(sym.data[,2],sym.data[,1],labels=TRUE,
#'                 col='red',main='Linear Regression')
#' abline(lm.mod,lwd=3,col='blue')
#'
#' lm.mod<-sym.lm.bi(sym.var(lynne1,2),sym.var(lynne1,1),method='billard')
#' sym.scatterplot(sym.data[,2],sym.data[,1],labels=TRUE,
#'                 col='red',main='Linear Regression')
#' abline(lm.mod$Intercept,lm.mod$Beta1,lwd=3,col='blue')
#' @keywords Symbolic Regression
#' @export
#'
sym.lm.bi <- function(sym.var.x, sym.var.y, method = c("mid-points", "tops", "inf-sup", 
    "billard")) {
    method <- match.arg(method)
    if (((sym.var.x$var.type != "$C") || (sym.var.y$var.type != "$C")) && ((sym.var.x$var.type != 
        "$I") || (sym.var.y$var.type != "$I"))) 
        stop("Impossible to use lm this type of variable")
    if (method == "mid-points") {
        if ((sym.var.x$var.type == "$C") && (sym.var.y$var.type == "$C")) {
            lm1 <- lm(sym.var.y$var.data.vector ~ sym.var.x$var.data.vector)
        }
        if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
            vx <- (sym.var.x$var.data.vector[, 1] + sym.var.x$var.data.vector[, 2])/2
            vy <- (sym.var.y$var.data.vector[, 1] + sym.var.y$var.data.vector[, 2])/2
            lm1 <- lm(vy ~ vx)
        }
        return(lm1)
    }
    if (method == "inf-sup") {
        if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
            vx <- sym.var.x$var.data.vector[, 2]
            vy <- sym.var.y$var.data.vector[, 1]
            lm1 <- lm(vy ~ vx)
            vx <- sym.var.x$var.data.vector[, 1]
            vy <- sym.var.y$var.data.vector[, 2]
            lm2 <- lm(vy ~ vx)
            
        }
        return(list(inf = lm1, sup = lm2))
    }
    if (method == "tops") {
        if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
            vx <- c(sym.var.x$var.data.vector[, 1], sym.var.x$var.data.vector[, 1], 
                sym.var.x$var.data.vector[, 2], sym.var.x$var.data.vector[, 2])
            vy <- c(sym.var.y$var.data.vector[, 1], sym.var.y$var.data.vector[, 2], 
                sym.var.y$var.data.vector[, 1], sym.var.y$var.data.vector[, 2])
            lm1 <- lm(vy ~ vx)
        }
        return(lm1)
    }
    if (method == "billard") {
        if ((sym.var.x$var.type == "$I") && (sym.var.y$var.type == "$I")) {
            vx <- sym.var.x
            vy <- sym.var.y
            beta1 <- sym.cov(vx, vy, method = "billard")/sym.variance(vx, method = "billard")
            intercept <- sym.mean(vy) - beta1 * sym.mean(vx)
        }
        return(list(Intercept = intercept, Beta1 = beta1))
    }
}
