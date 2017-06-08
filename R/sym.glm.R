#' Lasso, Ridge and and Elastic Net Linear regression model to interval variables
#' @name sym.glm
#' @aliases sym.glm
#' @author Oldemar Rodriguez Rojas
#' @description Execute Lasso, Ridge and and Elastic Net Linear regression model to interval variables.
#' @usage sym.glm(sym.data, response = 1, method = c('cm', 'crm'),
#' alpha = 1, nfolds = 10, grouped = TRUE)
#' @param sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param response The number of the column where is the response variable in the interval data table.
#' @param method 'cm' to generalized Center Method and 'crm' to generalized Center and Range Method.
#' @param alpha alpha=1 is the lasso penalty, and alpha=0 the ridge penalty. 0<alpha<1 is the elastic net method.
#' @param nfolds Number of folds - default is 10. Although nfolds can be as large as the sample size
#' (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable
#' is nfolds=3
#' @param grouped This is an experimental argument, with default TRUE, and can be ignored by most users.
#'
#' @return An object of class 'cv.glmnet' is returned, which is a list with the ingredients of the cross-validation fit.
#' @references Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear
#' regression model to symbolic interval data using Ridge Regression, Lasso
#' and Elastic Net methods. The IFCS2013 conference of the International Federation of
#' Classification Societies, Tilburg University Holland.
#' @seealso sym.lm
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm.lasso<-sym.glm(sym.data=int_prost_train,response=9,method='cm',
#'                       alpha=1,nfolds=10,grouped=TRUE)
#' pred.cm.lasso<-predictsym.glm(res.cm.lasso,response=9,int_prost_test,method='cm')
#' plot(res.cm.lasso)
#' plot(res.cm.lasso$glmnet.fit, 'norm', label=TRUE)
#' plot(res.cm.lasso$glmnet.fit, 'lambda', label=TRUE)
#' RMSE.L(sym.var(int_prost_test,9),pred.cm.lasso)
#' RMSE.U(sym.var(int_prost_test,9),pred.cm.lasso)
#' R2.L(sym.var(int_prost_test,9),pred.cm.lasso)
#' R2.U(sym.var(int_prost_test,9),pred.cm.lasso)
#' deter.coefficient(sym.var(int_prost_test,9),pred.cm.lasso)
#'
#' @keywords Symbolic Regression Lasso Ridge
#' @export
#' @import glmnet
#'
sym.glm <- function(sym.data, response = 1, method = c("cm", "crm"), 
    alpha = 1, nfolds = 10, grouped = TRUE) {
    idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
    if (idn == FALSE) 
        stop("All variables have to be of the same type")
    method <- match.arg(method)
    nn <- sym.data$N
    mm <- sym.data$M
    if (method == "cm") {
        centers <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, 
            j)$var.data.vector[i, 1] + sym.var(sym.data, j)$var.data.vector[i, 
            2])/2
        model <- cv.glmnet(centers[, -response], centers[, response], 
            nfolds = nfolds, grouped = grouped, alpha = alpha)
        return(model)
    }
    if (method == "crm") {
        ## Center Model
        centers <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, 
            j)$var.data.vector[i, 1] + sym.var(sym.data, j)$var.data.vector[i, 
            2])/2
        modelc <- cv.glmnet(centers[, -response], centers[, response], 
            nfolds = nfolds, grouped = grouped, alpha = alpha)
        # Range Model
        range <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) range[i, j] <- (sym.var(sym.data, 
            j)$var.data.vector[i, 2] - sym.var(sym.data, j)$var.data.vector[i, 
            1])/2
        modelr <- cv.glmnet(range[, -response], range[, response], 
            nfolds = nfolds, grouped = grouped, alpha = alpha)
        return(list(CenterModel = modelc, RangeModel = modelr))
    }
}
