#' Predict method to Lasso, Ridge and and Elastic Net Linear regression model
#' to interval variables
#' @name predictsym.glm
#' @aliases predictsym.glm
#' @author Oldemar Rodriguez Rojas
#' @description To execute Predict method to Lasso, Ridge and and Elastic Net Linear
#' regression model to interval variables.
#' @usage predictsym.glm(model, new.sym.data, response = 1, method = c('cm', 'crm'))
#' @param model The output of glm method.
#' @param new.sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param response The number of the column where is the response variable in the interval data table.
#' @param method 'cm' to generalized Center Method and 'crm' to generalized Center and Range Method.
#'
#' @return The object returned depends the ... argument which is passed on to the predict
#' method for glmnet objects.
#' @references Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear
#' regression model to symbolic interval data using Ridge Regression, Lasso
#' and Elastic Net methods. The IFCS2013 conference of the International Federation of
#' Classification Societies, Tilburg University Holland.
#' @seealso sym.glm
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
#' @import stats glmnet
#'
predictsym.glm <- function(model, new.sym.data, response = 1, method = c("cm", "crm")) {
  idn <- all(new.sym.data$sym.var.types == new.sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }
  method <- match.arg(method)
  nn <- new.sym.data$N
  mm <- new.sym.data$M
  if (method == "cm") {
    mins <- matrix(0, nn, mm)
    maxs <- matrix(0, nn, mm)
    for (i in 1:nn) {
      for (j in 1:mm) {
        mins[i, j] <- sym.var(new.sym.data, j)$var.data.vector[i, 1]
        maxs[i, j] <- sym.var(new.sym.data, j)$var.data.vector[i, 2]
      }
    }
    pred.mins <- predict(model, newx = mins[, -response], s = "lambda.min")
    pred.maxs <- predict(model, newx = maxs[, -response], s = "lambda.min")
    Prediction <- data.frame(Minimums = pred.mins, Maximums = pred.maxs)
    return(Prediction)
  }
  if (method == "crm") {
    # Center Model
    centers <- matrix(0, nn, mm)
    for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(new.sym.data, j)$var.data.vector[
          i,
          1
        ] + sym.var(new.sym.data, j)$var.data.vector[i, 2]) / 2
    predc <- predict(model$CenterModel, newx = centers[, -response], s = "lambda.min")
    # Range Model
    range <- matrix(0, nn, mm)
    for (i in 1:nn) for (j in 1:mm) range[i, j] <- (sym.var(new.sym.data, j)$var.data.vector[
          i,
          2
        ] - sym.var(new.sym.data, j)$var.data.vector[i, 1]) / 2
    predr <- predict(model$RangeModel, newx = range[, -response], s = "lambda.min")
    res.min <- predc - predr
    res.max <- predc + predr
    Prediction <- data.frame(Minimums = res.min, Maximums = res.max)
    return(Prediction)
  }
}
