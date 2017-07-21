#' CM and CRM Linear regression model.
#' @name sym.lm
#' @aliases sym.lm
#' @author Oldemar Rodriguez Rojas
#' @description To execute the Center Method (CR) and Center and Range Method (CRM) to Linear regression.
#' @usage sym.lm(formula, sym.data, method = c('cm', 'crm'))
#' @param formula An object of class 'formula' (or one that can be coerced to that class): a symbolic description
#' of the model to be fitted.
#' @param sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param method 'cm' to Center Method and 'crm' to Center and Range Method.
#' @details
#' Models for lm are specified symbolically. A typical model has the form response ~
#' terms where response is the (numeric) response vector and terms is a series of
#' terms which specifies a linear predictor for response. A terms specification of
#' the form first + second indicates all the terms in first together with all the
#' terms in second with duplicates removed. A specification of the form first:second indicates
#' the set of terms obtained by taking the interactions of all terms in first with all terms
#' in second. The specification first*second indicates the cross of first and second.
#' This is the same as first + second + first:second.
#' @return sym.lm returns an object of class 'lm' or for multiple responses of class c('mlm', 'lm')
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#'
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm<-sym.lm(lpsa~.,sym.data=int_prost_train,method='cm')
#' pred.cm<-predictsym.lm(res.cm,int_prost_test,method='cm')
#' RMSE.L(sym.var(int_prost_test,9),pred.cm$Fitted)
#' RMSE.U(sym.var(int_prost_test,9),pred.cm$Fitted)
#' R2.L(sym.var(int_prost_test,9),pred.cm$Fitted)
#' R2.U(sym.var(int_prost_test,9),pred.cm$Fitted)
#' deter.coefficient(sym.var(int_prost_test,9),pred.cm$Fitted)
#' @keywords Symbolic lm
#' @import stats scales
#' @export
#'
sym.lm <- sym.lm <- function(formula, sym.data, method = c("cm", "crm")) {
    idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
    if (idn == FALSE) 
        stop("All variables have to be of the same type")
    method <- match.arg(method)
    nn <- sym.data$N
    mm <- sym.data$M
    if (method == "cm") {
        centers <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 
            1] + sym.var(sym.data, j)$var.data.vector[i, 2])/2
        centers <- as.data.frame(centers)
        colnames(centers) <- sym.data$sym.var.names
        model <- lm(formula, data = centers)
        return(model)
    }
    if (method == "crm") {
        # Center Model
        centers <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 
            1] + sym.var(sym.data, j)$var.data.vector[i, 2])/2
        centers <- as.data.frame(centers)
        colnames(centers) <- sym.data$sym.var.names
        modelc <- lm(formula, data = centers)
        # Range Model
        range <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) range[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 
            2] - sym.var(sym.data, j)$var.data.vector[i, 1])/2
        range <- as.data.frame(range)
        colnames(range) <- sym.data$sym.var.names
        modelr <- lm(formula, data = range)
        return(list(CenterModel = modelc, RangeModel = modelr))
    }
}
