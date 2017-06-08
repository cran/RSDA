#' Predict method to CM and CRM Linear regression model
#' @name predictsym.lm
#' @aliases predictsym.lm
#' @author Oldemar Rodriguez Rojas
#' @description To execute predict method the Center Method (CR) and Center and Range Method (CRM) to
#' Linear regression.
#' @usage predictsym.lm(model, new.sym.data, method = c('cm', 'crm'))
#' @param model The output of lm method.
#' @param new.sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param method 'cm' to Center Method and 'crm' to Center and Range Method.
#'
#' @return predictsym.lm produces a vector of predictions or a matrix of predictions and bounds
#' with column names fit, lwr, and upr if interval is set. For type = 'terms' this is a
#' matrix with a column per term and may have an attribute 'constant'
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#'
#' @seealso sym.glm
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm <- sym.lm(lpsa~.,sym.data=int_prost_train,method='cm')
#' pred.cm <- predictsym.lm(res.cm,int_prost_test,method='cm')
#'
#' @keywords Symbolic lm
#' @export
#' @import stats
#'
predictsym.lm <- function(model, new.sym.data, method = c("cm", "crm")) {
    idn <- all(new.sym.data$sym.var.types == new.sym.data$sym.var.types[1])
    if (idn == FALSE) 
        stop("All variables have to be of the same type")
    method <- match.arg(method)
    nn <- new.sym.data$N
    mm <- new.sym.data$M
    if (method == "cm") {
        mins <- matrix(0, nn, mm)
        maxs <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) {
            mins[i, j] <- sym.var(new.sym.data, j)$var.data.vector[i, 
                1]
            maxs[i, j] <- sym.var(new.sym.data, j)$var.data.vector[i, 
                2]
        }
        mins <- as.data.frame(mins)
        colnames(mins) <- new.sym.data$sym.var.names
        maxs <- as.data.frame(maxs)
        colnames(maxs) <- new.sym.data$sym.var.names
        pred.mins <- predict(model, newdata = mins, se.fit = TRUE)
        pred.maxs <- predict(model, newdata = maxs, se.fit = TRUE)
        Prediction <- data.frame(Minimums = pred.mins$fit, Maximums = pred.maxs$fit)
        return(list(MinPrediction = pred.mins, MaxPredictions = pred.maxs, 
            Fitted = Prediction))
    }
    if (method == "crm") {
        # Center Model
        centers <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(new.sym.data, 
            j)$var.data.vector[i, 1] + sym.var(new.sym.data, j)$var.data.vector[i, 
            2])/2
        centers <- as.data.frame(centers)
        colnames(centers) <- new.sym.data$sym.var.names
        predc <- predict(model$CenterModel, newdata = centers, se.fit = TRUE)
        # Range Model
        range <- matrix(0, nn, mm)
        for (i in 1:nn) for (j in 1:mm) range[i, j] <- (sym.var(new.sym.data, 
            j)$var.data.vector[i, 2] - sym.var(new.sym.data, j)$var.data.vector[i, 
            1])/2
        range <- as.data.frame(range)
        colnames(range) <- new.sym.data$sym.var.names
        predr <- predict(model$RangeModel, newdata = range, se.fit = TRUE)
        res.min <- predc$fit - predr$fit
        res.max <- predc$fit + predr$fit
        Prediction <- data.frame(Minimums = res.min, Maximums = res.max)
        return(list(CenterPrediction = predc, RangePrediction = predr, 
            Fitted = Prediction))
    }
}
