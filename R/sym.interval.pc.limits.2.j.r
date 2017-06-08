sym.interval.pc.limits.2.j <- function(sym.data, prin.curve, num.vertex, lambda) {
    num.vars <- sym.data$M
    num.ind <- sym.data$N
    
    res <- as.data.frame(prin.curve)
    res$lambda <- lambda
    
    nn <- sym.data$N
    sym.indiv <- rep("X", sum(num.vertex))
    
    start <- 1
    finish <- num.vertex[1]
    sym.indiv[start:finish] <- sym.data$sym.obj.names[1]
    
    for (i in 2:nn) {
        previous <- num.vertex[i - 1]
        start <- start + previous
        finish <- num.vertex[i] + finish
        sym.indiv[start:finish] <- sym.data$sym.obj.names[i]
    }
    
    res$symindiv <- sym.indiv
    var.type <- rep("$I", num.vars + 1)
    variables <- c(sym.data$sym.var.names, "lambda")
    
    sym.res <- classic.to.sym(dataTable = res, concept = c("symindiv"), variables = variables, 
        variables.types = var.type)
    return(sym.res)
}
