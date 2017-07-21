meta.to.RSDA <- function(data.meta) {
    dim.meta <- dim(data.meta)
    names.col.meta <- colnames(data.meta)
    pos.var <- pos.hist <- which(names.col.meta %in% c("$S", "$H"))
    pos.inter <- which(names.col.meta %in% c("$I"))
    pos.cont <- which(names.col.meta %in% c("$C"))
    N <- dim.meta[1]
    tam.col.data <- 0
    if (length(pos.hist) > 0) {
        tam.col.data <- tam.col.data + sum(data.meta[1, pos.hist + 1])
    }
    
    if (length(pos.inter) > 0) {
        tam.col.data <- tam.col.data + 2 * length(pos.inter)
        pos.var <- c(pos.var, pos.inter)
    }
    
    if (length(pos.cont) > 0) {
        tam.col.data <- tam.col.data + length(pos.inter)
        pos.var <- c(pos.var, pos.cont)
    }
    
    data <- matrix(rep(0, tam.col.data * N), nrow = N)
    pos.var <- pos.var[order(pos.var)]
    M <- length(pos.var)
    
    sym.var.names <- sym.var.length <- sym.var.starts <- rep(0, M)
    
    ind.star <- 1
    for (i in 1:M) {
        
        pos.act <- pos.var[i]
        var.act <- names.col.meta[pos.act]
        switch(var.act, `$S` = {
            var.length <- data.meta[1, pos.act + 1]
            sym.var.length[i] <- var.length
            sym.var.names[i] <- names.col.meta[pos.act + 1]
            sym.var.starts[i] <- ind.star + 2
            ind.star <- ind.star + 2 + var.length
        }, `$H` = {
            var.length <- data.meta[1, pos.act + 1]
            sym.var.length[i] <- data.meta[1, pos.act + 1]
            sym.var.names[i] <- names.col.meta[pos.act + 1]
            sym.var.starts[i] <- ind.star + 2
            ind.star <- ind.star + 2 + var.length
            
        }, `$I` = {
            sym.var.length[i] <- 2
            sym.var.names[i] <- names.col.meta[pos.act + 1]
            sym.var.starts[i] <- ind.star + 1
            ind.star <- ind.star + 3
        }, `$C` = {
            sym.var.length[i] <- 1
            sym.var.names[i] <- names.col.meta[pos.act + 1]
            sym.var.starts[i] <- ind.star + 1
            ind.star <- ind.star + 2
        })
    }
    
    data <- data.meta[, -c(pos.var, pos.hist + 1)]
    sym.obj.names <- row.names(data.meta)
    
    return(list(N = N, M = M, sym.obj.names = sym.obj.names, sym.var.names = sym.var.names, 
        sym.var.types = names.col.meta[pos.var], sym.var.length = sym.var.length, sym.var.starts = sym.var.starts, 
        meta = data.meta, data = data))
}
