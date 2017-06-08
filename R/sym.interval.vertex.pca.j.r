sym.interval.vertex.pca.j <- function(data.sym) {
    vertex.sym <- vertex.interval.new.j(data.sym)
    data.vertex <- as.matrix(vertex.sym$vertex)
    dim.sym <- dim(data.vertex)
    indx.cols <- data.frame(i = 1:dim.sym[2])
    
    
    medias <- apply(indx.cols, 1, function(i) {
        mean(data.vertex[, i])
    })
    data.vertex.centrada <- t(t(data.vertex) - medias)
    
    desviaciones <- apply(indx.cols, 1, function(i) {
        sd(data.vertex[, i])
    })
    desviaciones <- desviaciones * sqrt((dim.sym[1] - 1)/dim.sym[1])
    
    data.vertex.centrada <- t(t(data.vertex.centrada)/desviaciones)
    
    matrix.data <- as.matrix(data.sym$data)
    matrix.data.centrada <- as.matrix(data.sym$data)
    m <- data.sym$M
    n <- data.sym$N
    
    indx <- data.frame(pos = 1:m)
    
    list.stand <- apply(indx, 1, function(i) {
        pos.ini <- 2 * (i - 1) + 1
        pos.fin <- 2 * i
        matrix.data.centrada[, pos.ini:pos.fin] <<- (matrix.data[, pos.ini:pos.fin] - 
            medias[i])/desviaciones[i]
    })
    
    cor.matrix <- t(data.vertex.centrada) %*% data.vertex.centrada/dim.sym[1]
    cor.matrix.eigen <- eigen(cor.matrix)
    vector.propios <- cor.matrix.eigen$vectors
    vector.propios.pos <- vector.propios
    
    vector.propios.pos <- apply(indx, 1, function(i) {
        apply(indx, 1, function(j) {
            if (vector.propios[j, i] > 0) {
                vector.propios[j, i]
            } else {
                0
            }
        })
    })
    
    
    vector.propios.neg <- vector.propios - vector.propios.pos
    
    indx.max <- seq(2, to = 2 * m, by = 2)
    indx.min <- seq(1, to = 2 * m, by = 2)
    
    max.neg <- matrix.data.centrada[, indx.max] %*% vector.propios.neg
    min.neg <- matrix.data.centrada[, indx.min] %*% vector.propios.neg
    
    max.pos <- matrix.data.centrada[, indx.max] %*% vector.propios.pos
    min.pos <- matrix.data.centrada[, indx.min] %*% vector.propios.pos
    
    maximos <- max.pos + min.neg
    minimos <- min.pos + max.neg
    
    names.sal <- paste0("Dim.", t(indx))
    sal.sym <- as.data.frame(matrix(rep(0, n * 2 * m), nrow = n))
    colnames(sal.sym)[indx.min] <- names.sal
    colnames(sal.sym)[indx.max] <- paste0(names.sal, ".1")
    
    sal.sym[, indx.max] <- maximos
    sal.sym[, indx.min] <- minimos
    
    row.names(sal.sym) <- data.sym$sym.obj.names
    
    return(list(Sym.Components = data.frame.to.RSDA.inteval.table.j(sal.sym), pos.coord.eigen = vector.propios.pos, 
        neg.coord.eigen = vector.propios.neg, mean.vertex = medias, sd.vertex = desviaciones))
}
