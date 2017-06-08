is.vertex <- function(optim.matrix, vertex.matrix) {
    optim.matrix <- as.data.frame(optim.matrix)
    m <- dim(optim.matrix)
    cols <- 1:m[2]
    cols.names <- paste0("V", cols)
    colnames(optim.matrix) <- colnames(vertex.matrix) <- cols.names
    T1.cols <- paste0("T1.", cols.names)
    T2.cols <- paste0("T2.", cols.names)
    
    join <- paste(paste(T1.cols, "=", T2.cols), collapse = " AND ")
    
    query <- "SELECT T1.* FROM [optim.matrix] T1 INNER JOIN [vertex.matrix] T2 ON "
    query <- paste0(query, join)
    sal <- sqldf(query)
    return(m[1] == dim(sal)[1])
}
