change.coord <- function(basis, points) {
    dim.points <- dim(points)
    dim.basis <- dim(basis)
    num.coord <- dim.points[2]
    
    matrix.change.coord <- matrix(rep(0, num.coord * num.coord * dim.points[1]), ncol = num.coord * 
        num.coord)
    
    for (i in 1:num.coord) {
        x.i <- points[, i]
        for (j in 1:num.coord) {
            indx <- (i - 1) * num.coord + j
            matrix.change.coord[, indx] <- basis[i, j] * x.i
        }
    }
    return(matrix.change.coord)
}


