#' dist.modal
#' @keywords internal
dist.mod <- function(x, y) {
    sum(sqrt(x * y))
}


#' Distance matrix for symbolic data
#'
#' @param sym.data Symbolic data table
#'
#' @return return an object of class 'dist
#' @export
dist.modal <- function(sym.data) {
    n <- sym.data$N
    m <- sym.data$M
    out <- matrix(0, nrow = n, ncol = n)
    for (i in seq_len(m)) {
        for (j in seq_len(n)) {
            for (k in seq_len(n)) {
                out[j, k] <- out[j, k] + dist.mod(sym.data[j, i]$data, sym.data[k, 
                  i]$data)
            }
        }
    }
    out <- dist(1 - (out/m))
    return(out)
}
