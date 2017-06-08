whiskers <- function(from, to, col = "red") {
    segments(from[, 1], from[, 2], to[, 1], to[, 2], col = col)
}
