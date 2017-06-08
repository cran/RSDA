#' process.histogram.variable
#' @keywords internal
calculate.probs <- function(x, breaks.) {
    h. <- hist(x, plot = F, breaks = breaks., right = F)
    i <- 1:(length(h.$breaks) - 1)
    j <- 2:(length(h.$breaks))
    prob <- h.$counts/sum(h.$counts)
    d <- data.frame(to = h.$breaks[i], from = h.$breaks[j], prob = prob)
    return(d)
}
