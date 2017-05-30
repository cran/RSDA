#' process.histogram.variable
#' @import dplyr tidyr stringr lazyeval
#' @keywords internal
process.histogram.variable <- function(variableName, concept, dataTable) {
    # librerias : dplyr tidyr stringr lazyeval
    concept. <- stringr::str_replace_all(concept, "[\\[\\]]", "")
    variable.name. <- stringr::str_replace_all(variableName, "[\\[\\]]",
        "")

    breaks. <- hist(dataTable[, variable.name.], plot = F, right = F)$breaks

    data. <- dataTable %>% dplyr::group_by_(concept.) %>%
      dplyr::do_(dots. = lazyeval::interp(~calculate.probs(x = .[[variable.name.]],breaks. = breaks.)))%>%
      tidyr::unnest()

    data. <- data. %>% tidyr::unite("interval",
        to, from, sep = ",") %>% dplyr::group_by_(concept.) %>% dplyr::mutate(interval = c(paste0("[",
        head(interval, n() - 1), ")"), interval[n()])) %>%
      dplyr::mutate(interval = c(interval[1:(n() -1)], paste0("[", interval[n()], "]")))

    order. <- unique(data.$interval)
    data. <- data. %>% tidyr::spread(interval, prob, fill = 0, convert = F)
    data. <- data.[, -1]
    data. <- data.[, order.]

    type <- data.frame(`$H` = rep("$H", nrow(data.)), check.names = F)
    type.length <- data.frame(rep(ncol(data.), nrow(data.)), check.names = F)
    colnames(type.length) <- variable.name.
    data. <- cbind(type, type.length, data.)
    return(data.)
}
