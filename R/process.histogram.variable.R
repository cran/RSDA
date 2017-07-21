#' process.histogram.variable
#' @import dplyr tidyr stringr lazyeval
#' @keywords internal
process.histogram.variable <- function(variableName, concept, dataTable, n) {
  as_quosure <- function(strs) rlang::parse_quosures(paste(strs, collapse=";"))
  concept. <- stringr::str_replace_all(concept, "[\\[\\]]", "")
  variable.name. <- stringr::str_replace_all(variableName, "[\\[\\]]","")
  n.breaks <- n
  n.breaks[is.na(n.breaks)] <- 5
  breaks. <- hist(dataTable[, variable.name.], plot = F, right = F,breaks = n.breaks)$breaks
  cal.prop <- rlang::quo(calculate.probs(x = .[[!!variable.name.]], breaks. = !!breaks.))

  data. <- dataTable %>% dplyr::group_by(!!!as_quosure(concept.)) %>% dplyr::do(!!cal.prop)

  data. <- data. %>% tidyr::unite("interval",.data$to, .data$from, sep = ",") %>%
    dplyr::mutate(interval = c(paste0("[",head(.data$interval, n() - 1), ")"), .data$interval[n()])) %>%
    dplyr::mutate(interval = c(.data$interval[1:(n() -1)], paste0("[", .data$interval[n()], "]")))

  order. <- unique(data.$interval)
  data. <- data. %>% tidyr::spread(interval,prob, fill = 0, convert = F)
  data. <- data.[, -1]
  data. <- data.[, order.]

  type <- data.frame(`$H` = rep("$H", nrow(data.)), check.names = F)
  type.length <- data.frame(rep(ncol(data.), nrow(data.)), check.names = F)
  colnames(type.length) <- variable.name.
  data. <- cbind(type, type.length, data.)
  return(data.)
}
