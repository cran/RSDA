#' Generate a symbolic data table
#' @aliases  classic.to.sym
#' @name classic.to.sym
#' @author Carlos Aguero.
#' @description Generate a symbolic data table from a classic data table.
#' @usage classic.to.sym(data, concept, variables, variables.types)
#'
#' @param data A data.frame.
#' @param concept These are the variable that we are going to use a concepts.
#' @param variables These are the variables that we want to include in the symbolic data table.
#' @param variables.types A vector with names and the type of symbolic data to use, the available types are type_histogram (), type_continuous (), type.set (), type.modal (), by default type_histogram () is used for numeric variables and type_modal () for the categorical variables.
#'
#' @return The symbolic data table.
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @seealso read.sym.table
#' @examples
#' result <- classic.to.sym(data = iris,
#'               concept = "Species",
#'               variables = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
#' result
#'
#' result <- classic.to.sym(data = iris,
#' concept = "Species", # concepto
#' variables = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width), # variable a utilizar
#' variables.types = c(Sepal.Length = type.interval(), # tipo para cada una de las variable
#'              Sepal.Width = type.interval(),
#'              Petal.Length = type.interval(),
#'              Petal.Width = type.interval()))
#' result
#' @keywords symbolic data table
#' @export
#' @import sqldf
#'
classic.to.sym <- function(data = NULL, concept = NULL, variables = NULL, variables.types = NULL) {
  variables <- enexpr(variables)
  data <- add_concept(
    x = data,
    concept = concept,
    col.names = !!variables
  )
  vars.selected <- c(names(variables.types), colnames(data)[!colnames(data) %in% c(names(variables.types), "concept")])
  meta.data <- create.meta.data(data, variables.types)

  data. <- remove_meta_info(meta.data)
  symbolic.object <- list()
  symbolic.object$N <- nrow(data.)
  symbolic.object$M <- length(vars.selected)

  symbolic.object$sym.obj.names <- rownames(meta.data)
  symbolic.object$sym.var.names <- vars.selected
  symbolic.object$sym.var.types <- extract_sym_types(meta.data)

  length_starts <- extract_length_starts(meta.data)
  symbolic.object$sym.var.length <- length_starts$lengths
  symbolic.object$sym.var.starts <- length_starts$starts

  symbolic.object$meta <- meta.data
  symbolic.object$data <- data.
  class(symbolic.object) <- "sym.data.table"
  return(suppressWarnings(symbolic.object))
}

#' Symbolic interval type
#' @keywords internal
type.interval <- function() {
  interval <- function(x) {
    var.name <- colnames(x[, 2])
    value <- x[, 2]
    min_name <- paste0(var.name, ".interval.min")
    max_name <- paste0(tolower(var.name), ".interval.max")
    df <- x %>% dplyr::group_by(concept) %>% dplyr::summarise("$I", min(!!sym(var.name)), max(!!sym(var.name)))
    # df <- data_frame(unique(x$concept),"$I",min(value),max(value))
    colnames(df) <- c("concept", "$I", min_name, max_name)
    df
  }
  class(interval) <- c("funtion", "fun_interval")
  interval
}

#' Symbolic continuous type
#' @keywords internal
type.continuous <- function(.fun = mean) {
  continuous <- function(x) {
    .fun <- .fun
    var.name <- colnames(x[, 2])
    value <- x[, 2][[1]]
    var_name <- paste0(tolower(var.name), ".continuous")
    df <- x %>% dplyr::group_by(concept) %>% dplyr::summarise("$C", round(.fun(!!sym(var.name)), 2))
    colnames(df) <- c("concept", "$C", var_name)
    df
  }
  class(continuous) <- c("funtion", "fun_continuous")
  continuous
}

#' Symbolic histogram type
#' @keywords internal
type.histogram <- function(bins = NA_integer_) {
  histogram <- function(x) {
    bins. <- bins
    all <- x[, 2][[1]]
    if (is.na(bins.)) {
      breaks <- hist(all, plot = F, right = F)$breaks
    } else {
      breaks <- hist(all, plot = F, right = F, breaks = bins)$breaks
    }
    i <- 1:(length(breaks) - 1)
    j <- 2:(length(breaks))
    bins <- gsub("\\s", "", paste(format(breaks[i], scientific = F), format(breaks[j], scientific = F), sep = ","))
    bins <- c(paste0("[", bins[-length(bins)], ")"), paste0("[", bins[length(bins)], "]"))

    calculate.hist <- x %>%
      dplyr::group_by(concept) %>%
      dplyr::do(hist = hist(.[, 2][[1]], plot = F, right = F, breaks = breaks)$counts)
    calculate.probs <- calculate.hist %>%
      split(calculate.hist$concept) %>%
      purrr::map_dfr(~ as.data.frame(t(round(.$hist[[1]] / sum(.$hist[[1]]), 2))))

    var_name <- paste0(tolower(colnames(x[, 2])), ".hist")
    df.out <- data.frame(concept = calculate.hist$concept, `$H` = "$H", var_name = length(bins), calculate.probs)
    colnames(df.out) <- c("concept", "$H", var_name, bins)
    tibble::as_data_frame(df.out)
  }
  class(histogram) <- c("funtion", "fun_histogram")
  histogram
}

#' Symbolic set type
#' @keywords internal
type.set <- function() {
  set <- function(x) {
    if (!is.factor(x[, 2, drop = T])) {
      # se combierte la variable a factor
      levels. <- unique(x[2][[1]])
      levels. <- levels.[order(levels.)]
      new.factor <- factor(x = x[2][[1]], levels = levels.)
      x[, 2] <- new.factor
    }

    # se calcula la frecuencia para de las categorias para cada uno de los conceptos
    split.df <- x %>% split(x$concept)
    props.df <- split.df %>% purrr::map_dfr(~ as.data.frame(t(ifelse(as.numeric(table(.[, 2])) > 0L, 1L, 0L))))

    # se completa el data.frame con tipo, largo y nombres de columnas
    var.name <- paste0(gsub("\\s", ".", tolower(colnames(x[, 2]))), ".set")
    out.df <- cbind(names(split.df), "$S", length(levels(x[, 2, drop = T])), props.df)
    colnames(out.df) <- c("concept", "$S", var.name, levels(x[, 2, drop = T]))
    out.df
  }
  class(set) <- c("funtion", "fun_set")
  set
}

#' Symbolic modal type
#' @keywords internal
type.modal <- function() {
  modal <- function(x) {
    # se calcula la frecuencia para de las categorias para cada uno de los conceptos
    split.df <- x %>% split(x$concept)
    props.df <- split.df %>% purrr::map_dfr(~ as.data.frame(t(as.numeric(prop.table(table(.[, 2]))))))

    # se completa el data.frame con tipo, largo y nombres de columnas
    var.name <- paste0(gsub("\\s", ".", tolower(colnames(x[, 2]))), ".modal")
    out.df <- cbind(names(split.df), "$M", length(levels(x[, 2][[1]])), props.df)
    colnames(out.df) <- c("concept", "$M", var.name, levels(x[, 2][[1]]))
    out.df
  }
  class(modal) <- c("funtion", "fun_modal")
  modal
}

#' Symbolic guest type
#' @keywords internal
type.symbolic <- function() {
  function(x) {
    type.numeric <- is.numeric(x[, 2][[1]])
    if (!type.numeric) {
      return(type.modal()(x))
    } else {
      return(type.interval()(x))
    }
  }
}

#' add the concept
#' @keywords internal
add_concept <- function(x = NULL, concept = NULL, col.names = NULL) {
  x <- as_data_frame(x)
  col.names <- enexpr(col.names) # enexpr : captura el objeto y el enviroment
  concept <- syms(concept) # sym : convierte a simbolo
  var.names <- names(x)
  df <- x[, tidyselect::vars_select(var.names, !!col.names, -c(!!!concept))]
  var.concepts <- x[, tidyselect::vars_select(var.names, !!!concept)]
  # var.concepts <- select(x, !!!concept)
  # df <- select(x,!!col.names)
  col_concept <- apply(var.concepts, 1, function(x) paste0(x, collapse = ":"))
  df <- cbind(concept = col_concept, df)
  as_data_frame(df)
}

#' Extract meta data
#' @keywords internal
create.meta.data <- function(x = NULL, col.types = NULL) {
  vars.selected <- c(names(col.types), colnames(x)[!colnames(x) %in% c(names(col.types), "concept")])
  vars.nof <- colnames(x)[!colnames(x) %in% c(names(col.types), "concept")]
  funs. <- c(col.types, purrr::map(seq_len(length(vars.nof)), ~ type.symbolic())) # cambio de nombre para uso interno

  meta.data <- list()
  for (i in seq_len(length(vars.selected))) {
    meta.data[[i]] <- x %>% dplyr::select(concept, !!sym(vars.selected[i])) %>% funs.[[i]]()
  }

  suppressWarnings(meta.data <- Reduce(
    function(dtf1, dtf2) merge(dtf1, dtf2, by = "concept", suffixes = c("", ""), all = TRUE),
    meta.data
  ))
  meta.data <- as.data.frame(meta.data)
  rownames(meta.data) <- meta.data$concept
  meta.data$concept <- NULL
  return(meta.data)
}

#' Remove meta data
#' @keywords internal
remove_meta_info <- function(meta.data = NULL) {
  meta.data <- as_data_frame(meta.data, validate = F)
  vars.remove1 <- which(colnames(meta.data) %in% c("$I", "$C"))
  vars.remove2 <- which(colnames(meta.data) %in% c("$H", "$M", "$S"))
  var.remove <- c(vars.remove1, vars.remove2, vars.remove2 + 1)
  meta.data[, -var.remove]
}

#' Get symbolic types
#' @keywords internal
extract_sym_types <- function(meta.data) {
  vars.remove <- which(colnames(meta.data) %in% c("$I", "$C", "$H", "$M", "$S"))
  colnames(meta.data)[vars.remove]
}

#' Get symbolic data length
#' @keywords internal
extract_length_starts <- function(meta.data) {
  syms <- extract_sym_types(meta.data)
  l <- which(colnames(meta.data) %in% c("$I", "$H", "$C", "$M", "$S"))
  length_and_starts <- list()
  length_and_starts$starts <- ifelse(syms %in% c("$I", "$C"), l + 1, ifelse(syms %in% c("$H", "$M", "$S"), l + 2, NA))
  length_and_starts$lengths <- ifelse(syms %in% c("$C"), 1, ifelse(syms %in% c("$I"), 2, ifelse(syms %in% c("$H", "$S", "$M"), as.numeric(meta.data[1, l + 1]), NA)))
  return(length_and_starts)
}

#' Extract or replace parts of a Symbolic Data Table
#' @keywords internal
#' @export
`[.sym.data.table` <- function(x, i, j) {
  out <- x
  if (!missing(j)) {
    if (any(j > length(out$sym.var.names))) {
      stop("undefined columns selected")
    }

    meta.data <- data.frame(row.names = out$sym.obj.names, check.names = F)
    real.data <- data.frame(row.names = out$sym.obj.names, check.names = F)
    new.var.l <- c()
    new.var.s <- c()

    if (any(j < 0)) {
      j <- seq_along(out$sym.var.names)[j]
    }

    for (columns in j) {
      for (column in columns) {
        type <- out$sym.var.types[column]
        var.l <- out$sym.var.length[column]
        var.s <- out$sym.var.starts[column]

        if (type %in% c("$H", "$M", "$S")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 3)

          data. <- out$meta[, (var.s - 2):(var.s + (var.l - 1))]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[, (var.s):(var.s + (var.l - 1))]
          real.data <- cbind(real.data, data.)

          new.var.l <- c(new.var.l, ncol(data.))
        }
        if (type %in% c("$I")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 2)
          new.var.l <- c(new.var.l, 2)

          data. <- out$meta[, (var.s - 1):(var.s + (var.l - 1))]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[, (var.s):(var.s + 1)]
          real.data <- cbind(real.data, data.)
        }
        if (type %in% c("$C")) {
          new.var.s <- c(new.var.s, ncol(meta.data) + 2)
          new.var.l <- c(new.var.l, 1)
          data. <- out$meta[, (var.s - 1):(var.s)]
          meta.data <- cbind(meta.data, data.)

          data. <- out$meta[var.s]
          real.data <- cbind(real.data, data.)
        }
      }
    }
    out$meta <- meta.data
    out$data <- real.data
    out$sym.var.names <- out$sym.var.names[j]
    out$sym.var.types <- out$sym.var.types[j]
    out$sym.var.length <- out$sym.var.length[j]
    out$sym.var.starts <- out$sym.var.starts[j]
    out$sym.var.starts <- new.var.s
    out$sym.var.length <- new.var.l
  }
  if (!missing(i)) {
    if (any(i > length(out$sym.obj.names))) {
      stop("undefined rows selected")
    }
    out$sym.obj.names <- out$sym.obj.names[i]
    out$data <- out$data[i, ]
    out$meta <- out$meta[i, ]
  }
  out$N <- length(out$sym.obj.names)
  out$M <- length(out$sym.var.names)
  out
  return(out)
}


#' Printing Symbolic Data Table
#'
#' @param x Object of class sym.data.table
#' @param ... optional arguments to print o format method
#'
#' @export
print.sym.data.table <- function(x, ...) {
  cat(
    "# A Symbolic Data Table : ", nrow(x$meta), " x ", length(x$sym.var.starts),
    "\n"
  )

  print(format_sym_vars(x), ...)
}

#' Pander method for symbolic data table
#'
#' Prints a symbolic data table in Pandoc's markdown
#' @param x a symbolic data table
#' @param caption caption (string) to be shown under the table
#' @param ... optional parameters passed to raw pandoc.table function
#' @export
pander.sym.data.table <- function(x, caption = attr(x, "caption"), ...) {
  if (is.null(caption)) {
    caption <- paste0(
      "A Symbolic Data Table : ", nrow(x$meta), " x ", length(x$sym.var.starts),
      "\n"
    )
  }
  pander::pander(format_sym_vars(x), caption, ...)
}



#' format.sym.vars
#' @keywords internal
format_sym_vars <- function(x) {
  out.table <- c()
  for (i in seq_len(x$M)) {
    var.type <- x$sym.var.types[i]
    var <- switch(var.type, `$C` = format_continuous_var(x, i), `$M` = format_modal_var(
      x,
      i
    ), `$H` = format_hist_var(x, i), `$S` = format_set_var(x, i), `$I` = format_interval_var(
      x,
      i
    ))
    out.table <- cbind(out.table, var)
  }
  out.table <- data.frame(out.table)
  colnames(out.table) <- x$sym.var.names
  rownames(out.table) <- x$sym.obj.names
  return(out.table)
}

#' format.hist.var
#' @keywords internal
format_hist_var <- function(x, i) {
  var <- x[, i]$meta
  var <- var[, -c(1, 2)]
  out <- apply(var, 1, function(x) spark_bar(as.numeric(x)))
  # out <- apply(var, 1, function(x) {
  #   paste0(names(x), ":", (round(x, 2) * 100), "%", collapse = " ")
  # })
  return(out)
}

#' format.set.var
#' @keywords internal
format_set_var <- function(x, i) {
  var <- x[, i]$meta
  var <- var[, -c(1, 2)]
  k <- colnames(var)
  out <- apply(var, 1, function(x) {
    paste0("{", paste0(k[as.logical(x)], collapse = ","), "}")
  })
  return(out)
}


#' format.modal.var
#' @keywords internal
format_modal_var <- function(x, i) {
  var <- x[, i]$meta
  var <- var[, -c(1, 2)]
  out <- apply(var, 1, function(x) {
    paste0(stringr::str_trunc(names(x), 3), ":", (round(x, 2) * 100), "% ", collapse = "")
  })
  return(out)
}

#' format.continuous.var
#' @keywords internal
format_continuous_var <- function(x, i) {
  var <- x[, i]$meta
  var <- var[, -1]
  out <- as.character(var)
  return(out)
}


#' format.interval.var
#' @keywords internal
format_interval_var <- function(x, i) {
  var <- x[, i]$meta
  var <- var[, -1]
  out <- paste0("[", round(var[, 1], 2), ",", round(var[, 2], 2), "]")
  return(out)
}

#' format.histogram bars.
#' @keywords internal
spark_bar <- function(x, safe = T) {
  stopifnot(is.numeric(x))

  bars <- vapply(0x2581:0x2588, intToUtf8, character(1))
  if (safe) {
    bars <- bars[-c(4, 8)]
  }

  factor <- cut(
    x,
    breaks = seq(0, 1, length = length(bars) + 1),
    labels = bars,
    include.lowest = TRUE
  )
  chars <- as.character(factor)
  chars[is.na(chars)] <- bars[length(bars)]

  return(paste0(chars, collapse = ""))
}
