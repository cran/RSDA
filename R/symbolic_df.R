#' Check duplicated names in a quo
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom rlang get_expr
check_quo_duplicated_names <- function(x) {
  duplicated_names <- which(duplicated(names(x), fromLast = T))
  fs <- purrr::map_chr(x[duplicated_names], ~ as.character(rlang::get_expr(.)[[1]]))
  names(x)[duplicated_names] <- paste0(names(x)[duplicated_names], "_", fs)
  return(x)
}

#' Generate a symbolic data frame
#' @description Generate a symbolic data table from a classic data table.
#' @param x A data.frame.
#' @param concept These are the variable that we are going to use a concepts.
#' @param variables These are the variables that we want to include in the symbolic data table.
#' @param default.numeric function to use for numeric variables
#' @param default.categorical function to use for categorical variables
#' @param ... A vector with names and the type of symbolic data to use, the available types are type_histogram (), type_continuous (), type.set (), type.modal (), by default type_histogram () is used for numeric variables and type_modal () for the categorical variables.
#' @return a [tibble][tibble::tibble-package]
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @importFrom tidyselect vars_select everything
#' @importFrom rlang quos !! !!! syms
#' @importFrom dplyr group_by summarise select summarise_all left_join ungroup mutate mutate_if '%>%'
#' @importFrom  purrr compose
#' @importFrom forcats fct_unify
#' @export
classic.to.sym <- function(x = NULL,
                           concept = NULL,
                           variables = tidyselect::everything(),
                           default.numeric = sym.interval,
                           default.categorical = sym.modal,
                           ...) {
  .concept <- tidyselect::vars_select(colnames(x), !!rlang::enquo(concept))
  col.types <- rlang::quos(...)
  col.types <- check_quo_duplicated_names(col.types)
  var.names <- tidyselect::vars_select(colnames(x), c(!!rlang::enquo(concept), !!rlang::enquo(variables)))
  var.names <- var.names[!var.names %in% names(col.types)]

  out1 <- x %>%
    dplyr::group_by(!!!rlang::syms(.concept)) %>%
    dplyr::summarise(!!!col.types)

  default_function <- function(x) {
    if (is.numeric(x)) {
      return(default.numeric(x))
    }
    return(default.categorical(x))
  }

  out2 <- x %>%
    dplyr::select(var.names) %>%
    dplyr::group_by(!!!dplyr::syms(.concept)) %>%
    dplyr::summarise_all(default_function)

  out <- dplyr::left_join(out1, out2, by = .concept) %>%
    dplyr::ungroup()

  concepts <- apply(out[, .concept], 1, function(x) paste0(x, collapse = ":"))
  out <- dplyr::select(out, -.concept)

  class(out) <- c("symbolic_tbl", class(out))
  attr(out, "row.names") <- concepts
  return(out)
}

#' Extract meta data
#' @keywords internal
#' @importFrom purrr map
#' @import tibble
#' @importFrom utils getFromNamespace
#' @export
`[.symbolic_tbl` <- function(x, i, j, drop = FALSE) {
  warningc <- getFromNamespace("warningc", "tibble")
  check_names_df <- getFromNamespace("check_names_df", "tibble")
  fast_nrow <- getFromNamespace("fast_nrow", "tibble")
  vec_restore_tbl_df_with_nr <- getFromNamespace("vec_restore_tbl_df_with_nr", "tibble")
  has_rownames <- getFromNamespace("has_rownames", "tibble")
  string_to_indices <- getFromNamespace("string_to_indices", "tibble")
  subset_rows <- getFromNamespace("subset_rows", "tibble")

  n_real_args <- nargs() - !missing(drop)
  if (n_real_args <= 2L) {
    if (!missing(drop)) {
      warningc("drop ignored")
    }
    if (missing(i)) {
      return(x)
    }
    i <- check_names_df(i, x)
    result <- .subset(x, i)
    nr <- fast_nrow(x)
    out <- vec_restore_tbl_df_with_nr(result, x, nr)
    attr(out, "row.names") <- attr(x, "row.names")[i]
    return(out)
  }
  if (missing(j)) {
    result <- x
  }
  else {
    j <- check_names_df(j, x)
    result <- .subset(x, j)
  }
  if (missing(i)) {
    nr <- fast_nrow(x)
  }
  else {
    if (is.logical(i) && !(length(i) %in% c(1L, fast_nrow(x)))) {
      warningc("Length of logical index must be 1", if (fast_nrow(x) !=
        1) {
        paste0(" or ", fast_nrow(x))
      }, ", not ", length(i))
    }
    if (length(result) == 0) {
      nr <- length(attr(x, "row.names")[i])
    }
    else {
      if (is.character(i)) {
        if (has_rownames(x)) {
          i <- match(i, rownames(x))
        }
        else {
          i <- string_to_indices(i)
        }
      }
      result <- purrr::map(result, subset_rows, i)
      nr <- NROW(result[[1]])
    }
  }
  if (drop) {
    if (length(result) == 1L) {
      return(result[[1L]])
    }
  }
  out <- vec_restore_tbl_df_with_nr(result, x, nr)
  attr(out, "row.names") <- attr(x, "row.names")[i]
  return(out)
}

#' Extract meta data
#' @keywords internal
extract_meta <- function(x, name = NA) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    out <- x
    out <- data.frame("$C" = "$C", out, check.names = F)
    colnames(out) <- c("$C", name)
    return(out)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    out <- as.data.frame(x)
    colnames(out) <- c(name, name)
    out <- data.frame("$I" = "$I", out, check.names = F)
    return(out)
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    out <- as.data.frame(x)
    out <- data.frame("$H" = "$H", hist = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    out <- as.data.frame(x)
    out <- data.frame("$M" = "$M", modal = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_set")) {
    out <- as.data.frame(x)
    out <- data.frame("$S" = "$S", modal = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }
}

#' Extract data
#' @keywords internal
extract_data <- function(x, name = NA) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    out <- data.frame(x, check.names = F)
    colnames(out) <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    out <- as.data.frame(x)
    colnames(out) <- c(name, name)
    return(out)
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    out <- as.data.frame(x)
    return(out)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    out <- as.data.frame(x)
    return(out)
  }

  if (any(class(x) %in% "symbolic_set")) {
    out <- as.data.frame(x)
    return(out)
  }
}

#' Extract length
#' @keywords internal
var.length <- function(x) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    return(1)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    return(2)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    return(length(x[[1]]$var))
  }

  if (any(class(x) %in% "symbolic_set")) {
    return(length(levels(x[[1]])))
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    return(length(x[[1]]$breaks) - 1)
  }
}

#' to.v2
#' @keywords internal
to.v2 <- function(x) {
  out <- list()
  out$N <- nrow(x)
  out$M <- ncol(x)
  out$sym.obj.names <- attr(x, "row.names")
  out$sym.var.names <- colnames(x)

  meta <- lapply(colnames(x), function(y) {
    extract_meta(x[y][[1]], name = y)
  })
  meta <- do.call("cbind", meta)
  rownames(meta) <- attr(x, "row.names")

  types <- which(stringr::str_detect(colnames(meta), "\\$\\w"))
  out$sym.var.types <- as.character(na.omit(stringr::str_extract(colnames(meta), "\\$\\w")))
  out$sym.var.length <- unname(sapply(x[, out$sym.var.names], var.length))
  out$sym.var.starts <- ifelse(out$sym.var.types %in% c("$I", "$C"), 1, 2) + types
  out$meta <- meta
  out$data <- lapply(colnames(x), function(y) {
    extract_data(x[y][[1]], name = y)
  })
  out$data <- do.call("cbind", out$data)
  rownames(out$data) <- attr(x, "row.names")
  class(out) <- "sym.data.table"
  return(out)
}

#' to.v3
#' @keywords internal
to.v3 <- function(x) {
  out <- tibble::tibble(.rows = x$N)
  for (i in seq_len(x$M)) {
    if (x$sym.var.types[i] == "$I") {
      values <- x[, i]$data
      new_interval <- new.sym.intreval(values[, 1], values[, 2])
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_interval)
    }

    if (x$sym.var.types[i] == "$S") {
      values <- x[, i]$data
      new_set <- c()
      for (.i in seq_len(nrow(values))) {
        .f <- factor(colnames(values)[as.logical(values[.i, ])],
          levels = colnames(values)
        )
        new_set <- vctrs::vec_c(new_set, new.sym.set(.f))
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_set)
    }

    if (x$sym.var.types[i] == "$M") {
      values <- x[, i]$data
      new_modal <- c()
      for (.i in seq_len(nrow(values))) {
        .m <- list(
          var = colnames(values),
          prop = as.numeric(values[.i, ])
        )
        .m <- vctrs::new_vctr(list(.m), class = "symbolic_modal")
        new_modal <- vctrs::vec_c(new_modal, .m)
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_modal)
    }

    if (x$sym.var.types[i] == "$H") {
      values <- x[, i]$data
      .breaks <- as.numeric(unique(unlist(stringr::str_extract_all(colnames(values), "(\\d+\\.\\d+)"))))
      new_histogram <- c()
      for (.i in seq_len(nrow(values))) {
        .h <- list(
          breaks = .breaks,
          props = as.numeric(values[.i, ])
        )
        .h <- vctrs::new_vctr(list(.h), class = "symbolic_histogram")
        new_histogram <- vctrs::vec_c(new_histogram, .h)
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_histogram)
    }
    if (x$sym.var.types[i] == "$C") {
      values <- x[, i]$data
      values <- as.numeric(values[[1]])
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := values)
    }
  }
  attr(out, "row.names") <- x$sym.obj.names
  class(out) <- c("symbolic_tbl", class(out))
  return(out)
}
