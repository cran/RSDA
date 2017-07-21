#' Generate a symbolic data table
#' @aliases  classic.to.sym
#' @name classic.to.sym
#' @author Olger Calderon and Roberto Zuniga.
#' @description Generate a symbolic data table from a classic data table.
#' @usage classic.to.sym(dataTable, concept, variables, variables.types)
#' @param dataTable This is the classic data table.
#' @param concept These are the variable that we are going to use a concepts.
#' @param variables These are the variables that we want to inlude in the symbolic data table.
#' @param variables.types These are the variables symbolic types (continuos, interval, set or histograma)
#' of the variables that we want to inlude in the symbolic data table.
#'
#' @return The symbolic data table.
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @seealso read.sym.table
#' @examples
#' data(ex1_db2so)
#' ex1 <- ex1_db2so
#' result <- classic.to.sym(ex1, concept=c('state', 'sex'),
#'                          variables=c('county', 'group', 'age','age'),
#'                          variables.types=c('$C', '$I', '$H', '$S'))
#' result
#' @keywords symbolic data table
#' @export
#' @import sqldf
#'
classic.to.sym <- function(dataTable, concept, variables, variables.types) {
    
    if (length(variables) != length(variables.types)) {
        stop("variables and variables.types must have the same length")
    }
    
    dataTable <- dataTable[, which(colnames(dataTable) %in% c(variables, concept))]
    
    concept <- paste0("[", concept, "]")
    variables <- paste0("[", variables, "]")
    conceptColumns <- paste(concept, collapse = ", ")
    
    sqldf()
    
    sqldf(paste0("CREATE INDEX main.concept_index ON dataTable (", conceptColumns, 
        ")"))
    
    sym.obj <- sqldf(paste0("SELECT DISTINCT ", conceptColumns, " FROM main.dataTable ORDER BY ", 
        conceptColumns))
    sym.obj.names <- do.call("paste", args = c(sym.obj, sep = "."))
    
    symObjTable <- data.frame(SymObjNames = sym.obj.names)
    
    sqldf("SELECT SymObjNames FROM symObjTable")
    
    meta.data <- list()
    for (i in 1:length(variables)) {
        n <- as.numeric(stringr::str_extract(variables.types[[i]], "[[:digit:]]"))
        variables.types[[i]] <- stringr::str_replace(variables.types[[i]], "[[:digit:]]", 
            "")
        switch(variables.types[[i]], `$C` = {
            meta.data[[i]] <- process.continuum.variable(variables[[i]], conceptColumns)
        }, `$I` = {
            meta.data[[i]] <- process.interval.variable(variables[[i]], conceptColumns)
        }, `$H` = {
            meta.data[[i]] <- process.histogram.variable(variables[[i]], concept, dataTable, 
                n)
        }, `$M` = {
            meta.data[[i]] <- process.modal.variable(variables[[i]], concept, sym.obj.names)
        }, `$S` = {
            meta.data[[i]] <- process.set.variable(variables[[i]], concept, sym.obj.names)
        }, stop("Invalid variable type"))
    }
    
    sqldf()
    
    meta.data <- data.frame(meta.data, check.names = F)
    rownames(meta.data) <- sym.obj.names
    
    colnames(meta.data)[colnames(meta.data) == "'$C'"] <- "$C"
    colnames(meta.data)[colnames(meta.data) == "'$I'"] <- "$I"
    new.sym <- newSobject(meta.data)
    class(new.sym) <- c("list", "sym.data.table")
    return(new.sym)
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
    cat("# A Symbolic Data Table : ", nrow(x$meta), " x ", length(x$sym.var.starts), 
        "\n")
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
        caption <- paste0("A Symbolic Data Table : ", nrow(x$meta), " x ", length(x$sym.var.starts), 
            "\n")
    }
    pander::pander(format_sym_vars(x), caption, ...)
}



#' format.sym.vars
#' @keywords internal
format_sym_vars <- function(x) {
    out.table <- c()
    for (i in seq_len(x$M)) {
        var.type <- x$sym.var.types[i]
        var <- switch(var.type, `$C` = format_continuous_var(x, i), `$M` = format_modal_var(x, 
            i), `$H` = format_hist_var(x, i), `$S` = format_set_var(x, i), `$I` = format_interval_var(x, 
            i))
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
    out <- apply(var, 1, function(x) {
        paste0(names(x), ":", (round(x, 2) * 100), "%", collapse = " ")
    })
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
