RSDA.to.latex <- function(sym.data) {
    return(xtable(generate.sym.table(sym.data)))
}
