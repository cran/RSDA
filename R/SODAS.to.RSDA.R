#' XML SODAS files to RSDA files.
#' @name SODAS.to.RSDA
#' @aliases SODAS.to.RSDA
#' @author Olger Calderon and Roberto Zuniga.
#' @description To convert XML SODAS files to RSDA files.
#' @param XMLPath Disk path where the SODAS *.XML file is.
#' @param labels If we want to include SODAS XML files lebels in RSDA file.
#'
#' @return A RSDA symbolic data file.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @seealso SDS.to.RSDA
#' @examples
#' \dontrun{
#' # We can read the file directly from the SODAS XML file as follows:
#'# abalone<-SODAS.to.RSDA('C:/Program Files (x86)/DECISIA/SODAS version 2.0/bases/abalone.xml)
#'# We can save the file in CSV to RSDA format as follows:
#'# write.sym.table(sodas.ex1, file='abalone.csv', sep=';',dec='.', row.names=TRUE,
#'#               col.names=TRUE)
#'# We read the file from the CSV file,
#'# this is not necessary if the file is read directly from
#'# XML using SODAS.to.RSDA as in the first statement in this example.
#'data(abalone)
#'res<-sym.interval.pca(abalone,'centers')
#'sym.scatterplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
#'                labels=TRUE,col='red',main='PCA Oils Data')
#'sym.scatterplot3d(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
#'                  sym.var(res$Sym.Components,3),color='blue',main='PCA Oils Data')
#'sym.scatterplot.ggplot(sym.var(res$Sym.Components,1),sym.var(res$Sym.Components,2),
#'                       labels=TRUE)
#'sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' }
#' @keywords Symbolic data table
#' @export
#' @importFrom XML xmlInternalTreeParse getNodeSet xmlGetAttr xpathSApply xmlValue xmlName
#'
SODAS.to.RSDA <- function(XMLPath, labels = T) {
    parsed.xml <- xmlInternalTreeParse(XMLPath)
    
    containsNode <- getNodeSet(parsed.xml, "/assofile/contains")
    if (length(containsNode) == 0) 
        stop("No 'contains' tag is present in the XML file")
    containsNode <- containsNode[[1]]
    if (xmlGetAttr(containsNode, "INDIVIDUALS") != "YES" || xmlGetAttr(containsNode, 
        "VARIABLES") != "YES" || xmlGetAttr(containsNode, "RECTANGLE_MATRIX") != "YES") 
        stop("Insufficient data in XML file")
    
    if (labels) {
        sym.obj.names <- xpathSApply(parsed.xml, "/assofile/individus/stindiv/label", 
            xmlValue)
        variables.names <- xpathSApply(parsed.xml, "/assofile/variables/stvar/ident/label", 
            xmlValue)
    } else {
        sym.obj.names <- xpathSApply(parsed.xml, "/assofile/individus/stindiv/name", 
            xmlValue)
        variables.names <- xpathSApply(parsed.xml, "/assofile/variables/stvar/ident/name", 
            xmlValue)
    }
    
    variables.types <- xpathSApply(parsed.xml, "/assofile/variables/stvar/*[2]", xmlName)
    result <- data.frame(row.names = sym.obj.names)
    number.of.rows <- nrow(result)
    
    for (i in 1:length(variables.types)) {
        
        cat(paste0("Processing variable ", i, ": ", variables.names[[i]], "\n"))
        
        switch(variables.types[[i]], `inter-cont` = {
            result <- cbind(result, process.inter.cont.variable(number.of.rows, parsed.xml, 
                i, variables.names[[i]]))
        }, continue = {
            result <- cbind(result, process.continue.variable(number.of.rows, parsed.xml, 
                i, variables.names[[i]]))
        }, nominal = {
            result <- cbind(result, process.nominal.variable(labels, number.of.rows, 
                parsed.xml, i, variables.names[[i]]))
        }, mult_nominal = {
            result <- cbind(result, process.mult.nominal.variable(labels, number.of.rows, 
                parsed.xml, i, variables.names[[i]]))
        }, mult_nominal_Modif = {
            type.modif <- xpathSApply(parsed.xml, paste0("/assofile/variables/stvar[", 
                i, "]/mult_nominal_Modif/type_modif"), xmlValue)
            if (type.modif != "proba") cat(paste0("Unsupported type.modif in mult_nominal_Modif variable: "), 
                type.modif, "\n") else result <- cbind(result, process.mult.nominal.modif.variable(labels, 
                number.of.rows, parsed.xml, i, variables.names[[i]]))
        }, cat(paste0("Variable type not supported:"), variables.types[[i]], "\n"))
    }
    out. <- newSobject(result)
    class(out.) <- "sym.data.table"
    return(out.)
}
