#' @name RSDA
#' @aliases RSDA
#' @docType package
#' @title R to Symbolic Data Analysis
#' @author Oldemar Rodriguez Rojas \cr
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#' @description
#' This work is framed inside the Symbolic Data Analysis (SDA). The objective of this
#' work is to implement in R to the symbolic case certain techniques of the
#' automatic classification, as well as some lineal models. These
#' implementations will always be made following two fundamental principles in
#' Symbolic Data Analysis like they are: Classic Data Analysis should always be
#' a case particular case of the Symbolic Data Analysis and both, the exit as
#' the input in an Symbolic Data Analysis should be symbolic. We implement
#' for variables of type interval the mean, the median, the mean of the
#' extreme values, the standard deviation, the deviation quartil, the
#' dispersion boxes and the correlation also three new methods are also
#' presented to carry out the lineal regression for variables of type interval.
#' We also implement in this R package the method of Principal Components
#' Analysis in two senses: First, we propose three ways to project the
#' interval variables in the circle of correlations in such way
#' that is reflected the variation or the inexactness of the variables. Second,
#' we propose an algorithm to make the Principal Components Analysis for
#' variables of type histogram. We implement a method for multidimensional
#' scaling of interval data, denominated INTERSCAL.
#' @details
#' \tabular{ll}{
#' Package: \tab RSDA\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.3\cr
#' Date: \tab 2017-10-18\cr
#' License: \tab GPL (>=2)\cr
#' }
#' Most of the function of the package stars from a symbolic data table that can be store in
#' a CSV file withe follwing forma: In the first row the labels $C means that follows a
#' continuous variable, $I means an interval variable, $H means a histogram variables and
#' $S means set variable. In the first row each labels should be follow of a name to
#' variable and to the case of histogram a set variables types the names of the modalities
#' (categories) . In data rows for continuous variables we have just one value, for
#' interval variables we have the minimum and the maximum of the interval, for histogram
#' variables we have the number of modalities and then the probability of each modality
#' and for set variables we have the cardinality of the set and next the elements of
#' the set.
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Billard L., Douzal-Chouakria A. and Diday E. (2011)
#' Symbolic Principal Components For Interval-Valued Observations, Statistical Analysis and
#' Data Mining. 4 (2), 229-246. Wiley.
#'
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' Carvalho F., Souza R.,Chavent M., and Lechevallier Y. (2006)
#' Adaptive Hausdorff distances and dynamic clustering of symbolic interval data. Pattern
#' Recognition Letters Volume 27, Issue 3, February 2006, Pages 167-179
#'
#' Cazes P., Chouakria A., Diday E. et Schektman Y. (1997).  Extension de l'analyse en
#' composantes principales a des donnees de type intervalle, Rev. Statistique Appliquee,
#' Vol. XLV Num. 3 pag. 5-24, France.
#'
#' Diday, E., Rodriguez O. and Winberg S. (2000).
#' Generalization of the Principal Components Analysis to Histogram
#' Data, 4th European Conference on Principles and Practice of Knowledge Discovery in
#' Data Bases, September 12-16, 2000, Lyon, France.
#'
#' Chouakria A. (1998)
#' Extension des methodes d'analysis factorialle a des
#' donnees de type intervalle, Ph.D. Thesis, Paris IX Dauphine University.
#'
#' Makosso-Kallyth S. and Diday E. (2012).  Adaptation of interval PCA to symbolic histogram
#' variables, Advances in Data Analysis and Classification July, Volume 6, Issue 2, pp 147-159.

#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @keywords package
#' @docType package
#' @examples
#' data(example3)
#' sym.data<-example3
#' class(sym.data) <- c('sym.data.table')
#' display.sym.table(sym.data)
#' sym.scatterplot(sym.data[,1], sym.data[,4], col='blue',main='Main Title')
#'
#' data(oils)
#' class(oils) <- c('sym.data.table')
#' res<-sym.interval.pca(oils,'centers')
#' sym.scatterplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                 labels=TRUE,col='red',main='PCA Oils Data')
#' sym.scatterplot3d(res$Sym.Components[,1], res$Sym.Components[,2],
#'                   res$Sym.Components[,3],color='blue',main='PCA Oils Data')
#' sym.scatterplot.ggplot(res$Sym.Components[,1],
#'                        res$Sym.Components[,2], labels=TRUE)
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' res<-sym.interval.pca(oils,'classic')
#' plot(res,choix='ind')
#' plot(res,choix='var')
#'
#' data(lynne2)
#' res<-sym.interval.pca(lynne2,'centers')
#' class(res$Sym.Components) <- c('sym.data.table')
#' sym.scatterplot(res$Sym.Components[,1], res$Sym.Components[,2],
#'                 labels=TRUE, col='red',main='PCA Lynne Data')
#' sym.scatterplot3d(res$Sym.Components[,1],res$Sym.Components[,2],
#'                   res$Sym.Components[,3],color='blue', main='PCA Lynne Data')
#' sym.scatterplot.ggplot(res$Sym.Components[,1],res$Sym.Components[,2],
#'                        labels=TRUE)
#' sym.circle.plot(res$Sym.Prin.Correlations)
NULL
utils::globalVariables(c("to", "from", "interval", "n", "prob", "pca.max", "N", "M",
    "<<-", ".", "do", "quo"))

