## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 5
)
library(RSDA)

## ---- eval=F-------------------------------------------------------------
#  install.packages("RSDA", dependencies=TRUE)

## ---- eval=F-------------------------------------------------------------
#  devtools::install_github("PROMiDAT/RSDA")

## ------------------------------------------------------------------------
ex3 <- read.sym.table('tsym1.csv', header=TRUE, sep=';',dec='.', row.names=1)
ex3

## ---- eval=F-------------------------------------------------------------
#  write.sym.table(ex3, file = 'tsymtemp.csv', sep = ';',dec = '.',
#                  row.names = TRUE, col.names = TRUE)

## ------------------------------------------------------------------------
data(example3)
example3

## ------------------------------------------------------------------------
example3[2,]
example3[,3]
example3[2:3,5]
example3$F1

## ------------------------------------------------------------------------
data(ex1_db2so)
ex1_db2so

## ------------------------------------------------------------------------
result <- classic.to.sym(x = ex1_db2so, 
                         concept = c("state", "sex"),
                         variables = c(county, group, age))
result

## ------------------------------------------------------------------------
result <- classic.to.sym(x = ex1_db2so, 
                         concept = c("state", "sex"),
                         variables = c(county, group, age),
                         age_hist = sym.histogram(age, breaks = pretty(ex1_db2so$age, 5)))
result

## ------------------------------------------------------------------------
data(USCrime)
head(USCrime)

## ------------------------------------------------------------------------
result  <- classic.to.sym(x = USCrime,
                          concept = state, 
                          variables= c(NumInShelters,
                                       NumImmig,
                                       ViolentCrimesPerPop),
                          ViolentCrimesPerPop_hist = sym.histogram(ViolentCrimesPerPop,
                                                                   breaks = pretty(USCrime$iolentCrimesPerPop,5)))
result

## ------------------------------------------------------------------------
data("ex_mcfa1") 
head(ex_mcfa1)

## ------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect, 
                            variables=c(hair,
                                        eyes,
                                        region),
                            default.categorical = sym.set)
sym.table

## ------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect,
                            default.categorical = sym.set)
sym.table

## ------------------------------------------------------------------------
hani3101 <- SDS.to.RSDA(file.path = "hani3101.sds")
hani3101

## ---- eval=F-------------------------------------------------------------
#  # We can save the file in CSV to RSDA format as follows:
#  write.sym.table(hani3101,
#                  file='hani3101.csv',
#                  sep=';',
#                  dec='.',
#                  row.names=TRUE,
#                  col.names=TRUE)

## ------------------------------------------------------------------------
abalone <- SODAS.to.RSDA("abalone.xml")
abalone

## ---- eval=F-------------------------------------------------------------
#  write.sym.table(abalone,
#                  file='abalone.csv',
#                  sep=';',
#                  dec='.',
#                  row.names = TRUE,
#                  col.names = TRUE)

## ------------------------------------------------------------------------
data(example3)
mean(example3$F1)
mean(example3[,1])

## ------------------------------------------------------------------------
mean(example3$F2)
mean(example3[,2])

## ------------------------------------------------------------------------
mean(example3$F2,method = "interval")
mean(example3[,2],method = "interval")

## ------------------------------------------------------------------------
median(example3$F1)
median(example3[,1])

## ------------------------------------------------------------------------
median(example3$F2)
median(example3[,2])

## ------------------------------------------------------------------------
median(example3$F6, method = 'interval')
median(example3[,6], method = 'interval')

## ------------------------------------------------------------------------
var(example3[,1])
var(example3[,2])
var(example3$F6)
var(example3$F6, method = 'interval')
var(example3$F6, method = 'billard')
sd(example3$F1)
sd(example3$F2)
sd(example3$F6)
sd(example3$F6, method = 'interval')
sd(example3$F6, method = 'billard')

## ------------------------------------------------------------------------
cor(example3$F1, example3$F4)
cor(example3[,1], example3[,4])
cor(example3$F2, example3$F6, method = 'centers')
cor(example3$F2, example3$F6, method = 'billard')

## ------------------------------------------------------------------------
library(ggpolypath)

data(oils)
sym.radar.plot(oils[2:3,])
sym.radar.plot(oils[2:5,])

res <- interval.histogram.plot(oils[,2],
                               n.bins = 4,
                               col = c(2,3,4,5))
res

res <- interval.histogram.plot(oils[,3],
                               n.bins = 3,
                               main = "Histogram",
                               col = c(2, 3, 4))
res

## ------------------------------------------------------------------------
data("oils")
DM <- sym.dist.interval(sym.data = oils[,1:4],
                        method = "Gowda.Diday")
model <- hclust(DM)
plot(model, hang = -1)

## ------------------------------------------------------------------------
DM <- sym.dist.interval(sym.data= oils[,1:4],
                        method = "Ichino")
model <- hclust(DM)
plot(model, hang = -1)

## ------------------------------------------------------------------------
DM <- sym.dist.interval(sym.data = oils[,c(1,2,4)],
                        gamma = 0.5,
                        method = "Hausdorff",
                        normalize = FALSE,
                        SpanNormalize = TRUE,
                        euclidea = TRUE,
                        q = 2)
model <- hclust(DM)
plot(model, hang = -1)

## ------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)
res.cm <- sym.lm(formula = lpsa~., sym.data = int_prost_train, method = 'cm')
res.cm

## ------------------------------------------------------------------------
pred.cm <- sym.predict(model = res.cm, new.sym.data = int_prost_test)

## ------------------------------------------------------------------------
RMSE.L(int_prost_test$lpsa, pred.cm$Fitted)
RMSE.U(int_prost_test$lpsa, pred.cm$Fitted)
R2.L(int_prost_test$lpsa, pred.cm$Fitted)
R2.U(int_prost_test$lpsa, pred.cm$Fitted)
deter.coefficient(int_prost_test$lpsa, pred.cm$Fitted)

## ------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)

## ------------------------------------------------------------------------
res.cm.lasso <- sym.glm(sym.data = int_prost_train,
                        response = 9,
                        method = 'cm',
                        alpha = 1,
                        nfolds = 10,
                        grouped = TRUE)

## ------------------------------------------------------------------------
pred.cm.lasso <- sym.predict(res.cm.lasso,
                             response = 9,
                             int_prost_test,
                             method = 'cm')

## ------------------------------------------------------------------------
plot(res.cm.lasso)
plot(res.cm.lasso$glmnet.fit, "lambda", label=TRUE)

## ------------------------------------------------------------------------
RMSE.L(int_prost_test$lpsa,pred.cm.lasso)
RMSE.U(int_prost_test$lpsa,pred.cm.lasso) 
R2.L(int_prost_test$lpsa,pred.cm.lasso) 
R2.U(int_prost_test$lpsa,pred.cm.lasso) 
deter.coefficient(int_prost_test$lpsa, pred.cm.lasso)

## ------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)

res.cm.ridge <- sym.glm(sym.data = int_prost_train,
                        response = 9,
                        method = 'cm',
                        alpha = 0,
                        nfolds = 10,
                        grouped = TRUE)

## ------------------------------------------------------------------------
pred.cm.ridge <- sym.predict(res.cm.ridge,
                             response = 9,
                             int_prost_test,
                             method = 'cm')

## ------------------------------------------------------------------------
plot(res.cm.ridge)
plot(res.cm.ridge$glmnet.fit, "lambda", label=TRUE)
RMSE.L(int_prost_test$lpsa, pred.cm.ridge)
RMSE.U(int_prost_test$lpsa, pred.cm.ridge)
R2.L(int_prost_test$lpsa, pred.cm.ridge)
R2.U(int_prost_test$lpsa, pred.cm.ridge)
deter.coefficient(int_prost_test$lpsa, pred.cm.ridge)

## ------------------------------------------------------------------------
data("oils")
res <- sym.pca(oils,'centers')
plot(res, choix = "ind")
plot(res, choix = "var")

## ------------------------------------------------------------------------
res <- sym.pca(oils,'tops')
plot(res, choix = "ind")

## ------------------------------------------------------------------------
res <- sym.pca(oils, 'principal.curves')
plot(res, choix = "ind")

## ------------------------------------------------------------------------
res <- sym.pca(oils,'optimized.distance')
plot(res, choix = "ind")
plot(res, choix = "var")

## ------------------------------------------------------------------------
res <- sym.pca(oils,'optimized.variance')
plot(res, choix = "ind")
plot(res, choix = "var")

## ------------------------------------------------------------------------
data("ex_mcfa1") 
ex_mcfa1

## ------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect, 
                            default.categorical = sym.set)
sym.table

## ------------------------------------------------------------------------
res <- sym.mcfa(sym.table, c(2,3))
mcfa.scatterplot(res[,2], res[,3], sym.data = sym.table, pos.var = c(2,3))

## ------------------------------------------------------------------------
res <- sym.mcfa(sym.table, c(2,3,4))
mcfa.scatterplot(res[,2], res[,3], sym.data = sym.table, pos.var = c(2,3,4))


