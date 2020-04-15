# XGboost - Extreamly gradient boosting algorithm <  Desicion tree
#                                                       or               <= Gradient descent
#                                                   Linear Regression



install.packages("drat", repos="https://cran.rstudio.com")

drat:::addRepo("dmlc")

install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source")


library(xgboost)
