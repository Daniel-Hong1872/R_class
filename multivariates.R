library(vegan)
library(qgraph)
library(ade4)
library(mvabund)
library (pvclust)
library(factoextra)
library(ecodist)
library(tree)
library(rpart)
library(ggplot2)
library(randomForest)
library(caret)
library(rattle)
library (tidyr)

# to be careful
library(mvpart) # install_github("cran/mvpart", force = T) # after devtools
library(MVPARTwrap) # install_github("cran/MVPARTwrap", force = T) # after devtools

#  functions from Borcard et al. 2011
source('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/coldiss.r') 

t1<-tikus$abund
t2<-tikus$x
tikus.sel <- t1[t2$time %in% c(81, 83, 85)]
tikus.sel2 <- as.data.frame(t(tikus.sel))

tikus.hell <- decostand(tikus.sel, 'hellinger')
tikus.bc <- vegdist(tikus.sel2)
tikus.bc
coldiss(tikus.bc,byrank=F,diag=T)
qgraph(1-tikus.bc, layout='spring', vsize=4)
