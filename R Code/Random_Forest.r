###Random Forest###

library(randomForest)
library(rfUtilities)

set.seed(100)
df <- read.csv("Path_to_File.csv", header=TRUE)

df$Class <- as.factor(df$Class)

( rf.mdl <- randomForest(df[,2:58], df[,"Class"], ntree=5000, importance = TRUE) )

varImpPlot(rf.mdl, pt.cex = 4, cex = 1.5, bg = "plum2", n.var=min(5, nrow(rf.mdl$importance)))
importance(rf.mdl)

rf.imp.freq(rf.mdl, p = 0.9, plot = TRUE)

