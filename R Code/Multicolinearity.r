#initialize and load the libraries
library(gplots)
library(RColorBrewer)
library(pheatmap)


#reading the csv file with the data
df<- read.csv("\Path_to_File.csv" ,header=TRUE)

# remove certain unwanted columns
drop1 <- c("ID","Label","Label.2")
df = df[,!(names(df) %in% drop1)]

dff1 <- df[colMeans(is.na(df)) <= 0.1 & colMeans((df == 0), na.rm = T) <= 0.1]
dff1<-df

#replacing NA values with '0'
dff1[is.na(dff1)] <- 0

# correlation analysis
library(ggplot2)
library(reshape2)
library(corrplot)
library(Hmisc)

matriz_cor <-cor(data.matrix(dff1[,1:ncol(dff1)]))
write.csv(matriz_cor, file="Path_to_File.csv")

for (i in 1:nrow(matriz_cor)){
  correlations <-  which((abs(matriz_cor[i,]) > 0.5) & (matriz_cor[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(dff1)[i+1])
    print(correlations)
  }
}

corrplot(matriz_cor, method="circle", type = "lower", tl.cex = .8, tl.col = "black", order="hclust")


library(performance)
#reading the csv file with the data
df2<- read.csv("Path_to_File.csv" ,header=TRUE)

# remove certain unwanted columns
drop2 <- c("ID","Label")
df2 = df2[,!(names(df2) %in% drop2)]

dff2 <- df2[colMeans(is.na(df2)) <= 0.1 & colMeans((df2 == 0), na.rm = T) <= 0.1]
dff2<-df2

#replacing NA values with '0'
dff2[is.na(dff2)] <- 0

# fit model
model <- lm(Label.2 ~., data = dff2)


# now check for multicollinearity
check_collinearity(model)

x <- check_collinearity(model)
plot(x)

