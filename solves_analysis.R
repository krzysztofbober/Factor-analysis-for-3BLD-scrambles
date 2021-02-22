library(data.table)
library(moments)
solves = fread('3bld_solves.csv')
solves
solves <- solves[,c(2,5,6,7,8,9,10,11,12,13)]
solves
hist(solves$result, main="Distribution of 3BLD solves in 2021", xlab="result")
summary(solves$result)

summary(solves$algs)
table(solves$algs)

seven_algs <- solves[solves$algs==7]
eight_algs <- solves[solves$algs==8]
nine_algs <- solves[solves$algs==9]
ten_algs <- solves[solves$algs==10]
ten_algs
eleven_algs <- solves[solves$algs==11]
twelve_algs <- solves[solves$algs==12]
thirteen_algs <- solves[solves$algs==13]

library(outliers)


remove_outlier <- function(data){
  grubbs.test(data$result)
  if(grubbs.test(data$result)$p.value <= 0.05)
  {
    res <- data[order(data$result)]
    return(res[-c(2)])
  }
  else{
    return(data)
  }

    
  return(data)
}

solves_wo_outliers <- rbind(seven_algs, remove_outlier(eight_algs), 
                            remove_outlier(eleven_algs), remove_outlier(nine_algs),
                            remove_outlier(ten_algs), remove_outlier(twelve_algs), 
                            remove_outlier(thirteen_algs))


solves <- solves_wo_outliers[order(solves_wo_outliers$result)]
summary(solves$result)
plot(solves$result, solves$algs)

solves.normality <- shapiro.test(solves$result)
solves.normality

skewness(solves$result)
kurtosis(solves$result)

library(rcompanion)

transformed_solves <- apply(solves, 2, function(x) transformTukey(x, quiet = TRUE))
head(transformed_solves)

solves.PCA <- prcomp(solves, center = TRUE, scale=TRUE)
transformed_solves.PCA <- prcomp(transformed_solves, center = TRUE, scale=TRUE)

solves.PCA
summary(solves.PCA)
transformed_solves.PCA
summary(transformed_solves.PCA)


library(ggplot2)
library(factoextra)
fviz_eig(transformed_solves.PCA, main="Procent wyjasnionej zmiennosci przez poszczególne skladowe")
plot.PCA(transformed_solves.PCA)

library(devtools)
install_github("vqv/ggbiplot")


fviz_pca_ind(transformed_solves.PCA, col.ind="contrib", label="none")
fviz_pca_var(transformed_solves.PCA, col.var="contrib", repel=TRUE )

library(Rtsne)

plot(Rtsne(transformed_solves, perplexity=1, check_duplicates = FALSE)$Y, 
     main="preplexity = 1", xlab='', ylab='')
plot(Rtsne(transformed_solves, perplexity=10, check_duplicates = FALSE)$Y, 
     main="preplexity = 10", xlab='', ylab='')
plot(Rtsne(transformed_solves, perplexity=30, check_duplicates = FALSE)$Y, 
     main="preplexity = 30", xlab='', ylab='')
plot(Rtsne(transformed_solves, perplexity=50, check_duplicates = FALSE)$Y, 
     main="preplexity = 50", xlab='', ylab='')

transformed_solves.tSNE <- Rtsne(transformed_solves, perplexity=50, check_duplicates = FALSE)

plot(transformed_solves.tSNE$Y, main = 'tsne - colored by algs', pch=16, label=NULL)
text(transformed_solves.tSNE$Y, col = rainbow(solves$algs))
plot(transformed_solves.tSNE$Y, main = 'tsne - colored by result', pch=16, label=NULL)
text(transformed_solves.tSNE$Y, col = rainbow(solves$result))
plot(transformed_solves.tSNE$Y, main = 'tsne - colored by edges', pch=16, label=NULL)
text(transformed_solves.tSNE$Y, col = rainbow(solves$edges))

