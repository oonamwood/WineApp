# Libraries
library(cluster)
devtools::install_github("bwlewis/rthreejs")
library(RCurl)
library(knitr)
library(plyr)
library(qcc)
library(threejs)
library(rgl)
library(pca3d)
library(gridExtra)
library(ggplot2)

install.packages('shinyapps')
install.packages('shiny')
install.packages('qcc')
install.packages('ggbiplot')

df = read.csv('winequality-white separated.csv', header=T)
str(df)
dataMC = apply(df, 2, function(y) y - mean(y))
head(dataMC)
pca = prcomp(dataMC, center=T, scale.=T)
str(pca)
round(pca$sdev^2, 2)
pcs = data.frame(pca$x)
str(pcs)
cov = round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
cov = data.frame(c(1:12),cov)
names(cov)[1] = 'PCs'
names(cov)[2] = 'Variance'
cov

PCA = pca$sdev^2
names(PCA) = paste0('PC', cov$PCs)
qcc::pareto.chart(PCA)

pcs$quality = df$quality
p1 = ggplot(pcs, aes(PC1, PC2, color=Label))+ geom_point() + 
  scale_colour_manual(values=c('green','red')) + theme(legend.position='none') 
p2 = ggplot(pcs, aes(PC2, PC3, color=Label))+ geom_point() +
  scale_colour_manual(values=c('green','red')) + theme(legend.position='none')
p3 = ggplot(pcs, aes(PC1, PC3, color=Label))+ geom_point() +
  scale_colour_manual(values=c('green','red')) + theme(legend.position='none')
p4 = ggplot(pcs, aes(PC1, PC4, color=Label))+geom_point() +
  scale_colour_manual(values=c('green','red')) + theme(legend.position='none')
grid.arrange(p1, p2, p3, p4, ncol=2)
