install.packages("devtools")
install.packages("rlang")
install.packages("usethis")
install.packages("ggplot2")
install.packages("plyr")
install.packages("scales")
install.packages("grid")
install_github("vqv/ggbiplot")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggpubr")

library(devtools) 
library(usethis) 
library(ggplot2)
library(plyr)
library(scales)
library(grid)
library(stats)
library(ggpubr)
library(magrittr)
library(ggbiplot)

#Task 1
muskData = read.csv(file="c:/Users/deniz/Desktop/musk1.csv", header=TRUE, sep=",")
#####################################PCA############################################################
musk.pca <- prcomp(muskData[,c(3:168)],center = TRUE,scale. = TRUE)
 #SUMMARY OF PCA 
print("PCA is performed on the data in order to understand which of the available attribute (166) is more variant and so givesmore information about the system.")
print(summary(musk.pca))
print("Denoting PCA standardized attributes as PC; it is seen that PC1, PC2 and PC3 so first three attributes contribute the most. Moreover PC1 explains more than 1/3 of the variance followed by 13.9% and 6.7% for PC1 and PC2.")

#x11()
ggbiplot(musk.pca)

print("It can be seen that PC1 and PC2 explain a great proportion of variance ~44% the arrows parallel to PC1 axis imply that they contribute to PC1 however it is very difficult to distinguish between the different attributes.X20,X35,X110,X149, X50 seem to contribute to PC2 and X48, X28 and others contribute to PC1. In order to see which instances can be characterized as musk or not their musk class.")

#x11()
ggbiplot(musk.pca, labels=muskData[,c(1)])

print("Another representation may be the following: light blue instances are the musk ones whereas the dark blue colored the non-musk ones. Musk instances are surrounded by the non-musk for the negative region of y-axis (PC2). The two attributes have seemingly categorized those instances effectively. However this is not the case for the points on the positive side of y-axis. Here, although there is a region of purely non-musk molecules, usually musk instances surround non-musk ones which gather towards the middle of two ellipses (PC1<-5 and PC2>-5) and (PC1>0 and PC2>-5)  ")
#x11()
ggbiplot(musk.pca,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=muskData[,c(2)], groups=muskData[,c(1)])

print("Plotting the cumulative variance explained by each attribute it can be seen that in order to reach more than 80% explanation of the variance almost 25 attributes out of 166 are enough")

cumpro <- cumsum(musk.pca$sdev^2 / sum(musk.pca$sdev^2))
plot(cumpro[0:166], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")


#####################################MDS############################################################
#install.packages("tibble")
library(tibble)
mds <- muskData %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

#plot clusters using mds
clust <- kmeans(mds, 6)$cluster %>%
  as.factor()

mds <- mds %>%
  mutate(groups = clust)


ggscatter(mds, x = "Dim.1", y = "Dim.2", label = muskData[,c(1)], color = "groups", palette = "jco",size = 1,  ellipse = TRUE, ellipse.type = "convex", repel = TRUE)
print("If 6-means clustering is used for the system described by the two dimensions (same as for PCA) the groups distinguished from PCA can also be observed here.")


#Task1b: Take average of the features for musks with same id
library(dplyr)
library(data.table)

muskMeanAttributes <- matrix(0, 92, 168)#initiating mean matrix

for(i in 1:168){
res.by <- by(muskData[,c(i)], muskData[,c(2)], mean)
asFrame = as.matrix(res.by)
muskMeanAttributes[,i] = asFrame[,1]
}

#repeat PCA with mean of attributes taken
muskMean.pca <- prcomp(muskMeanAttributes[,c(3:168)],center = TRUE,scale. = TRUE)
x11()
ggbiplot(muskMean.pca,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=muskMeanAttributes[,c(2)], groups=muskMeanAttributes[,c(1)])
cumproMean <- cumsum(muskMean.pca$sdev^2 / sum(muskMean.pca$sdev^2))
plot(cumproMean[0:166], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot", xlim = c(0,102))

print("Regarding PCA it can be seen that taking the average of attributes for the instances with same instance id has slightly increased the variance explained by the two Principle components (from 44.9 to 45.8%). Its contribution is more observable when cumulative explained variance by each attribute is plotted. It can be seen that compared to the previous case for which 25 attributes where needed to explain 80% of the variance, now 13 are enough.")

#repeat MDS 

mdsMean <- muskMeanAttributes %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mdsMean) <- c("Dim.1", "Dim.2")

#plot clusters using mds
clustMean <- kmeans(mdsMean, 6)$cluster %>%
  as.factor()

mdsMean <- mdsMean %>%
  mutate(groups = clustMean)


ggscatter(mdsMean, x = "Dim.1", y = "Dim.2", label = muskMeanAttributes[,c(1)],color = "groups",palette = "jco", size = 1, ellipse = TRUE,ellipse.type = "convex", repel = TRUE)

print("Clearer plots are obtained when same number of clusters (6) are used however no clear comment on the classification power of the new set can be made due to non-robustness of the system. When three clusters are used a more robust system still with missclassifications is obtained. However, it can be seen that with less clusters still valid classification can be made when mean values of characteristics of the molecules are used. Taking the mean seems to have a greater effect on MDS than PCA (?)")

clustMean <- kmeans(mdsMean, 3)$cluster %>%
  as.factor()

mdsMean <- mdsMean %>%
  mutate(groups = clustMean)


ggscatter(mdsMean, x = "Dim.1", y = "Dim.2", label = muskMeanAttributes[,c(1)], color = "groups",  palette = "jco",  size = 1,  ellipse = TRUE,ellipse.type = "convex", repel = TRUE)

#Task2
#install.packages("jpeg")
#install.packages("imager")
library(jpeg)
library(imager)
#imageHW = readJPEG("C:/Users/deniz/Downloads/HW2IE586.jpg") #read image 
im <- load.image("C:/Users/deniz/Downloads/HW2IE586.jpg")

#test<- imageHW
#plot(1, type="n", xlim=c(100, 150), ylim=c(300, 350))
#rasterImage(test,100, 300, 150, 350)

imNoisy <- (im + .1*rnorm(prod(dim(im)))) #apply noise to image 
plot(imNoisy) #Display noisy image
xlim(0,250)
ylim(0,250)

print("The three channels of the noisy image are shown using image()")
#x11()

par(mfrow=c(1,3))
image(t(apply(imNoisy[,,1], 1, rev)), col = hcl.colors(12, "Reds 3", rev = FALSE),useRaster=TRUE, axes=FALSE)#take transpose because image() reads entries by column
title("Red Channel")
image(t(apply(imNoisy[,,2], 1, rev)), col = hcl.colors(12, "Greens 3", rev = FALSE),useRaster=TRUE, axes=FALSE)
title("Green Channel")
image(t(apply(imNoisy[,,3], 1, rev)), col = hcl.colors(12, "Blues 3", rev = FALSE),useRaster=TRUE, axes=FALSE)
title("Blue Channel")

par(mfrow=c(1,1))
imNoisy <- (im + .1*rnorm(prod(dim(im)))) #Turn noisy image gray
imGray <- grayscale(imNoisy, method = "Luma",drop = "TRUE") #when drop equal true rgb (3d depth) is reduced to 1-d depth
plot(imGray)

#imGray.pca <- prcomp(imGray, center = F)#Use pca for the image

#Let patch size to be 25
#position of center of last patch should be at 244 which is 256 - size of patch +1
k = 0 #
patchesAll = matrix(0, (NROW(imGray)-25+1)^2,25^2)#there are 25 entries of each patch whereas the number of patches equal (NROW(imGray)-25+1)^2
for(i in 1:(NROW(imGray)- 25 + 1)){
  for(j in 1:(NCOL(imGray)- 25 + 1)){
    k = k +1
patch = unlist(extract_patches(imGray,i+12,j+12,25,25)) # extract patch with 25 patch width and x, y center at i+12 and j+12 respectively
patchesAll[k,] = patch
}
}

patches.pca = prcomp(patchesAll, center = TRUE,scale. = TRUE)
print("PCA has applied to the patches (patch size = 25) of the image First principle component has variance of 16.96 which is 5.21% of the total variance. When 180 components out of 625 are used more than 50% of variance is explained.  ")

sum(patches.pca[["sdev"]][1:180])/sum(patches.pca[["sdev"]]) #cumulative deviation of 180 components

#Task2_2
patchPCA <- princomp(patchesAll, cor = TRUE, scores = TRUE)


PC1 = matrix(patchPCA$scores[,1],sqrt(NROW(patchPCA$scores[,1])),sqrt(NROW(patchPCA$scores[,1])))
PC2 =  matrix(patchPCA$scores[,2],sqrt(NROW(patchPCA$scores[,1])),sqrt(NROW(patchPCA$scores[,1])))
PC3 = matrix(patchPCA$scores[,3],sqrt(NROW(patchPCA$scores[,3])),sqrt(NROW(patchPCA$scores[,3])))
PC2cum = matrix(patchPCA$scores[,1],sqrt(NROW(patchPCA$scores[,1])),sqrt(NROW(patchPCA$scores[,1]))) + PC2 
PC3cum = matrix(patchPCA$scores[,1],sqrt(NROW(patchPCA$scores[,1])),sqrt(NROW(patchPCA$scores[,1]))) + PC2 + PC3 
#t(apply(imNoisy[,,1], 1, rev))
#x11()
par(mfrow=c(1,3))
image(t(apply(PC1,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 1")
image(t(apply(PC2,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 2")
image(t(apply(PC3,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 3")

print("When the first three components are plotted separately the figures above are obtained. Using the first component the contour and maximum dissimilarities are shown. Going to the third component variations in smaller scales (e.g. shades on the boy's shirts are seen")
print("When PC1,PC2 and PC3 are plotted cumulatively the way that the image is formed may be observed in a better way.")

#x11()
par(mfrow=c(1,3))
image(t(apply(PC1,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 1")
image(t(apply(PC2cum,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 1+2")
image(t(apply(PC3cum,2,rev)),col=grey(seq(0,1,length=256)))
title("Principle Component 1+2+3")

#Task2_3
PC1eigen = matrix(patchPCA$loadings[,1],sqrt(NROW(patchPCA$loadings[,1])),sqrt(NROW(patchPCA$loadings[,1])))
PC2eigen = matrix(patchPCA$loadings[,1],sqrt(NROW(patchPCA$loadings[,1])),sqrt(NROW(patchPCA$loadings[,1]))) + matrix(patchPCA$loadings[,2],sqrt(NROW(patchPCA$loadings[,2])),sqrt(NROW(patchPCA$loadings[,2])))
PC3eigen = matrix(patchPCA$loadings[,1],sqrt(NROW(patchPCA$loadings[,1])),sqrt(NROW(patchPCA$loadings[,1]))) + matrix(patchPCA$loadings[,2],sqrt(NROW(patchPCA$loadings[,2])),sqrt(NROW(patchPCA$loadings[,2]))) + matrix(patchPCA$loadings[,3],sqrt(NROW(patchPCA$loadings[,3])),sqrt(NROW(patchPCA$loadings[,3])))

x11()
par(mfrow=c(1,3))
image(t(apply(PC1eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors  1")
image(t(apply(PC2eigen-PC1eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors  2")
image(t(apply(PC3eigen-PC2eigen-PC1eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors  3")

par(mfrow=c(1,3))
image(t(apply(PC1eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors  1")
image(t(apply(PC2eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors 1+2")
image(t(apply(PC3eigen,2,rev)),col=grey(seq(0,1,length=256)))
title("PC_eigenvectors 1+2+3")

print("A pattern of lighter to darker colors is observed for the first component. The darker phase at the silluettes on the bottom may be observed at the second PC while on the 3rd PC the rght part gets darker. When all of the components are combined the cumulative PC plots are obtained.")
print("When eigenvectors are used for image reconstruction only very broad differentiations which help to distinguish the figures on the image are observed. Only on the cumulative plot of the third component, although the figures are still not observable some shade towards the girl figure is seen.")
