library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggdendro)
library(cluster)
library(corrplot)
library(SciViews)
library(MASS)
library(klaR)
library(ggcorrplot)
library(factoextra)

fish.data <- read.csv("C:/Users/moharahman/Downloads/Fish.csv", header=TRUE)
View(fish.data)
head(fish.data)
summary(fish.data)

fish <-filter(fish.data, ï..Species %in% c("Perch","Bream", "Roach"))
View(fish)

str(fish)
names(fish) [1] <- "Species"
fish$Species <- as.factor(fish$Species)

# 1. Means & Correlation

means <- round(colMeans(fish[2:7]), digits = 3)
means

round(colMeans(fish[fish$Species=="Bream", 2:7]), digits = 3)
round(colMeans(fish[fish$Species=="Perch", 2:7]), digits = 3)
round(colMeans(fish[fish$Species=="Roach", 2:7]), digits = 3)


correlation <- round(cor(fish[2:7]), digits = 3)
correlation

ggcorrplot(correlation, hc.order = TRUE, lab = TRUE)


# 2_A. H clustering with variables

t_fish <- t(fish[2:7])
d_fish <- dist(t_fish)
d_fish

com_hclust_fish <- hclust(d_fish, method= "complete")
com_hclust_fish
plot(com_hclust_fish)

s_hclust_fish <- hclust(d_fish, method= "single")
s_hclust_fish
plot(s_hclust_fish)

# 2_B. H clustering with observations

df <- dist(fish[2:7])
hclust_df <- hclust(df, method= "complete")
hclust_df
plot(hclust_df)

x <- cutree(hclust_df, 2)
x
plot(silhouette(x,df), col = 1:2, border=NA)

z <- cutree(hclust_df, 3)
z
plot(silhouette(z,df), col = 1:3, border=NA)

y <- cutree(hclust_df, 4)
y
plot(silhouette(y,df), col = 1:4, border=NA)


# 3. K means

scf <- scale(fish[2:7])
fviz_nbclust(scf, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


k_fish <- kmeans(fish[2:7], 2, iter.max = 30, algorithm = "MacQueen")
k_fish

k_fish$cluster
k_fish$size
k_fish$iter

kfc <- k_fish$cluster
slt <- silhouette(kfc,dist(kfc))
plot(slt, border = NA)

table(fish$Species, k_fish$cluster)

# 4. Linear regression with all variables on weight

str(fish)
attach(fish)

hist(Weight)

lin_fish <- lm(Weight~Length1+Length2+Length3+Height+Width, data = fish)
lin_fish
summary(lin_fish)

par(mfrow=c(2,2))
plot(lin_fish)

#testing outlier with the weight variables
boxplot(fish$Weight) 
fish_no_outlier <- fish$Species[!fish$Species %in% 
                           boxplot.stats(fish$Weight)$out]
fish_no_outlier
length(fish$Weight) - length(fish_no_outlier) #number of outliers
boxplot(fish_no_outlier) 


# 5. LDA

lda_fish <- lda(Species~Weight+Length3+Height+Width, data = fish)
lda_fish

class_fish <- predict(lda_fish, method="plug-in")$class
class_fish
table(Species, class_fish)

accuracy <- (35+53+14)/(35+53+3+6+14)
accuracy

error <- 1 - accuracy
error

new_fish <- data.frame(Weight= 300, Length3=28, Height=8, Width=5)
predict(lda_fish, new_fish)

f_species <- as.factor(fish$Species)
partimat(f_species~Weight+Height, method= "lda")

ggplot(fish, aes(x=Weight, y=Height, color=Species))+
  geom_point(size = 2)

ggplot(fish, aes(x=Weight, y=Height, color=Species))+
  geom_point(size = 2)+
  geom_point(aes(x=300, y=8, color="new_fish", size= 15))+
  labs(title = "Plotting New Fish Datapoint",
       caption = "Project 2: SPring 22")









