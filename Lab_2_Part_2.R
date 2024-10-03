#Naive-Bayes Theorem Example (Iris)

iris_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Lab 2 Part 2/iris.csv")

View(iris_data)

attach(iris_data)

## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ## 
install.packages("e1071")

library("e1071")

classifier<-naiveBayes(iris[,1:4], iris[,5]) 
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual')) 
classifier$apriori
classifier$tables$Petal.Length

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue") 
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

#Exercise 1 Naive Bayes Theorem - Abalone
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), 
                    header = FALSE, sep = ",")

View(abalone)

colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 
                       'viscera_weight', 'shell_weight', 'rings' ) 

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

new_abalone <- subset(abalone, select = -c(sex, rings))

View(new_abalone)

abalone_classifier<-naiveBayes(new_abalone[,1:7], new_abalone[,8]) 

table(predict(abalone_classifier, new_abalone[,-8]), new_abalone[,8], dnn=list('predicted','actual')) 

abalone_classifier$apriori

#shell weight classifier
abalone_classifier$tables$shell_weight

plot(function(x) dnorm(x, 0.1213945, 0.08096481), col="red", main="Shell Weight Distribution for the Three Age Groups") 
curve(dnorm(x, 0.2752133, 0.10944254), add=TRUE, col="blue") 
curve(dnorm(x, 0.3423526, 0.13680125), add=TRUE, col = "green")

#Length

abalone_classifier1<-naiveBayes(new_abalone[,1:7], new_abalone[,8]) 

table(predict(abalone_classifier1, new_abalone[,-8]), new_abalone[,8], dnn=list('predicted','actual')) 

abalone_classifier1$apriori

#length classifier
abalone_classifier1$tables$length

plot(function(x) dnorm(x, 0.4209915, 0.11137474), col="red", main="Length Distribution for the Three Age Groups", ylim = c(0.0, 5.0)) 
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="blue") 
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "green")


#Diameter

abalone_classifier2<-naiveBayes(new_abalone[,1:7], new_abalone[,8]) 

table(predict(abalone_classifier2, new_abalone[,-8]), new_abalone[,8], dnn=list('predicted','actual')) 

abalone_classifier2$apriori

#diameter classifier
abalone_classifier2$tables$diameter

plot(function(x) dnorm(x, 0.3212758, 0.09029187), col="red", main="Length Distribution for the Three Age Groups", ylim = c(0.0, 7.0)) 
curve(dnorm(x, 0.4458591, 0.07153798), add=TRUE, col="blue") 
curve(dnorm(x, 0.4632083, 0.06699741), add=TRUE, col = "green")


#KNN Model for Abalone (Example)

colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 
                       'viscera_weight', 'shell_weight', 'rings' ) 

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 
abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_weight)

#split the data into sample and population data "2924" is 70% of the data set for training set
s_abalone <- sample(4177,2924)

#split into train/test set
abalone.train <-abalone.norm[s_abalone,]

#the '-' before s_abalone is the notation for taking all other values in the data set not captured in the line above
abalone.test <-abalone.norm[-s_abalone,]

abalone.train <-abalone[s_abalone,-1]
abalone.test <-abalone[-s_abalone,-1]

sqrt(2924)
k = 55

install.packages("class")
library(class)

KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)

contingency.table <- table(KNNpred,abalone.test$age.group)
contingency.table

contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone.test$age.group)

accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)

for (k in ks) {
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group)) 
}

plot(ks,accuracy,type = "b", ylim = c(0.67,0.69))

#Exercise 2 KNN Model (Iris)

View(iris_data)

colnames(iris_data) <- c("X", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" ) 

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

iris_data.norm <- iris_data

iris_data.norm[2:5] <- as.data.frame(lapply(iris_data.norm[2:5], normalize))

View(iris_data.norm)

summary(iris_data.norm$Sepal.Length)

n <- nrow(iris_data.norm) 

s_iris <- sample(150, 105)

#split into train/test set
iris.train <-iris_data.norm[s_iris,-1]
iris.test <-iris_data.norm[-s_iris,-1]


iris.train <-iris_data.norm[s_iris, ]
iris.test <-iris_data.norm[-s_iris, ]

sqrt(105)
k = 10

install.packages("class")
library(class)

str(iris.train)
str(iris.test)


KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Species, k = k)

contingency.table <- table(KNNpred,iris.test$Species)
contingency.table

contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(iris.test$Species)

accuracy <- c()

ks <- c(1, 3, 5, 7, 10, 12, 15, 20)

for (k in ks) {
  KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Species, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species)) 
}

plot(ks,accuracy,type = "b", ylim = c())

#Section 3 K-Means - Iris

# Plot iris petal length vs. petal width, color by species
install.packages("ggplot2")
library(ggplot2)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

# set seed for random number generator
set.seed(123)

# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])

#K-Means Abalone


ggplot(new_abalone, aes(x = length, y = diameter, colour = age.group)) +
  geom_point()

# set seed for random number generator
set.seed(123)


str(new_abalone)

normalize_abalone <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

abalone_data.norm <- new_abalone

abalone_data.norm[1:7] <- as.data.frame(lapply(abalone_data.norm[1:7], normalize_abalone))

View(abalone_data.norm)


# run k-means
abalone.km <- kmeans(abalone_data.norm[,-8], centers = 3)

assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone_data.norm, aes(x = length, y = diameter, colour = assigned.clusters)) +
  geom_point()

wsx <- c()
ks <- c(1,3,5,7)

for (k in ks) {
  abalone.km <- kmeans(abalone_data.norm[,-8], centers = k)
  wsx <- c(wsx,abalone.km$tot.withinss)
}
length(ks)
length(wss)

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "young"
labeled.clusters[labeled.clusters==2] <- "adult"
labeled.clusters[labeled.clusters==3] <- "old"
table(labeled.clusters, abalone_data.norm[,8])


  