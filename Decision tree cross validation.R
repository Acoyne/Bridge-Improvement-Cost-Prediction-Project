setwd("~/Desktop/DSO530/Project1")
load("data_model.rda")
library(tree)
set.seed(1)
train = sample(1:nrow(data), nrow(data)*(7/10))
test = (1:nrow(data))[-train]
train.data = data[train,]
test.data = data[test,"ADJ.ITEM96"]
tree.bridge = tree(ADJ.ITEM96 ~ . - FID,data = data, subset = train,mindev = 0.0001)
summary(tree.bridge)
###
#From this summary, we can tell that there is only 7 variables 
#are used in constructing the tree.
plot(tree.bridge)
?text
text(tree.bridge ,pretty =1,cex = 0.4)
set.seed(1)
cv.bridge = cv.tree(tree.bridge, K = 10)
plot(cv.bridge$size, cv.bridge$dev, type = 'b')
cv.bridge$size[which.min(cv.bridge$dev)]
summary(cv.bridge)
prune.bridge=prune.tree(tree.bridge , best=6)
summary(prune.bridge)
plot(prune.bridge)
text(prune.bridge, pretty = 0,cex = 1)

yhat=predict(tree.bridge,newdata=data[test,])
test.data=data[test,"ADJ.ITEM96"]
plot(yhat,test.data)
abline(0,1)
mean((yhat-test.data)^2)

#####KNN#####
attach(data)
library(ISLR)
library(dplyr)
library(class)
data_knn = data %>%
  select(ITEM19,ITEM27,ITEM29,ITEM32, ITEM45, ITEM46,
         ITEM48, ITEM49,ITEM50A,ITEM50B,ITEM51, ITEM76,ITEM109,
         ITEM114, SR2,ADJ.ITEM96)
standardize.X = scale(data_knn)
var(data[,1])
var(data[,2])
var(standardize.X[,1])
var(standardize.X[,2])
set.seed(1)
train.X = sample(1:nrow(standardize.X), nrow(standardize.X)*(7/10))
test.X = (1:nrow(standardize.X))[-train]
train.data = data[train.X,]
test.data = data[test.X,"ADJ.ITEM96"]
train.Y = ADJ.ITEM96[train.X]
test.Y = ADJ.ITEM96[-train.X]
set.seed(1)
knn.pred = knn(data.frame(train.X), data.frame(test.X), train.Y, k = 5)
mean(test.Y != knn.pred)
table(knn.pred,test.Y)
###KNN is useless for this situation.


library(ggplot2)
var.impor <- data.frame(Item = c("Structure Length","Length of Structure Improvement","Main Structure Type of Design","Average Daily Traffic","Number of Approach Spans","Bypass, Detour Length","Future Average Daily Traffic",
                                 "Year Built","Length of Maximum Spans","Wearing Surface","Number of Spans in Main Unit","Average Daily Truck Traffic","Bridge Roadway Width","Bridge Median","Main Structure Type of Material",
                                 "Waterway Adequacy","Traffic Safety Features","Sufficiency Rating","Historical Significance","Reconstructed","Conditional Rating of Channel","Approach Roadway Width","Underclearances","Span Structure Type of Design",
                                 "Span Structure Type of Material","Deck Geometry"),
                        Importance = c(21,14,6,5,5,5,5,4,4,3,3,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1))
ggplot(var.impor, aes(x = reorder(Item,-Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Variable Importance") +
  xlab("Variable") +
  ylab("Importance") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6)) +
  geom_text(stat = "identity",aes(label = Importance,vjust = -0.5))



library(rpart)
test.data = data[test,]
cost_test = data[-train ,"ADJ.ITEM96"]
rpart.fit = rpart(ADJ.ITEM96 ~.-FID,data = data,subset = train)
summary(rpart.fit)
pred.rpart = predict(rpart.fit,test.data)
mean((pred.rpart-cost_test)^2)

ggplot(data, aes(x = ADJ.ITEM96)) +
  geom_histogram()+
  scale_x_log10()


?element_text()
