library(MASS)
set.seed(1)
train = sample (1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston , subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston , pretty =0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type="b")

prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston , pretty =0)

yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

install.packages("randomForest")
library( randomForest)
set.seed(1)
bag.boston= randomForest( medv~.,data=Boston , subset=train,mtry=13,importance =TRUE)
bag.boston

yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
plot(yhat.bag , boston .test)
abline (0,1)
mean((yhat.bag -boston .test)^2)

