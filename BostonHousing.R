boston <- read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], 
       boston$LAT[boston$CHAS==1],
       col = "blue", pch=19)
points(boston$LON[boston$TRACT==3531],
       boston$LAT[boston$TRACT==3531],
       col = "red", pch = 19)
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],
       boston$LAT[boston$NOX>=0.55],
       col = "green", pch =19)
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2],
       boston$LAT[boston$MEDV>=21.2],
       col = "red", pch = 19)
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)
latlonlm <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],
       boston$LAT[boston$MEDV>=21.2],
       col = "red", pch = 19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2],
       boston$LAT[latlonlm$fitted.values>=21.2],
       col ="blue", pch = "$")
install.packages("rpart")
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON, data= boston)
prp(latlontree)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],
       boston$LAT[boston$MEDV>=21.2],
       col = "red", pch = 19)
fittedvalues <- predict(latlontree)
points(boston$LON[fittedvalues>=21.2],
       boston$LAT[fittedvalues>=21.2],
       col = "blue", pch = "$")
latlontree2 <- rpart(MEDV ~ LAT + LON, data=boston, minbucket = 50)
plot(latlontree2)
text(latlontree2)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
plot(latlontree2)
text(latlontree2)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h= 42.21)
abline(h =42.17)
points(boston$LON[boston$MEDV>=21.2],
       boston$LAT[boston$MEDV>=21.2],
       col = "red", pch = 19)
library(caTools)
set.seed(123)
split<- sample.split(boston$MEDV,
                     SplitRatio = 0.7)
train <- subset(boston, split == TRUE)
test <- subset(boston, split == FALSE)
linreg <- lm(MEDV ~ LAT + LON + CRIM +ZN +INDUS+
               CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data = train)
summary(linreg)

linreg.pred = predict(linreg, newdata = test)
linreg.sse = sum((linreg.pred-test$MEDV)^2)
linreg.sse
tree <- rpart(MEDV ~ LAT + LON + CRIM +ZN +INDUS+
               CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data = train)
prp(tree)
tree.pred <- predict(tree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse
library(caret)
library(e1071)
tr.control <- trainControl(method="cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)
tr <- train(MEDV ~ LAT + LON + CRIM +ZN +INDUS+
              CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,
            data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = test)
best.tree.sse <- sum((best.tree.pred- test$MEDV)^2)
best.tree.sse
linreg.sse
