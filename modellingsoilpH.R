setwd("~\\documents\\BS paper new")
library(caret)
library(glmnet)
library(mlbench)
library(readxl)
library(vip)
library(randomForest)
library(earth)
library(gbm)
library(Metrics)
library(pdp)
library(mgcv)
library(gam)
library(rpart)
#detach the loaded package
detach("package:dplyr")

df1 <- read_xlsx("pH_Feb_21.xlsx", sheet = "90cm")[4:17]

df2 <- read_xlsx("pH_Feb_21.xlsx", sheet = "90cm")[4:19]

df3 <- df2[-2]


df1$Soil_Series <- as.factor(df1$Soil_Series)
df2$Soil_Series <- as.factor(df2$Soil_Series)
df2$Crop_Rotation <- as.factor(df2$Crop_Rotation)

#custom control parameteres
custom <- trainControl(method = "repeatedcv",
                       repeats = 10,
                       number = 8,
                       verboseIter = T)



#train test data
set.seed(1234)
ind <- sample(2, nrow(df2), replace = T, prob = c(0.7, 0.3))

train<- df2[ind==1,]
test <- df2[ind == 2,]

#fine tuning random forest
features <- setdiff(names(train), "pH")
m2 <- tuneRF(
  x          = train[features],
  y          = train$pH,
  ntreeTry   = 500,
  mtryStart  = 3,
  stepFactor = 0.5,
  improve    = 0.01,
  trace      = FALSE)

#random forest

set.seed(1234)
rforest <- train(pH ~.,
                 train,
                 method
                 ="rf",
                 prox = FALSE,
                 trControl = custom,
                 importance = TRUE)

rforest1 <- predict(rforest, test)

rforest1 <- as.data.frame(rforest1)
final <- cbind(test$pH, rforest1)
head(final)
cor(test$pH, rforest1)
plot(varImp(rforest))
vip(rforest)



set.seed(1234)
rf <- randomForest(pH ~., train,
                   importance = T, mtry =3)
rf1 <- predict(rf, test)

rf1 <- as.data.frame(rf1)
final <- cbind(test$pH, rf1)
head(final)
cor(test$pH, rf1)

print(rf)
print(rf1)
varImpPlot(rf)
plot(rf)





#gradient boosting
set.seed(1234)
boosting <- train(pH ~.,
                 train,
                 method
                 ="gbm",
                 trControl = custom)
boosting1 <- predict(boosting, test)

boosting1 <- as.data.frame(boosting1)
final <- cbind(test$pH, boosting1)
head(final)
cor(test$pH, boosting1)
summary(boosting)
plot(varImp(boosting))

#Partial plot
p1 <- partial(rforest, pred.var = "AH", plot = T, rug = T)
p2<- partial(rforest, pred.var = "Sl", plot = T,rug=T)
p3 <- partial(rforest, pred.var = "Soil_Series", plot = T, rug = T)
p4<- partial(rforest, pred.var = "Nrate", plot = T,rug=T)
grid.arrange(p1, p2,p3, p4, nrow =2, ncol=2)

#partialplot ggplot
p1 <- partial(rforest, pred.var = "TWI", plot = T, rug = T, plot.engine = "ggplot2")
p2<- partial(rf, pred.var = "CI", plot = T,rug=T, plot.engine = "ggplot2")
p3 <- partial(rf, pred.var = "TC", plot = T, rug = T, plot.engine = "ggplot2")
p4<- partial(rf, pred.var = "Sl", plot = T,rug=T, plot.engine = "ggplot2")
grid.arrange(p1, p2,p3, p4, nrow =2, ncol=2)

#compare models
model_list <- list(forest = rforest1, boost = boosting)
res <- resamples(model_list)
summary(res)

#decision tree
tree <- rpart( pH ~., data = df1, control = rpart.control(minsplit = 6))
printcp(tree)
plotcp(tree)
summary(tree)
rsq.rpart(tree)
library(rpart.plot)
prp(tree)







# graphs

library(gridExtra)
require(cowplot)

impo<-plot_grid(eng,lassog, ridgeg,labels = c("A.", "B.", "C."),
                ncol = 2, nrow = 2)
impo

ggsave("Importance_new.tiff",impo, units="in", width=6.5, height=6.5, dpi = 1000)

#prediction
pr <- predict(en,data1)
head(pr)
df <- as.data.frame(pr)
df1 <- round(df, digits = 0)
write_tsv(df1, "predicted_30withbd.txt")
 
 
data3 <- read_excel("10cm.xlsx", sheet = "Sheet4")

d.filter <- filter(data3, BS < 90 & BS > 50)
d.filter1 <- filter(data2, BS < 90)

df5 <- slice(data3, 1:63)
tail(df5)
df3 <- aov(BS ~as.factor(Year), data = d.filter1)
summary(df3)


TU<-TukeyHSD(df3)
TU
p <- ggplot(d.filter1, aes(x = as.factor(Year), y = BS))
p + geom_bar (stat = "identity", aes(fill= as.factor(Year))) +facet_grid(~ as.factor(depth))

p4 <- ggplot(d.filter1, aes(x = as.factor(Depth), y = BS, fill = as.factor(Year))) +
        geom_col(position = "dodge")




