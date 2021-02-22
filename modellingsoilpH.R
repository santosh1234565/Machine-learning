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
detach("package:varImp")
df1 <- read_xlsx("pH_Feb_21.xlsx", sheet = "90cm")[4:16]

df2 <- read_xlsx("pH_Feb_21.xlsx", sheet = "150cm")[4:16]

df3 <- read_xlsx("pH_Feb_21.xlsx", sheet = "60cm")[4:17]



df1$Soil_Series <- as.factor(df1$Soil_Series)
df2$Soil_Series <- as.factor(df2$Soil_Series)
df3$Soil_Series <- as.factor(df3$Soil_Series)

#custom control parameteres
custom <- trainControl(method = "repeatedcv",
                       repeats = 10,
                       number = 8,
                       verboseIter = T)




#easy method for sample split
library(rsample)
set.seed(123)
split_data = initial_split(df1, prop = 0.75)
train_90 = training(split_data)
test_90 = testing(split_data)
set.seed(1234)
split_data1 = initial_split(df2, prop = 0.75)
train_150 = training(split_data1)
test_150 = testing(split_data1)



#random forest with caret package
#for 90cm soil depth
set.seed(123)
rmodel_90 <- train(pH ~.,
                 train_90,
                 method
                 ="rf",
                 prox = FALSE,
                 trControl = custom,
                 importance = TRUE)

rpredict_90 <- predict(rmodel_90, test_90)


rdataframe_90 <- as.data.frame(rforest1)
final <- cbind(test_90$pH, rpredict_90) #to compare side by side actual vs predicted 
f =head(final)

cor(test_90$pH, rpredict_90)#testing the correlation of predicted versus actual, r = 0.835
x =varImp(rmodel_90)
a<- plot(x)
a
vip(rmodel_90)


#for 150cm soil depth
set.seed(123)
rmodel_150 <- train(pH ~.,
                   train_150,
                   method
                   ="rf",
                   prox = FALSE,
                   trControl = custom,
                   importance = TRUE)

rpredict_150 <- predict(rmodel_150, test_150)


rdataframe_150 <- as.data.frame(rpredict_150)
final <- cbind(test_150$pH, rpredict_150) #to compare side by side actual vs predicted 
f1 =head(final)

cor(test_150$pH, rpredict_150)#r  = 0.661
varImp(rmodel_150)
b <-plot(varImp(rmodel_150))
vip(rmodel_150)
d =vip(rmodel_150)
plot(varImp(rmodel_150))



#random forest with randomForest package, needs tuning

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


set.seed(1234)
rf <- randomForest(pH ~., train_90,
                   importance = T, mtry =3)
rf1 <- predict(rf, test_90)

rf1 <- as.data.frame(rf1)
final <- cbind(test$pH, rf1)
head(final)
cor(test_90$pH, rf1)


varImpPlot(rf, type = 1) #type represents either mean decrease in 
#accuracy or mean decrease in node purity
plot(rf)
varImp(rf)
vip(rf)

# make dataframe from importance() output
feat_imp_df <- importance(rf) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

head(feat_imp_df)
# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, IncNodePurity), 
                        y = X.IncMSE)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "",
    y     = "Importance"
  )
######################################################################



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
#############################################################################

#Partial plot for 90cm depth
p1 <- partial(rmodel_90, pred.var = "AH", plot = T, rug = T)
p2<- partial(rmodel_90, pred.var = "AGSR", plot = T,rug=T)
p3 <- partial(rmodel_90, pred.var = "Sl", plot = T, rug = T)
p4<- partial(rmodel_90, pred.var = "TWI", plot = T,rug=T)
p5<- partial(rmodel_90, pred.var = "VD", plot = T,rug=T)
p6 <- partial(rmodel_90, pred.var = "PrC", plot = T, rug = T)
p7<- partial(rmodel_90, pred.var = "PlC", plot = T,rug=T)
p8 <- partial(rmodel_90, pred.var = "CI", plot = T, rug = T)
p9<- partial(rmodel_90, pred.var = "TC", plot = T,rug=T)
p10 <- partial(rmodel_90, pred.var = "El", plot = T, rug = T)
p11<- partial(rmodel_90, pred.var = "As", plot = T,rug=T)
p12<- partial(rmodel_90, pred.var = "Soil_Series", plot = T,rug=T)


#pp for 150 cm depth
p1 <- partial(rmodel_150, pred.var = "AH", plot = T, rug = T)
p2<- partial(rmodel_150, pred.var = "AGSR", plot = T,rug=T)
p3 <- partial(rmodel_150, pred.var = "Sl", plot = T, rug = T)
p4<- partial(rmodel_150, pred.var = "TWI", plot = T,rug=T)
p5<- partial(rmodel_150, pred.var = "VD", plot = T,rug=T)
p6 <- partial(rmodel_150, pred.var = "PrC", plot = T, rug = T)
p7<- partial(rmodel_150, pred.var = "PlC", plot = T,rug=T)
p8 <- partial(rmodel_150, pred.var = "CI", plot = T, rug = T)
p9<- partial(rmodel_150, pred.var = "TC", plot = T,rug=T)
p10 <- partial(rmodel_150, pred.var = "El", plot = T, rug = T)
p11<- partial(rmodel_150, pred.var = "As", plot = T,rug=T)
p12<- partial(rmodel_150, pred.var = "Soil_Series", plot = T,rug=T)


g1 <-grid.arrange(p1, p2,p3, p4,p5, p6, nrow =3, ncol=2)
g2<- grid.arrange(p7, p8,p9,p10,p11,p12, nrow = 3, ncol =2)

ggsave("partialplot_90cm1.tiff",g1, units="in", width=6, height=7, dpi = 800)
ggsave("partialplot_90cm2.tiff",g2, units="in", width=6.5, height=7, dpi = 800)

ggsave("partialplot_150cm1.tiff",g1, units="in", width=6, height=7, dpi = 800)
ggsave("partialplot_150cm2.tiff",g2, units="in", width=6.5, height=7, dpi = 800)


#partialplot ggplot
p1 <- partial(rforest, pred.var = "TWI", plot = T, rug = T, plot.engine = "ggplot2")
p2<- partial(rf, pred.var = "CI", plot = T,rug=T, plot.engine = "ggplot2")
p3 <- partial(rf, pred.var = "TC", plot = T, rug = T, plot.engine = "ggplot2")
p4<- partial(rf, pred.var = "Sl", plot = T,rug=T, plot.engine = "ggplot2")
grid.arrange(p1, p2,p3, p4, nrow =2, ncol=2)


# graphs with labels 

library(gridExtra)
require(cowplot)

impo<-plot_grid(a,b,labels = c("A.", "B." ),
                ncol = 2, nrow = 1)
impo

ggsave("VarImportance.tiff",impo, units="in", width=6.2, height=4, dpi = 800)

#decision tree
tree <- rpart( pH ~., data = df1, control = rpart.control(minsplit = 6))
printcp(tree)
plotcp(tree)
summary(tree)
rsq.rpart(tree)
library(rpart.plot)
prp(tree)



#compare models
model_list <- list(forest = rforest1, boost = boosting)
res <- resamples(model_list)
summary(res)

model_list <- list(forest90 = rmodel_90, forest150 = rmodel_150)
res<- resamples(model_list)
summary(res)






