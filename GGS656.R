#-------------------------------------------------------------------------------
#section: packages
library(ggmap)
library(mapproj)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(ggrepel)
library(scales)
library(DT)
library(xlsx)
library(readxl)
library(sp)
library(magrittr)
library(scanstatistics)
library(rgdal)
library(rgeos)
library(maptools)
library(plyr)
library(MASS)
library(readr)
library(rpart)
library(caret)
library(lubridate)
library(randomForest)
library(dplyr)
library(class)
library(gbm)
library(randomcoloR)
library(gridExtra)
library(neuralnet)
library(MLmetrics)
library(Metrics)
library(raster)
library(viridis)
library(ggpointdensity)
library(grid)
library(e1071)
set.seed(1)


#-------------------------------------------------------------------------------
#section: prediction

#predict--random forest
#setup
dir = "C:\\Users\\Shawn\\Desktop\\GG656\\Drought.tif"

stk = stack(dir)

names(stk) <- c("Drought","NDVI","NDWI","EVI","Temperature",
                "Precipitation","Windspeed","Humidity",
                "Burn","SoilMoisture","Runoff","Evapotranspiration")


plot(stk)

df <- as.data.frame(stk,xy=TRUE)
df <- na.omit(df)
df <- df[!(df$EVI >100),]

fn <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df$EVI <- fn(df$EVI)
df$NDVI <- fn(df$NDVI)
df$NDWI <- fn(df$NDWI)
df$Temperature <- fn(df$Temperature)
df$Precipitation <- fn(df$Precipitation)
df$Windspeed <- fn(df$Windspeed)
df$Humidity <- fn(df$Humidity)
df$Burn <- fn(df$Burn)
df$SoilMoisture <- fn(df$SoilMoisture)
df$Runoff <- fn(df$Runoff)
df$Evapotranspiration <- fn(df$Evapotranspiration)


'
df$Drought <- as.numeric(df$Drought)
df$NDVI <- as.numeric(df$NDVI)
df$NDWI <- as.numeric(df$NDWI)
df$EVI <- as.numeric(df$EVI)
df$Temperature <- as.numeric(df$Temperature)
df$Precipitation <- as.numeric(df$Precipitation)
df$Windspeed <- as.numeric(df$Windspeed)
df$Humidity <- as.numeric(df$Humidity)
df$Burn <- as.numeric(df$Burn)
df$SoilMoisture <- as.numeric(df$SoilMoisture)
df$Runoff <- as.numeric(df$Runoff)
df$Evapotranspiration <- as.numeric(df$Evapotranspiration)
'
#split data 100% to train 80%, test 20%
spit = c(train = .7, test = .3)
gs = sample(cut(seq(nrow(df)), nrow(df)*cumsum(c(0,spit)),labels = names(spit)))
res = split(df, gs)
sapply(res, nrow)/nrow(df)


rf <- randomForest(Drought ~ NDVI+NDWI+EVI+Temperature+Precipitation+Windspeed+
                     Humidity+Burn+SoilMoisture+Runoff+Evapotranspiration,
                     data = res$train,ntree = 10,importance=TRUE)

#importance
varImpPlot(rf)
varImp(rf)


ref <- res$test[4:14]
pred <- predict(rf, ref)
res$test$prediction <- pred
#error measures
r2 = round(cor(res$test$Drought, pred, method = "pearson")^2,3)
RMSE <- sqrt(mean((res$test$Drought - pred)^2))
MAE <- mae(pred, res$test$Drought)

txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),
             "\n" , "MAE: ", round(MAE,3),
             "\n" , "Sample size: ", nrow(res$test))

grob <- grobTree(textGrob(txt, x=0.03,  y=0.8, hjust=0,gp=gpar(fontsize=20)))


results <- data.frame(correct = res$test$Drought, prediction=pred)
ggplot(results, aes(x=prediction, y=correct))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  annotation_custom(grob) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20)) + 
  theme(legend.title = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="red", linetype="solid", size=1.5)+
  labs(x="Predicted drought index", y="Actual drought index")+
  theme(legend.position="bottom",legend.key.width=unit(4,"cm"))


#map
df_test <- res$test[, -c(4:14)]

colnames(df_test)[colnames(df_test) == 'Drought'] <- 'Actual drought index'
colnames(df_test)[colnames(df_test) == 'prediction'] <- 'Predicted drought index'
dfr <- rasterFromXYZ(df_test,res=c(0.0416666666666667,0.0416666666666667))  
dfr <- rasterFromXYZ(df_test)  
par(mfrow=c(1,2))
plot(dfr)

#prediction---rPART
rparts <- rpart(Drought ~ NDVI+NDWI+EVI+Temperature+Precipitation+Windspeed+
                    Humidity+Burn+SoilMoisture+Runoff+Evapotranspiration,
                  data = res$train)
ref <- res$test[4:14]
pred <- predict(rparts, ref)
res$test$prediction <- pred
r2 = round(cor(res$test$Drought, pred, method = "pearson")^2,3)
RMSE <- sqrt(mean((res$test$Drought - pred)^2))
MAE <- mae(pred, res$test$Drought)

#prediction--random sample based
df<-df[sample(nrow(df), 3000), ]
spit = c(train = .7, test = .3)
gs = sample(cut(seq(nrow(df)), nrow(df)*cumsum(c(0,spit)),labels = names(spit)))
res = split(df, gs)
sapply(res, nrow)/nrow(df)

anns <- neuralnet(Drought ~ NDVI+NDWI+EVI+Temperature+Precipitation+Windspeed+
                  Humidity+Burn+SoilMoisture+Runoff+Evapotranspiration,
                data = res$train)

glms <- glm(Drought ~ NDVI+NDWI+EVI+Temperature+Precipitation+Windspeed+
              Humidity+Burn+SoilMoisture+Runoff+Evapotranspiration,
            data = res$train)

svms <- svm(Drought ~ NDVI+NDWI+EVI+Temperature+Precipitation+Windspeed+
              Humidity+Burn+SoilMoisture+Runoff+Evapotranspiration,
            data = res$train)

ref <- res$test[4:14]
pred <- predict(glms, ref)
res$test$prediction <- pred
round(cor(res$test$Drought, pred, method = "pearson")^2,3)
round(sqrt(mean((res$test$Drought - pred)^2)),3)
round(mae(pred, res$test$Drought),3)


#Q12
dir = "C:\\Users\\Shawn\\Desktop\\Q12\\Q12_Comp.tif"

stk = stack(dir)

names(stk) <- c("Drought","Precipitation","SoilMoisture","Evapotranspiration")
plot(stk)

df <- as.data.frame(stk,xy=FALSE)
df <- na.omit(df)
fn <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df$Precipitation <- fn(df$Precipitation)
df$SoilMoisture <- fn(df$SoilMoisture)
df$Evapotranspiration <- fn(df$Evapotranspiration)
df<-df[sample(nrow(df), 3000), ]
fit <- kmeans(df, 5)
