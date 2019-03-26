# 3.13
# 工作环境
setwd("C:\\Users\\Administrator.SC-201902221855\\Desktop")
# 相关库
library(spgwr)
# 读取数据
data1 = read.csv("F:\\毕业论文程序\\整合数据\\各地区\\日均\\总地区.csv")



# 地理加权回归模型
#res = gwr(PM2.5浓度 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation +  pressure + temperature + visibility + windSpeed, 
#          data = data1, bandwidth=BW, hatmatrix = TRUE)

# 尝试十折交叉验证
library("caret")
folds<-createFolds(data1,k=10) #根据training的laber-Species把数据集切分成10等份
re<-{}
for(i in 1:10){
  traindata<-data1[-folds[[i]],]
  testdata<-data1[folds[[i]],]
  coordinates(traindata) <-~X+Y
  coordinates(testdata) <-~X+Y
  # 找最优带宽   UNINTS: KM
  BW = gwr.sel(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation + precipIntensity + pressure + temperatureMax + visibility + windSpeed,
                 , data=traindata, adapt=FALSE, method = "cv", verbose = TRUE, longlat=NULL, RMSE=TRUE)
  # 拟合
  res = gwr(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity +  pressure + precipAccumulation + precipIntensity  + temperature + visibility + windSpeed, 
            data = traindata, bandwidth=BW, hatmatrix = TRUE)
  re = c(re,res$results$rss/res$gTSS)
  print(re)
}

mean(re)


# 积雪降雨均0
res = gwr(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity +  pressure + precipAccumulation + precipIntensity + temperature + visibility + windSpeed, 
            data = data1, bandwidth=BW, hatmatrix = TRUE)
# summary(res$results)
sdf = res$SDF
localR2 = sdf$localR2
r2 = 1 - (res$results$rss/res$gTSS)
r2 = mean(r2)
MAE=mean(abs(res$lm$residuals))
mse = res$results$rss/res$results$n

# t 检验
t = sdf$humidity/sdf$humidity_se
t = sdf$AOD值/sdf$AOD值_se
t = sdf$cloudCover/sdf$cloudCover_se_EDF
t = sdf$dewPoint/sdf$dewPoint_se
t = sdf$precipAccumulation/sdf$precipAccumulation_se # 0.383
t = sdf$pressure/sdf$pressure_se
t = sdf$temperature/sdf$temperature_se
t = sdf$visibility/sdf$visibility_se
t = sdf$windSpeed/sdf$windSpeed_se
