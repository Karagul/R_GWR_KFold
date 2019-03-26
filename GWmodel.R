

library(GWmodel)
# 读取
data = read.csv("F:\\毕业论文程序\\整合数据\\各地区\\日均\\总地区toR.csv")

coordinates(data)<-~X+Y

# 找最优带宽   UNINTS: KM
BW = bw.gwr(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation + precipIntensity + pressure + temperature + visibility + windSpeed,
              data=data, approach="CV",kernel="bisquare", adaptive=FALSE, p=2, theta=0, longlat=F)
# 地理加权模型

res = gwr.predict(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation + precipIntensity + uvIndex + pressure + temperature + visibility + windSpeed,
                   data=data[1:2555,], predictdata=data[2556:3011,], bw=6333, kernel="bisquare",adaptive=TRUE, p=2,
                theta=0, longlat=F)
cha = res$SDF$prediction - data[2556:3011,]$日均PM2.5
AME = mean(abs(cha))




# 尝试十折交叉验证
library("caret")
folds<-createFolds(data,k=10) #根据training的laber-Species把数据集切分成10等份
re1<-{}
re2<-{}
for(i in 1:10){
  traindata<-data[-folds[[i]],]
  testdata<-data[folds[[i]],]
  coordinates(traindata) <-~X+Y
  coordinates(testdata) <-~X+Y
  # 找最优带宽   UNINTS: KM
  #BW = bw.gwr(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation + precipIntensity + pressure + temperature + visibility + windSpeed,
  #              data=traindata, approach="CV",kernel="bisquare", adaptive=FALSE, p=2, theta=0, longlat=F)
  BW = 6353.376
  # 拟合
  res = gwr.predict(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity + precipAccumulation + precipIntensity + pressure + temperature + visibility + windSpeed,
                      data=traindata, predictdata=testdata, bw=BW, kernel="bisquare",adaptive=TRUE, p=2,
                      theta=0, longlat=F)
  cha = res$SDF$prediction - testdata$日均PM2.5
  cha_2 = cha*cha
  AME = mean(abs(cha))
  MSE = mean(cha_2)
  re1 = c(re1,AME)
  re2 = c(re2,MSE)
}

OUTCOME = mean(re1)
outcome = mean(re2)
sdre1 = sd(re1)
sdre2 = sd(re2)

BW = 0.6353376

