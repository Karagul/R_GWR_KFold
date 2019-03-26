# 读取
data = read.csv("F:\\毕业论文程序\\整合数据\\各地区\\日均\\总地区.csv")

# delete columns
data <- subset(data, select = -监测站)
data <- subset(data, select = -日期 )
data <- subset(data, select = -icon )
data <- subset(data, select = -precipType)
data <- subset(data, select = -summary)


# 库
library(neuralnet)

# 划分集合
set.seed(20180808)
index <-  sort(sample(nrow(data), nrow(data)*.7))
train <- data[index,]
test <-  data[-index,]

# 训练10个隐藏神经元的神经网络
net.sqrt <- neuralnet(日均PM2.5 ~ AOD值 + cloudCover + dewPoint + humidity +  pressure + precipAccumulation + precipIntensity + temperature + visibility + windSpeed,
                       train, hidden=20, threshold=0.04,act.fct="tanh", linear.output=TRUE, stepmax=1e7) #,learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic")
print(net.sqrt)

# 绘制神经网络拓扑图
#plot(net.sqrt)



# 预测
net.results <- compute(net.sqrt,test)
ls(net.results)

# 查看结果
print(net.results$net.result)

# 让结果更直观些
cleanoutput <- cbind(test$日均PM2.5, as.data.frame(net.results$net.result))
# colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output") #更换列名
print(cleanoutput)

