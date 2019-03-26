# 库
library(lme4)
library(psych)
# 读取
data = read.csv("F:\\毕业论文程序\\整合数据\\各地区\\日均\\总地区.csv")
# 基本信息
des_data = describe(data)
# str(data)
# pairs.panels(data)

model0 <- lmer(日均PM2.5 ~ (1|AOD值),data=data)
summary(model0)






