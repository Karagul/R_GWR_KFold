# 尝试十折交叉验证
library("caret")
folds<-createFolds(data2,k=3) #根据training的laber-Species把数据集切分成10等份
re<-{}
for(i in 1:3){
  traindata<-data2[-folds[[i]],]
  testdata<-data2[folds[[i]],]
  # 此处添加方程部分
  # 此处添加检验部分
}
