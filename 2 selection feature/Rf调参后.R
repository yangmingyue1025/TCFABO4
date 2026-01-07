pb <- txtProgressBar(min = 0, max = 1000, style = 3)
library(randomForest)
data <- read.csv("D:\\YMY\\78\\降维前\\调参\\ABO478.csv")
L <- length(data)
# 重要性提取-矩阵，1000次试验
im1 <- matrix(0, nrow = (L-1), ncol = 1000)
im2 <- matrix(0, nrow = (L-1), ncol = 1000)
b = 1
for (b in c(1:1000)){
  a <- round(b)
  # 训练集测试集
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  set.seed(a)
  rf <- randomForest(TCF~., train, 
                     ntree = 250, maxnodes = 33, mtry = 2,
                     importance = TRUE, proximity=TRUE) # 公式需要变化
  im1[,a] <- rf$importance[,1]
  im2[,a] <- rf$importance[,2]
  Sys.sleep(0.1)
  setTxtProgressBar(pb, b)
  b = b+1
  
}



# 训练数据
p1 <- predict(rf,train)
p1
plot(train$TCF,p1,xlim = c(-80, 20), ylim = c(-80, 20)) # 需要变化
x <- seq(-80, 20,20)
lines(x,x,col = "red")
# 测试数据
p2 <- predict(rf,test)
p2
plot(test$TCF,p2,xlim = c(-80, 20), ylim = c(-80, 20)) # 需要变化
x <- seq(-80, 20,20)
lines(x,x,col = "red")


write.csv(im1, "D:\\YMY\\78\\降维前\\rf_im1.csv")
write.csv(im2, "D:\\YMY\\78\\降维前\\rf_im2.csv")

