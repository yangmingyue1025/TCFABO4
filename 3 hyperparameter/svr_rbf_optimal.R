# install.packages("Metrics")
library(Metrics)
library(e1071)
library(data.table)

data <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSPCC//after_importance.csv", encoding = "UTF-8")
str(data)
feature <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSPCC//feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"

para <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1//r2_te.csv", encoding = "UTF-8")
L <- dim(feature)[1]
str(para)

# 定义输出矩阵
rmse_train <- matrix(0, nrow = L, ncol = 1000)
r2_train <- matrix(0, nrow = L, ncol = 1000)
rmse_test <- matrix(0, nrow = L, ncol = 1000)
r2_test <- matrix(0, nrow = L, ncol = 1000)

# 循环：1000次循环
b = 1
for (b in c(1:L)){
  
  # 特征量组合
  "formula" = as.character(feature[b,1])
  formula <- as.formula(formula)
  
  # 最佳参数
  gamma = para[b,3]
  cost = para[b,4]
  
  a = 1
  for (a in c(1:1000)){
    
    #读取训练集和测试集
    set.seed(a)
    par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
    train <- data[par==1,]
    test <- data[par==2,]
    
    #支持向量机模型
    set.seed(a)
    svr <- svm(formula, train, gamma = gamma, cost = cost,
               kernel ="radial")  # linear 或者 radial 或者 polynomial
    
    # 训练+测试---输出
    # 训练集
    ptrain <- predict(svr, train)
    rmse_train[b,a] <- rmse(train$TCF,ptrain) # 训练集 RMSE
    R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
    R2a[,1] <- ptrain
    R2a[,2] <- train$TCF
    R2a <- as.data.frame(R2a)
    names(R2a)[1] <- "ptrain"
    names(R2a)[2] <- "TCF"
    la <- lm(TCF~.,R2a)
    r2_train[b,a] <- as.numeric(summary(la)["r.squared"])
    
    
    # 测试集
    ptest <- predict(svr, test)
    rmse_test[b,a] <- rmse(test$TCF,ptest) # 测试集 RMSE
    R2b <- matrix(0, nrow = length(ptest), ncol = 2)
    R2b[,1] <- ptest
    R2b[,2] <- test$TCF
    R2b <- as.data.frame(R2b)
    names(R2b)[1] <- "ptest"
    names(R2b)[2] <- "TCF"
    lb <- lm(TCF~.,R2b)
    r2_test[b,a] <- as.numeric(summary(lb)["r.squared"])
    
    
    
    ## 循环
    a = a+1
  }
  b = b+1
}

write.csv(rmse_train,"D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1\\超参数优化后计算结果//rmse_train.csv")
write.csv(r2_train,"D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1\\超参数优化后计算结果//r2_train.csv")
write.csv(rmse_test,"D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1\\超参数优化后计算结果//rmse_test.csv")
write.csv(r2_test,"D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1\\超参数优化后计算结果//r2_test.csv")

