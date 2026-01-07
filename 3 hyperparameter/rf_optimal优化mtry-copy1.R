library(Metrics)
library(randomForest)
data <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSSCC\\after_importance.csv", encoding = "UTF-8")

feature <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSSCC\\feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"

para <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSSCC\\rf\\r2_te.csv", encoding = "UTF-8")
L <- dim(feature)[1]

# Defined output matrix
rmse_train <- matrix(0, nrow = L, ncol = 1000)
r2_train <- matrix(0, nrow = L, ncol = 1000)
rmse_test <- matrix(0, nrow = L, ncol = 1000)
r2_test <- matrix(0, nrow = L, ncol = 1000)

b = 1
for (b in c(1:L)){
  
  # Qf~features
  "formula" = as.character(feature[b,1])
  formula <- as.formula(formula)
  # Optimum hyperparameters
  mtry = para[b, 2]
  maxnodes = para[b, 4]
  ntree = para[b, 5]
  
  a = 1
  for (a in c(1:1000)){
    
    #split training set and testing set
    set.seed(a)
    par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
    train <- data[par==1,]
    test <- data[par==2,]
    
    #rf
    set.seed(a)
    rf <- randomForest(formula, train, mtry = mtry, 
                       ntree = ntree, maxnodes = maxnodes)
    
    # result of training
    ptrain <- predict(rf, train)
    rmse_train[b,a] <- rmse(train$TCF,ptrain) 
    R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
    R2a[,1] <- ptrain
    R2a[,2] <- train$TCF
    R2a <- as.data.frame(R2a)
    names(R2a)[1] <- "ptrain"
    names(R2a)[2] <- "TCF"
    la <- lm(TCF~.,R2a)
    r2_train[b,a] <- as.numeric(summary(la)["r.squared"])
    
    
    # result of testing
    ptest <- predict(rf, test)
    rmse_test[b,a] <- rmse(test$TCF,ptest)
    R2b <- matrix(0, nrow = length(ptest), ncol = 2)
    R2b[,1] <- ptest
    R2b[,2] <- test$TCF
    R2b <- as.data.frame(R2b)
    names(R2b)[1] <- "ptest"
    names(R2b)[2] <- "TCF"
    lb <- lm(TCF~.,R2b)
    r2_test[b,a] <- as.numeric(summary(lb)["r.squared"])
    
    a = a+1
  }
  b = b+1
}

write.csv(rmse_train,"D:\\YMY\\78\\特征组合筛选\\SFSSCC\\rf\\超参数优化后计算结果\\rmse_train.csv")
write.csv(r2_train,"D:\\YMY\\78\\特征组合筛选\\SFSSCC\\rf\\超参数优化后计算结果\\r2_train.csv")
write.csv(rmse_test,"D:\\YMY\\78\\特征组合筛选\\SFSSCC\\rf\\超参数优化后计算结果\\rmse_test.csv")
write.csv(r2_test,"D:\\YMY\\78\\特征组合筛选\\SFSSCC\\rf\\超参数优化后计算结果\\r2_test.csv")

