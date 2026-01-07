# install.packages("Metrics")
library(Metrics)
library(e1071)

data <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSPCC\\after_importance.csv", encoding = "UTF-8")
str(data)
feature <- read.csv("D:\\YMY\\78\\特征组合筛选\\SFSPCC\\feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]

ad <- "D:\\YMY\\78\\特征组合筛选\\SFSPCC\\svr.rbf1\\计算结果/"

for (tt in c(1:10)){
  # 交叉验证 + 网格搜索
  # 循环：127个组合*30次循环
  b = 1
  
  aver1_tr <- matrix(0, nrow = 10, ncol = 12)   # 每一行代表一个gamma，每一列代表cost
  aver2_tr <- matrix(0, nrow = 10, ncol = 12)
  aver1_te <- matrix(0, nrow = 10, ncol = 12)
  aver2_te <- matrix(0, nrow = 10, ncol = 12)
  
  for (b in c(1:L)){
    
    # 公式：目标量~特征量
    "formula" = as.character(feature[b,1])
    formula <- as.formula(formula)
    
    name <- (feature[b,1])
    
    nn <- 1
    
    for (gamma in c(0.015625, 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8)){
      
      mm <- 1
      
      for (cost in c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)){
        
        # 定义输出矩阵
        rmse_train <- matrix(0, nrow = 1, ncol = 10)
        r2_train <- matrix(0, nrow = 1, ncol = 10)
        rmse_test <- matrix(0, nrow = 1, ncol = 10)
        r2_test <- matrix(0, nrow = 1, ncol = 10)
        
        t <- 1
        rmse_tr <- 0
        rmse_te <- 0
        r2_tr <- 0
        r2_te <- 0
        
        for (t in c(1:10)){
          
          # 训练集测试集
          set.seed(tt)
          par1 <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
          train1 <- data[par1==1,]
          test1 <- data[par1==2,]
          par
          #十折交叉验证
          par <- sample(10, nrow(train1),replace = TRUE, prob = rep(0.1,10))
          
          train <- train1[par != t,]
          test <- train1[par == t,]
          test
          #模型训练
          set.seed(tt)
          svr <- svm(formula, train, gamma = gamma, cost = cost, 
                     kernel ="radial")  # linear 或者 radial 或者 polynomial
          
          # 训练+测试---输出
          # 训练集
          ptrain <- predict(svr, train)
          rmse_train[1,t] <- rmse(train$TCF,ptrain) # 训练集 RMSE
          R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
          R2a[,1] <- ptrain
          R2a[,2] <- train$TCF
          R2a <- as.data.frame(R2a)
          names(R2a)[1] <- "ptrain"
          names(R2a)[2] <- "TCF"
          la <- lm(TCF~.,R2a)
          r2_train[1,t] <- as.numeric(summary(la)["r.squared"])
          
          # 测试集
          ptest <- predict(svr, test)
          rmse_test[1,t] <- rmse(test$TCF,ptest) # 测试集 RMSE
          R2b <- matrix(0, nrow = length(ptest), ncol = 2)
          R2b[,1] <- ptest
          R2b[,2] <- test$TCF
          R2b <- as.data.frame(R2b)
          names(R2b)[1] <- "ptest"
          names(R2b)[2] <- "TCF"
          lb <- lm(TCF~.,R2b)
          r2_test[1,t] <- as.numeric(summary(lb)["r.squared"])
          
          t = t + 1
        }
        
        rmse_tr <- mean(rmse_train[1, ])
        rmse_te <- mean(rmse_test[1, ])
        r2_tr <- mean(r2_train[1, ])
        r2_te <- mean(r2_test[1, ])
        
        aver1_tr[nn,mm] <- rmse_tr
        aver2_tr[nn,mm] <- r2_tr
        aver1_te[nn,mm] <- rmse_te
        aver2_te[nn,mm] <- r2_te
        
        mm <- mm + 1
        
      }
      
      nn <- nn + 1
      
    }
    
    write.csv(aver1_tr,paste(ad,tt,name,"rmse_train.csv"))
    write.csv(aver2_tr,paste(ad,tt,name,"r2_train.csv"))
    write.csv(aver1_te,paste(ad,tt,name,"rmse_test.csv"))
    write.csv(aver2_te,paste(ad,tt,name,"r2_test.csv"))
    
  }
}

