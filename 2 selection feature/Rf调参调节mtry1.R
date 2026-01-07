# install.packages("Metrics")
library(Metrics)
library(randomForest)

data <- read.csv("D:\\YMY\\78\\降维前\\调参\\ABO478.csv", encoding = "UTF-8")
str(data)

ad <- "D:\\YMY\\78\\降维前\\调参\\rf1/"

tt <- 1

for (tt in c(1:30)){  #原来是1到30，分两次跑的
  aver1_tr <- matrix(0, nrow = 10, ncol = 20)   # 每一行代表一个maxnodes，每一列代表一个ntree
  aver2_tr <- matrix(0, nrow = 10, ncol = 20)
  aver1_te <- matrix(0, nrow = 10, ncol = 20)
  aver2_te <- matrix(0, nrow = 10, ncol = 20)
  
  mm <- 1 # 用于记录maxnodes的数量
  
  for (maxnodes in seq(from = 21, to = 48, by = 3)){
    
    nn <- 1 #用于记录ntree数量
    
    for (ntree in seq(from = 50, to = 1000, by = 50)){
      
      # 交叉验证 + 网格搜索
      rmse_train <- matrix(0, nrow = 1, ncol = 10)
      r2_train <- matrix(0, nrow = 1, ncol = 10)
      rmse_test <- matrix(0, nrow = 1, ncol = 10)
      r2_test <- matrix(0, nrow = 1, ncol = 10)
      
      t <- 1
      # 十折交叉验证
      for (t in c(1:10)){
        set.seed(tt)
        par1 <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
        train1 <- data[par1==1,]
        test1 <- data[par1==2,]
        
        #十折交叉验证
        par <- sample(10, nrow(train1),replace = TRUE, prob = rep(0.1,10))
        
        train <- train1[par != t,]
        test <- train1[par == t,]
        
        set.seed(tt)
        rf <- randomForest(TCF~., train, ntree = ntree, maxnodes = maxnodes, mtry = 1)
        
        # 训练+测试---输出
        # 预测训练集
        ptrain <- predict(rf, train)
        rmse(train$TCF,ptrain)
        # 计算训练集的RMSE
        rmse_train[1, t] <- rmse(train$TCF,ptrain)
        # 计算训练集的R2
        R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
        R2a[,1] <- ptrain
        R2a[,2] <- train$TCF
        R2a <- as.data.frame(R2a)
        names(R2a)[1] <- "ptrain"
        names(R2a)[2] <- "TCF"
        la <- lm(TCF~.,R2a)
        r2_train[1, t] <- as.numeric(summary(la)["r.squared"])
        # 测试集
        ptest <- predict(rf, test)
        # 计算测试集的RMSE
        rmse_test[1, t] <- rmse(test$TCF,ptest)
        # 计算测试集的R2
        R2b <- matrix(0, nrow = length(ptest), ncol = 2)
        R2b[,1] <- ptest
        R2b[,2] <- test$TCF
        R2b <- as.data.frame(R2b)
        names(R2b)[1] <- "ptest"
        names(R2b)[2] <- "TCF"
        lb <- lm(TCF~.,R2b)
        r2_test[1, t] <- as.numeric(summary(lb)["r.squared"])
        
        t <- t + 1
        
      }
      
      aver1_tr[mm, nn] <- mean(rmse_train[1, ])
      aver2_tr[mm, nn] <- mean(r2_train[1, ])
      aver1_te[mm, nn] <- mean(rmse_test[1, ])
      aver2_te[mm, nn] <- mean(r2_test[1, ])
      
      nn <- nn + 1
      
    }
    
    mm <- mm + 1
    
  }
  
  write.csv(aver1_tr,paste(ad,tt,"rmse_train.csv"))
  write.csv(aver2_tr,paste(ad,tt,"r2_train.csv"))
  write.csv(aver1_te,paste(ad,tt,"rmse_test.csv"))
  write.csv(aver2_te,paste(ad,tt,"r2_test.csv"))
  
  tt <- tt + 1
}




