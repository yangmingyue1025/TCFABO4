library(Metrics)
library(Matrix)
library(xgboost)

# 读取数据
data <- read.csv("/Users/yangmingyue/Desktop/78/模型验证/SFSPCC/xgb/data.csv", encoding = "UTF-8")
validation <- read.csv("/Users/yangmingyue/Desktop/78/模型验证/SFSPCC/xgb/validation.csv", encoding = "UTF-8")
L <- nrow(data)

# 创建输出矩阵
rmse_test <- matrix(0, nrow = 1, ncol = 1000)
r2_test <- matrix(0, nrow = 1, ncol = 1000)
pred <- matrix(NA, nrow = nrow(validation), ncol = 1000)

for (a in 1:1000) {
  set.seed(a)
  par <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
  train <- data[par == 1, ]
  test <- data[par == 2, ]
  
  print(paste("--- Iteration a =", a, "---")) # 打印当前迭代次数
  
  # --- 处理训练数据 ---
  print(paste("Original nrow(train):", nrow(train))) # 打印抽样后训练集行数
  if (nrow(train) == 0) {
    print("WARNING: train dataset is empty for this iteration!")
    # 如果训练集为空，XGBoost无法训练，后续会报错
    # 为了让循环能继续（如果只是想收集非空训练集的结果），可以跳过
    # 但按您的要求“不改变代码其他部分”，我们先让它报错，以便确认这是不是原因
  }
  
  train_matrix <- sparse.model.matrix(TCF ~ . - 1, data = train, na.action = na.pass)
  train_label <- train$TCF
  
  print(paste("nrow(train_matrix):", nrow(train_matrix)))
  print(paste("length(train_label):", length(train_label)))
  
  # 检查训练数据的维度匹配
  if (nrow(train_matrix) != length(train_label)) {
    print("ERROR: Mismatch in train_matrix rows and train_label length!")
    # 如果不匹配，下面的 dtrain 创建会失败
  }
  
  # --- 处理验证数据 ---
  print(paste("Original nrow(validation):", nrow(validation))) # 验证集行数
  validation_matrix <- sparse.model.matrix(TCF ~ . - 1, data = validation, na.action = na.pass)
  validation_label <- validation$TCF
  
  print(paste("nrow(validation_matrix):", nrow(validation_matrix)))
  print(paste("length(validation_label):", length(validation_label)))
  
  # 检查验证数据的维度匹配
  if (nrow(validation_matrix) != length(validation_label)) {
    print("ERROR: Mismatch in validation_matrix rows and validation_label length!")
    # 如果不匹配，下面的 dvalidation 创建会失败
  }
  
  # 创建 xgb.DMatrix (错误通常在这里或之前发生)
  # 只有在训练集非空时才尝试创建 dtrain 和训练
  if (nrow(train) > 0) { # XGBoost 需要至少1行训练数据
    dtrain <- xgb.DMatrix(data = train_matrix, label = train_label, missing = NA)
    dvalidation <- xgb.DMatrix(data = validation_matrix, label = validation_label, missing = NA)
    
    # 模型训练
    set.seed(a)
    xgb <- xgboost(data = dtrain, max_depth = 6, eta = 0.06, verbose = 0,
                   objective = 'reg:squarederror', nround = 300)
    
    # 预测
    ptest <- predict(xgb, dvalidation)
    rmse_test[1, a] <- rmse(validation$TCF, ptest)
    
    R2b <- data.frame(ptest = ptest, TCF = validation$TCF)
    lb <- lm(TCF ~ ptest, data = R2b)
    r2_test[1, a] <- summary(lb)$r.squared
    
    pred[, a] <- ptest
  } else {
    # 如果训练集为空，我们不能训练模型，所以将结果设为 NA
    rmse_test[1, a] <- NA
    r2_test[1, a] <- NA
    # pred[, a] 已经默认为 NA
    print(paste("Skipping training for iteration a =", a, "due to empty train set."))
  }
  
  print(paste('已完成：', a))
}

# 保存结果
ad <- "/Users/yangmingyue/Desktop/78/模型验证/SFSPCC/xgb/"
write.csv(rmse_test, paste0(ad, "rmse_test.csv"), row.names = FALSE)
write.csv(r2_test, paste0(ad, "r2_test.csv"), row.names = FALSE)
write.csv(pred, paste0(ad, "pred_test.csv"), row.names = FALSE)

print("所有计算和保存已完成！")

