# ----------------------------
# 初始化环境
# ----------------------------
# 安装必要包（如果未安装）
if (!require("corrplot")) install.packages("corrplot")
if (!require("GGally")) install.packages("GGally")
if (!require("dplyr")) install.packages("dplyr")

# 加载包
library(corrplot)
library(GGally)
library(dplyr)

# ----------------------------
# 数据读取与预处理
# ----------------------------
# 读取数据（注意检查文件路径是否正确）
data <- read.csv("D:\\YMY\\78\\皮尔森\\ABO478.csv", header = TRUE, stringsAsFactors = FALSE)

# 强制将第13列转换为数值型（修复可能的格式问题）
data[,13] <- as.numeric(data[,13]) %>% replace_na(0)  # 处理NA值

# 检查所有列是否为数值型
if (!all(sapply(data, is.numeric))) {
  stop("存在非数值列，请检查数据预处理步骤！")
}

# 重命名列名（确保与数据实际列数匹配）
names(data) <- c("vm","sp","d", "pm", "ppv", "vc","vi", "vcar", "pd",  "Xz","iec",  "enP", "mi","blm", 
                 "blsd", "irc", "ipc", "cv", "cvsd", "cvd", "bvs", "S", "Sm","Ssd")

# ----------------------------
# 计算相关系数矩阵（完整计算）
# ----------------------------
# 皮尔森相关系数矩阵
pcc_matrix <- cor(data, method = "pearson", use = "complete.obs")

# 斯皮尔曼相关系数矩阵
spearman_matrix <- cor(data, method = "spearman", use = "complete.obs")

# ----------------------------
# 结果输出
# ----------------------------
# 保存矩阵到CSV
write.csv(pcc_matrix, "D:\\YMY\\78\\皮尔森\\pcc_matrix.csv", row.names = TRUE)
write.csv(spearman_matrix, "D:\\YMY\\78\\皮尔森\\spearman_matrix.csv", row.names = TRUE)


# ----------------------------
# 可视化2：并排对比热力图
# ----------------------------
corrplot(pcc_matrix, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")
#调整字体大小
cex.before <- par("cex") 
par(cex = 0.6) 
p <- corrplot(corr=pcc_matrix, type = "full", method = "color")

corrplot(pcc_matrix, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")

ls()
dsamp <- diamonds[sample(nrow(diamonds), 2000), ]
qplot(carat, price, data=dsamp, colour=clarity)
savePlot(filename = "Rplot",
         type ="wmf",
         restoreConsole = TRUE)



# help(corrplot)


corrplot.mixed(pcc_matrix)  
corrplot.mixed(pcc_matrix, lower = "color", upper = "ellipse",outline = TRUE, mar = c(1, 1, 1, 1),
               tl.cex = 0.95, number.font = 1,
               cl.cex = 1.5,cl.align.text = "l")


#spearman画图
corrplot(spearman_matrix, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")
#调整字体大小
cex.before <- par("cex") 
par(cex = 0.6) 
p <- corrplot(corr=spearman_matrix, type = "full", method = "color")

corrplot(spearman_matrix, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")

ls()
dsamp <- diamonds[sample(nrow(diamonds), 2000), ]
qplot(carat, price, data=dsamp, colour=clarity)
savePlot(filename = "Rplot",
         type ="wmf",
         restoreConsole = TRUE)



# help(corrplot)


corrplot.mixed(spearman_matrix)  
corrplot.mixed(spearman_matrix, lower = "color", upper = "ellipse",outline = TRUE, mar = c(1, 1, 1, 1),
               tl.cex = 0.95, number.font = 1,
               cl.cex = 1.5,cl.align.text = "l")





# ----------------------------
# 高相关性对筛选与输出
# ----------------------------
threshold <- 0.85  # 可调整阈值

# 筛选皮尔森高相关对
high_pcc <- which(abs(pcc_matrix) > threshold & lower.tri(pcc_matrix), arr.ind = TRUE)
high_pcc_df <- data.frame(
  Var1 = rownames(pcc_matrix)[high_pcc[,1]],
  Var2 = colnames(pcc_matrix)[high_pcc[,2]],
  PCC_Value = pcc_matrix[high_pcc]
) %>% arrange(desc(abs(PCC_Value)))

# 筛选斯皮尔曼高相关对
high_spearman <- which(abs(spearman_matrix) > threshold & lower.tri(spearman_matrix), arr.ind = TRUE)
high_spearman_df <- data.frame(
  Var1 = rownames(spearman_matrix)[high_spearman[,1]],
  Var2 = colnames(spearman_matrix)[high_spearman[,2]],
  Spearman_Value = spearman_matrix[high_spearman]
) %>% arrange(desc(abs(Spearman_Value)))

# 保存结果
write.csv(high_pcc_df, "D:\\YMY\\78\\皮尔森\\high_pcc_pairs.csv", row.names = FALSE)
write.csv(high_spearman_df, "D:\\YMY\\78\\皮尔森\\high_spearman_pairs.csv", row.names = FALSE)



