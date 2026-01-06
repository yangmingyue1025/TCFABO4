pm_set <- seq(11.5, 20.5, 0.09)
cvd_set <- seq(0.34, 0.54, 0.002)
bvs_set <- seq(2.7, 5.3, 0.026)


Lp <- length(pm_set)
Lc <- length(cvd_set)
Lb <- length(bvs_set)


possible_set <- matrix(0, nrow = 1030301, ncol = 3)

i = 1
j = 1
k = 1
a = 1
b = 1
l = 1


for (i in c(1:Lp)){
  for (j in c(1:Lc)){
    for (k in c(1:Lb)){
      possible_set[l,1] <- pm_set[i]
      possible_set[l,2] <- cvd_set[j]
      possible_set[l,3] <- bvs_set[k]

          
      l = l + 1
        
    }
    k = 1
    
  }
  j = 1
  
}

write.csv(possible_set,"/Users/yangmingyue/Desktop/78/材料预测122/possible_set.csv")

