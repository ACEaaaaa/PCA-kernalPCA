FastKPCA = function (A, target_dim, sigmas){
  #V-主成分分量
  #数据集的行数核列数
  #A<-as.numeric(A)
  #A<-as.matrix(A)
#A <- x
  rows <- nrow(A)
  cols <-ncol(A)
  #去平均值处理
  meanA <- matrix(0,nrow = 1, ncol = ncol(A))
  Z <- matrix(0, rows, cols)
  for (j in 1:cols){
    Smean <- mean(A[, j]) #每列的平均值
    meanA[1,j] <- Smean
    #Svar  <- var(A[, j])  #每行的方差
    for(i in 1:rows){
      #da[i, j] <- (da[i, j]-Smean)/Svar  #标准化数据
      Z[i, j] <- A[i, j]-Smean  #标准化数据
    }
  }
  
  #aaa<<-kfun_rbf(Z[1, ],Z[2, ],sigmas)
  #Z <<- as.matrix(Z) #全局变量
  
  I <- matrix(1, rows, rows) #全1阵
  k <- matrix(0, rows, rows)
  #调用核函数
  for (i in 1:rows){
    for (j in 1:rows){
      #a<-as.matrix(Z[i,1:ncol(Z)])
      b<-as.matrix(Z[j,1:ncol(Z)])
      k[i, j] <- kfun_rbf(Z[i, ],Z[j, ],sigmas)
    } 
  }
  
  kl <- k - I%*%k*(1/rows) - k%*%I*(1/rows) + I%*%k%*%I*(1/rows^2)
  res <- eigen(kl)
  r_values <- as.matrix(res$values)
  r_vector <- as.matrix(res$vectors)
  #保留目标维度
  PCA_values <- r_values[1:target_dim]
  PCA_vector <- r_vector[,1:target_dim]
  V <- t(Z)%*%PCA_vector  #计算最大的k个特征值和特征向量
  #特征向量单位化
  for(i in 1:target_dim){
    z <- norm(as.matrix(V[ ,i]))
    V[ ,i] <- V[ ,i]/z
  }
  pcaA <- Z%*%V
  output <- list(pcaA = pcaA, V = V, mA = meanA)
  return(output)
}
#FastKPCA(x, 1, 0.2)
#kfun_rbf(x[1,],x[2,],0.2)  