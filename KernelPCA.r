# 3 groups of sample, each size is 20
N_in_group = 40
N_group = 3
N_total = N_in_group * N_group
x=matrix(0, ncol = 2, nrow = N_total)
label = rep(c(1,2,3), rep(N_in_group,N_group))
# 生成四个图像
layout(matrix(seq(4), 2,2))

# use polar coordinate to generate sample
# theta ~ UNIF(0, 2pi)，theta分布在(0~2pi)范围内
r = rep(c(1, 3, 6), rep(N_in_group,N_group))
theta = runif(N_total)*2*pi   # runif生成随机数，默认(0, 1)
#对样本进行赋值，
x[,1] = (r  ) * cos(theta) + rnorm(N_total, sd = .2)
x[,2] = (r  ) * sin(theta) + rnorm(N_total, sd = .2)

plot(x[,1], x[,2], col = rainbow(3)[label], main = "Origin"
, xlab="First dimension", ylab="Second dimension")

X = x
XtX = t(X) %*% X
res = eigen(XtX) #计算特征值和特征向量

V = res$vectors       #V是特征向量
D = diag(res$values)  #D是对角线

# verify eigen decop
# sum(abs(XtX %*% V - V %*% (D)))
Y = X%*% V
plot(Y[,1], Y[,2], col = rainbow(3)[label], main = "Traditional PCA" 
, xlab="First component", ylab="Second component")


# Kernel PCA
# Polynomial Kernel
# k(x,y) = t(x) %*% y + 1
k1 = function (x,y) { (x[1] * y[1] + x[2] * y[2] + 1)^2 }
K = matrix(0, ncol = N_total, nrow = N_total)
for (i in 1:N_total) {
  for (j in 1:N_total) { 
    K[i,j] = k1(X[i,], X[j,])
}}
ones = 1/N_total* matrix(1, N_total, N_total)
K_norm = K - ones %*% K - K %*% ones + ones %*% K %*% ones
res = eigen(K_norm)

V = res$vectors
D = diag(res$values)

Y = K %*% V
plot(Y[,1], Y[,2], col = rainbow(3)[label], main = "Kernel PCA (Poly)"
, xlab="First component", ylab="Second component")



# Gaussian Kernel
# k(x,y) = exp(-sum((x-y)^2)))
k2 = function (x,y) { dnorm(norm(matrix(x-y), type="F"))}
K = matrix(0, ncol = N_total, nrow = N_total)
for (i in 1:N_total) {
  for (j in 1:N_total) { 
    K[i,j] = k2(X[i,], X[j,])
}}
ones = 1/N_total* matrix(1, N_total, N_total)
K_norm = K - ones %*% K - K %*% ones + ones %*% K %*% ones
res = eigen(K_norm)

V = res$vectors
D = diag(res$values)

Y = K %*% V
plot(Y[,1], Y[,2], col = rainbow(3)[label], main = "Kernel PCA (Gaussian)"
, xlab="First component", ylab="Second component")


