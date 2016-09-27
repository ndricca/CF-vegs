############ START ############ 
rm(list=ls())
setwd("C:\\Users\\Andrea\\Desktop\\FEM\\Italian Taste\\Modello\\")
options(digits = 5)
set.seed(123)
par(mfrow=c(1,1))
# Load libraries
library(recommenderlab)
library(ggplot2)
# library(devtools)
# install_github("ggbiplot", "vqv")
library(ggbiplot)
# library(pls)
library(data.table)
library(caret)

############ Data Preparation ############
# Loading data
# Desktop PC
# data <- read.csv("C:\\Users\\Casa\\Desktop\\Andrea\\italian taste\\IT_TN1.csv",header=T,sep=";")
# Notebook
data <- read.csv("IT_TN1.csv",header=T,sep=";")
data <- data[,1:883]
# split data into training and test set
train_ind <- sample(seq_len(nrow(data)), size = 0.8*nrow(data))
train <- data[train_ind, ]
# valid_ind <- sample(seq_len(nrow(train)), size = 0.8*nrow(train))
# train <- train[valid_ind,]
# valid <- train[-valid_ind,]
test <- data[-train_ind, ]
# View(train[order(train[,4]), ]) 
# View(test[order(test[,4]), ]) # check train and test differ
rm(train_ind)

# Familiarity matrix
# familiarity <- data[,185:368]
# familiarity <- train[,185:368]
fam <- as.matrix(train[,185:368])
# Rating matrix
# rat <- as.matrix(data[,369:552])
rat <- as.matrix(train[,369:552])
# rat1 <- as.matrix(data[,369:552])
rat[fam==1|fam==2] <- NA # Input rating = NA every time familiarity is low (ie fam < 3)
# Visualize ratings
ratings <- as(rat, "realRatingMatrix")
# ratings_n <- normalize(ratings)
# head(ratings)
image(normalize(ratings),main="Normalized Ratings matrix",ylab="Users",xlab="Products") # graph 1

# # rat <- as(ratings_n,"matrix")

############ Similarity matrices ############
# Function: Adjusted Cosine similarity function
avg.rat.prod <- colMeans(ratings,na.rm = TRUE)
qplot(seq_along(avg.rat.prod), avg.rat.prod,main = "Product average ratings") # graph 2
avg.rat.user <- rowMeans(ratings,na.rm = TRUE)
qplot(seq_along(avg.rat.user), avg.rat.user,main = "User average ratings") # graph 3
u <- as(avg.rat.user,'matrix')
rm(fam,avg.rat.prod,avg.rat.user,ratings) # cleaning environment
getAdjCos <- function(i,j) {
  nuser=table(complete.cases(cbind(i,j,u)))["TRUE"]
  tmp01 <- cbind(i,j,u)
  concat<- matrix(data=tmp01[complete.cases(tmp01),], nrow = nuser, ncol = 3)
  i=concat[,1]
  j=concat[,2]
  u=concat[,3]
  # Compute adjusted cosine similarity
  adj.sim.ij <- sum((i-u)*(j-u))/(sqrt(sum((i-u)^2))*sqrt(sum((j-u)^2)))
  return(adj.sim.ij)
}

# Function: Correlation similarity function 
getCorr <- function(i,j) {
  ii <- i[complete.cases(i,j)]
  jj <- j[complete.cases(i,j)]
  corr.ij <- cor(ii,jj,method = "pearson")
}

# Get the Adjusted Cosine and the Correlation similarity matrices

if (file.exists("AdjCos.similarity.RData") & file.exists("Corr.similarity.RData")) {
  load("AdjCos.similarity.RData")
  load("Corr.similarity.RData")
} else {
  AdjCos.similarity.pt  <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
  Corr.similarity.pt <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
  a <- Sys.time()
  pb.sim <- txtProgressBar(min = 1, max = ncol(rat), initial = 0, style = 3) 
  for (i in 1:ncol(rat)) {
    for (j in 1:ncol(rat)) {
      if (i!=j) {
        AdjCos.similarity[i,j] <- getAdjCos(as.matrix(rat[,i]),as.matrix(rat[,j]))
        Corr.similarity[i,j] <- getCorr(as.matrix(rat[,i]),as.matrix(rat[,j]))
      }
      setTxtProgressBar(pb.sim,i)
    }
  }
  b <- Sys.time()
  b-a # Time difference of 31.98095 secs
  rm(pb.sim,a,b,i,j)
  save(AdjCos.similarity,file = "AdjCos.similarity.RData")
  save(Corr.similarity,file = "Corr.similarity.RData")
}
rm(u)
# #  Plot similarities

# ########### Most Similar: c-v parameter tuning ###########
# # How many similar products? 5-fold cross validated parameter tuning 
# n <- nrow(train)
# K <- 5
# size <- n %/% K
# set.seed(123)
# rdm <- runif(n)
# ranked <- rank(rdm)
# block <- (ranked-1) %/% size+1
# block <- as.factor(block)
# rm(n,rdm,ranked)
# 
# # Function: compute weighted sum 
# getWeightedSum <- function(k,z) {
#   pred <- sum(k*z,na.rm = T)/sum(z,na.rm = T)
#   return(pred)
# }
# 
# # Cross-validation: training and validation sets
#     RMSE.adj.v <- matrix(NA,nrow = 5,ncol = 10)
#     RMSE.corr.v <- matrix(NA,nrow = 5,ncol = 10)
#     MAE.adj.v <- matrix(NA,nrow = 5,ncol = 10)
#     MAE.corr.v <- matrix(NA,nrow = 5,ncol = 10)
# for (c in 1:K) {
#   # c <- 2
#   train<-data[block!=c,]
#   valid<-data[block==c,]
# 
#   # train  
#   fam <- as.matrix(train[,185:368])
#   rat <- as.matrix(train[,369:552])
#   rat[fam==1|fam==2] <- NA # Input rating = NA every time familiarity is low (ie fam < 3)
#   
#   # valid
#   fam.v<- as.matrix(valid[,185:368])
#   rat.v <- as.matrix(valid[,369:552])
#   rat.v[fam.v==1|fam.v==2] <- NA 
#   rat.v[,9:37] <- NA
#   rat.v1 <- as.matrix(valid[,369:552])
#   
#   avg.rat.user <- rowMeans(rat,na.rm = TRUE)
#   u <- as(avg.rat.user,'matrix')
#   
#   AdjCos.similarity.pt  <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
#   Corr.similarity.pt <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
#   a <- Sys.time()
#   pb.sim <- txtProgressBar(min = 1, max = ncol(rat), initial = 0, style = 3) 
#   for (i in 1:ncol(rat)) {
#     for (j in 1:ncol(rat)) {
#       if (i!=j) {
#         AdjCos.similarity.pt[i,j] <- getAdjCos(as.matrix(rat[,i]),as.matrix(rat[,j]))
#         Corr.similarity.pt[i,j] <- getCorr(as.matrix(rat[,i]),as.matrix(rat[,j]))
#       }
#       setTxtProgressBar(pb.sim,i)
#     }
#   }
#   b <- Sys.time()
#   b-a # Time difference of 31.98095 secs
#   rm(pb.sim,a,b,i,j)
#   
#     
#     l <- 1
#     for (p in seq(0.1,1,0.1)) {
#       true.v <- rat.v1
#       ms.adj <- apply(AdjCos.similarity.pt,2,function(x){ifelse(x >= quantile(x,p,na.rm = T),x,NA)})
#       ms.cor <- apply(Corr.similarity.pt,2,function(x){ifelse(x >= quantile(x,p,na.rm = T),x,NA)})
#       # Create predicted and true matrices 
#       pred.adj.v <- matrix(NA, nrow = nrow(rat.v),ncol = ncol(rat.v))
#       pred.corr.v <- matrix(NA, nrow = nrow(rat.v),ncol = ncol(rat.v))
#       a <- Sys.time()
#       for (u in 1:nrow(rat.v)) {
#         for (i in 9:37) {
#           if (!is.na(true.v[u,i])){
#             # prediction from AdjCos similarity matrix
#             k <- rat.v[u,!is.na(ms.adj[,i])]
#             z <- ms.adj[!is.na(ms.adj[,i]),i]
#             pred.adj.v[u,i] <- getWeightedSum(k,z)
#             # prediction from Corr similarity matrix
#             kk <- rat.v[u,!is.na(ms.cor[,i])]
#             zz <- ms.cor[!is.na(ms.cor[,i]),i]
#             pred.corr.v[u,i] <- getWeightedSum(kk,zz)
#             # diff[u,i] <- true[u,i] - pred[u,i]
#           }
#         }
#       }
#       b <- Sys.time()
#       b-a
#       # rm(a,b,i,u,k,z,kk,zz,rat.v1)
#       # Name the products
#       colnames(pred.adj.v) <- colnames(rat.v)
#       colnames(pred.corr.v) <- colnames(rat.v)
#       # Select predictions for the veggies
#       pred.adj.v <- pred.adj.v[,9:37]
#       pred.corr.v <- pred.corr.v[,9:37]
#       # Difference matrices
#       true.v <- true.v[,9:37]
#       diff.adj.v <- true.v-pred.adj.v
#       diff.corr.v <- true.v-pred.corr.v
#       # Name the products
#       colnames(diff.adj.v) <- colnames(pred.adj.v)
#       colnames(diff.corr.v) <- colnames(pred.corr.v)
#       # RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
#       k <- nrow(pred.adj.v)*ncol(pred.adj.v)
#       RMSE.adj.v[c,l] <- sqrt(sum(rowSums(diff.adj.v^2,na.rm = T),na.rm = T)/abs(k))
#       # R,MSE.adj = 3.228498
#       RMSE.corr.v[c,l] <- sqrt(sum(rowSums(diff.corr.v^2,na.rm = T),na.rm = T)/abs(k))
#       # RMSE.corr = 3.215119
#       # MAE: sum(abs(pred-true))/k
#       MAE.adj.v[c,l] <- sum(abs(pred.adj.v-true.v),na.rm = T)/k 
#       # MAE.adj = 2.833856
#       MAE.corr.v[c,l] <- sum(abs(pred.corr.v-true.v),na.rm = T)/k 
#       # MAE.corr = 2.822884
#       l <- l+1
#       # rm(k)
#     }
# }
#   
# p.tune <- data.frame(
#   Parameter = rep(c(sapply(seq(0.1,1,0.1),function(x){rep(x,5)})),4),
#   Error = c(RMSE.adj.v,RMSE.corr.v,MAE.adj.v,MAE.corr.v), 
#   ErrorType = c(rep("RMSE",100),rep("MAE",100)),
#   Distance = c(rep("Adjusted Cosine",50),rep("Correlation",50),
#                rep("Adjusted Cosine",50),rep("Correlation",50)),
#   Fold = rep(c(t(sapply(1:5,function(x){rep(x,10)}))),4))
# 
# RMSE.a.mean <- apply(RMSE.adj.v,2,mean)
# RMSE.c.mean <- apply(RMSE.corr.v,2,mean)
# MAE.a.mean <- apply(MAE.adj.v,2,mean)
# MAE.c.mean <- apply(MAE.corr.v,2,mean)
# 
# p.tune.mean <- data.frame(
#   Parameter = rep(seq(0.1,1,0.1),4),
#   Error = c(RMSE.a.mean,RMSE.c.mean,MAE.a.mean,MAE.c.mean),
#   ErrorType = c(rep("RMSE",20),rep("MAE",20)),
#   Distance = c(rep("Adjusted Cosine",10),rep("Correlation",10),
#                rep("Adjusted Cosine",10),rep("Correlation",10)))
# 
# p.tune.plot <- ggplot(p.tune,aes(x=Parameter,y=Error,group=interaction(Fold,ErrorType),color=ErrorType)) + 
#   geom_line() + facet_wrap(~ Distance)
# p.tune.plot <- p.tune.plot +
#   geom_line(data = p.tune.mean,aes(x=Parameter,y=Error,group=ErrorType,color=ErrorType),size=1.1) +
#   geom_line() +
#   facet_wrap(~ Distance)
# p.tune.plot
# 
# DT <- data.table(p.tune.mean)
# tmp <- DT[,.SD[which.min(Error)],by=.(Distance,ErrorType)]
# p.tune.plot <- p.tune.plot + 
#   geom_text(data=tmp,aes(x=Parameter,y=Error-0.07,group=ErrorType,label=round(Error,2)),size=4,color="black",show_guide=FALSE) +
#   # geom_point(data=tmp)
#   scale_x_continuous(breaks=seq(0.1,0.9,0.2)) +
#   theme(legend.position="bottom") +
#   ggtitle("Parameter tuning: \n which number of similar products \n is better to pick?")
# p.tune.plot

########### Most similar: matrices ###########
# Adjusted cosine similarity
if (file.exists("most.similar.adj.RData")) {
  load("most.similar.adj.RData")
} else {
  most.similar.adj <- apply(AdjCos.similarity,2,function(x){ifelse(x >= quantile(x,0.5,na.rm = T),x,NA)})
  save(most.similar.adj,file = "most.similar.adj.RData")
}
# View(most.similar.adj)

# Correlation similarity
if (file.exists("most.similar.corr.RData")) {
  load("most.similar.corr.RData")
} else {
  most.similar.corr <- apply(Corr.similarity,2,function(x){ifelse(x >= quantile(x,0.3,na.rm = T),x,NA)})
  save(most.similar.corr,file = "most.similar.corr.RData")
}
# View(most.similar.corr)

########### Test set ###########
# Familiarity matrix - test set
fam.t<- as.matrix(test[,185:368])
# Rating matrix - test set
rat.t <- as.matrix(test[,369:552])
# Input rating = NA every time familiarity is low (ie fam < 3)
# and remove vegetables ratings from the test set
rat.t[fam.t==1|fam.t==2] <- NA 
rat.t[,9:37] <- NA
# Comparison matrix
rat.t1 <- as.matrix(test[,369:552])
rm(fam.t)
# Function: compute weighted sum 
getWeightedSum <- function(k,z) {
  # function(u,i) { # old version
  # Example: user = 4, product = 11
  # u <- 7
  # i <- 14
  # k <- rat.t[u,!is.na(most.similar.adj[,i])]
  # z <- most.similar.adj[!is.na(most.similar.adj[,i]),i]
  pred <- sum(k*z,na.rm = T)/sum(z,na.rm = T)
  return(pred)
}

######## Create predicted and true matrices ########
pred.adj <- matrix(NA, nrow = nrow(rat.t),ncol = ncol(rat.t))
pred.corr <- matrix(NA, nrow = nrow(rat.t),ncol = ncol(rat.t))
true <- rat.t1
a <- Sys.time()
for (u in 1:nrow(rat.t)) {
  for (i in 9:37) {
    # there are products not rated by the users in
    # the test set --> "if statement" necessary 
    # to exclude them from the prediction
    # true[u,i] <- rat.t1[u,i] 
    if (!is.na(true[u,i])){
      # prediction from AdjCos similarity matrix
      k <- rat.t[u,!is.na(most.similar.adj[,i])]
      z <- most.similar.adj[!is.na(most.similar.adj[,i]),i]
      # pred.adj[u,i] <- round(getWeightedSum(k,z))
      pred.adj[u,i] <- getWeightedSum(k,z)
      # prediction from Corr similarity matrix
      kk <- rat.t[u,!is.na(most.similar.corr[,i])]
      zz <- most.similar.corr[!is.na(most.similar.corr[,i]),i]
      # pred.corr[u,i] <- round(getWeightedSum(kk,zz))
      pred.corr[u,i] <- getWeightedSum(kk,zz)
      # diff[u,i] <- true[u,i] - pred[u,i]
    }
  }
}
b <- Sys.time()
b-a
rm(a,b,i,u,k,z,kk,zz,rat.t1)
# Time difference of 0.1685388 secs

# Name the products
colnames(pred.adj) <- colnames(rat.t)
colnames(pred.corr) <- colnames(rat.t)
# Select predictions for the veggies
pred.adj <- pred.adj[,9:37]
pred.corr <- pred.corr[,9:37]
# Difference matrices
true <- true[,9:37]
diff.adj <- true-pred.adj
diff.corr <- true-pred.corr

# # View(pred.adj)
# # View(pred.corr)
# # View(true)

colnames(diff.adj) <- colnames(pred.adj)
colnames(diff.corr) <- colnames(pred.corr)
# View(diff.adj)
# View(diff.corr)
######### Evaluation metrics #########
mean(colMeans(diff.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.corr, na.rm=T),na.rm = T)
# In mean, considering raw differences, true ratings are 2.659349 and 2.661307 points more than 
# the corrispondent predicted ratings --> the algorithm underestimates vegetable ratings

# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
k <- nrow(pred.adj)*ncol(pred.adj)
RMSE.adj <- sqrt(sum(rowSums(diff.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.adj = 3.228498
RMSE.corr <- sqrt(sum(rowSums(diff.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.corr = 3.215119

# MAE: sum(abs(pred-true))/k
MAE.adj <- sum(abs(pred.adj-true),na.rm = T)/k 
# MAE.adj = 2.833856
MAE.corr <- sum(abs(pred.corr-true),na.rm = T)/k 
# MAE.corr = 2.822884
rm(k)

######### Data preparation: bitter taste #########
# Prediction formula modification with bitter tasting preferences.
# What are the worst results in terms of difference between 
# predicted and actual ratings?
worstres.prod.adj <- ifelse(colMeans(diff.adj, na.rm=T) >= mean(colMeans(diff.adj, na.rm=T)), 1, 0)
worstres.prod.corr <- ifelse(colMeans(diff.corr, na.rm=T) >= mean(colMeans(diff.corr, na.rm=T)), 1, 0)
# what are the most bitter vegetables?
bitterveg <- c(1,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,1)  
names(bitterveg) <- names(worstres.prod.corr)
# What is the correlation between these two?
cor(worstres.prod.adj,bitterveg) # 0.6505161
cor(worstres.prod.corr,bitterveg) # 0.4313725
rm(worstres.prod.adj,worstres.prod.corr)

# Load ratings users did to 4 chocolate creams with different amounts of sugar
bit.lik.train <- as.matrix(train[,110:113])
bit.lik.test <- as.matrix(test[,110:113])

# PCA on chocolate liking (with different sugar concentration)
bit.lik.pca <- prcomp(bit.lik.train, center = TRUE, scale. = TRUE)
print(bit.lik.pca)
# plot(bit.lik.pca, type = "l")
summary(bit.lik.pca)

# Biplot 
pca.lik.plot <- ggbiplot(bit.lik.pca, obs.scale = 1, var.scale = 1)
# pca.lik.plot <- pca.lik.plot + scale_color_discrete(name = '')
# pca.lik.plot <- pca.lik.plot + theme(legend.direction = "horizontal", legend.position = "topright")
pca.lik.plot

######### New pred - 1: scaled bitter score added to the predicted rating ######### 
sweet.pc <- predict(bit.lik.pca, bit.lik.test)[,2]
bitter.pc <- -1*sweet.pc
# bitter.pc <- sweet.pc
max(rat,na.rm = T)
min(rat,na.rm = T)
# scale bitter.pc in terms of rating points (1:9)
bitter.pc.n <- ((max(rat,na.rm = T)-min(rat,na.rm = T))/(max(bitter.pc)-min(bitter.pc)))*(bitter.pc-max(bitter.pc))+max(rat,na.rm = T)
# min(true,na.rm = T)

pred.b.adj <- pred.adj
pred.b.corr <- pred.corr
# Add the scaled bitter score for every user of the test set
for (j in 1:ncol(pred.corr)){
  for (i in 1:nrow(pred.corr)) {
    # j<-29
    # i<-1
    if(bitterveg[j]!=0){
      # pred.b.adj[,j] <- round(pred.adj[,j] + bitter.pc.n)
      # pred.b.corr[,j] <- round(pred.corr[,j] + bitter.pc.n)
      pred.b.adj[,j] <- pred.adj[,j] + bitter.pc.n
      pred.b.corr[,j] <- pred.corr[,j] + bitter.pc.n
    }
  }
}
pred.b.adj[pred.b.adj>9]<-9
pred.b.corr[pred.b.corr>9]<-9

# View(pred.b.adj)
# View(pred.b.corr)
# # View(true)

# pred <- pred[,9:37]
# true <- true[,9:37]
# diff <- diff[,9:37]
diff.b.adj <- true-pred.b.adj
diff.b.corr <- true-pred.b.corr
colnames(diff.b.adj) <- colnames(pred.adj)
colnames(diff.b.corr) <- colnames(pred.corr)
# View(diff.b.adj)
rm(bitter.pc,bitter.pc.n)
######### New pred - 1: evaluation metrics #########
mean(colMeans(diff.b.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.b.corr, na.rm=T),na.rm = T)
# In mean, true ratings are higher than predicted of 2.659349 points

# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
k <- nrow(pred.b.adj)*ncol(pred.b.adj)
RMSE.b.adj <- sqrt(sum(rowSums(diff.b.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b.adj = 2.807011 
RMSE.b.corr <- sqrt(sum(rowSums(diff.b.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b.corr = 2.916551 

# MAE: sum(abs(pred-true))/k
MAE.b.adj <- sum(abs(pred.b.adj-true),na.rm = T)/k 
# MAE.b.adj = 2.252351 
MAE.b.corr <- sum(abs(pred.b.corr-true),na.rm = T)/k 
# MAE.b.corr = 2.34953 
rm(k)

######### New pred - 2: bitter score  weighted with a lm coeff #########
# bit.lik.pca$x[,2]
mod <- rep(NA,length = ncol(rat))
# bit.lik.pca$x[,2] could be interpreted as the principal component related to the liking for sweeter
# chocolate cream --> the more bitterness is appreciated, the less is bit.lik.pca$x[,2]
# i<-10
for (i in 9:37){ 
  y <- scale(rat[,i], center = T) ########### WARN ###
  x <- scale(bit.lik.pca$x, center = T)
  # mod[i] <- lm(y~x)$coefficients[3]
  mod[i] <- lm(rat[,i]~bit.lik.pca$x)$coefficients[3]
  #   pls.dat <- data.frame(rat = rat[,i],
  #                         choc = I(bit.lik.train))
  #   mod <- pcr(rat~choc,data=pls.dat,ncomp = 4,validation="LOO")
  # summary(mod)
  # mod$coefficients
  # mod$scores
  # mod$loadings
  # mod$loading.weights
  # plot(mod, ncomp = 2, asp = 1, line = TRUE)
  #   plot(mod, plottype = "biplot", comps = c(1,3))
}
mod <- mod[9:37]
# mod[j]
pred.b2.adj <- pred.adj
pred.b2.corr <- pred.corr
# Add the scaled bitter score for every user of the test set
for (j in 1:ncol(pred.corr)){
  # j=29
  if(bitterveg[j]!=0){
    # pred.b2.adj[,j] <- round(pred.adj[,j] + mod[j]*sweet.pc)
    # pred.b2.corr[,j] <- round(pred.b.corr[,j] + mod[j]*sweet.pc)
    pred.b2.adj[,j] <- pred.adj[,j] + mod[j]*sweet.pc
    pred.b2.corr[,j] <- pred.b.corr[,j] + mod[j]*sweet.pc
  }
}
pred.b2.adj[pred.b2.adj>9]<-9
pred.b2.corr[pred.b2.corr>9]<-9
diff.b2.adj <- true-pred.b2.adj
diff.b2.corr <- true-pred.b2.corr
colnames(diff.b2.adj) <- colnames(pred.adj)
colnames(diff.b2.corr) <- colnames(pred.corr)
rm(mod,i,j,x,y,sweet.pc)
######### New pred - 2: evaluation metrics #########
mean(colMeans(diff.b2.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.b2.corr, na.rm=T),na.rm = T)
# In mean, true ratings are 2.643205 and 0.6511787 points higher than predicted ones

k <- nrow(pred.b2.adj)*ncol(pred.b2.adj)
# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
RMSE.b2.adj <- sqrt(sum(rowSums(diff.b2.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b2.adj = 3.213412 
RMSE.b2.corr <- sqrt(sum(rowSums(diff.b2.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b2.corr = 3.141142 

# MAE: sum(abs(pred-true))/k
MAE.b2.adj <- sum(abs(pred.b2.adj-true),na.rm = T)/k 
# MAE.b2.adj = 2.824451 
MAE.b2.corr <- sum(abs(pred.b2.corr-true),na.rm = T)/k 
# MAE.b2.corr = 2.647335 
rm(k)

######### New pred - 3: taste intensity  #########
int<- as.matrix(train[,122:128])
coef <- matrix(0,ncol = 29,nrow = 7)
vegs <- rat[,9:37]
coef <- apply(vegs,2,function(x){ 
  mod <- lm(x~int-1)
  ifelse(summary(mod)$coef[,4]<=.1,summary(mod)$coef[,1],0)
})

int.t <- as.matrix(test[,122:128])

pred.i.adj <- pred.adj + (int.t %*% coef)
pred.i.corr <- pred.corr + (int.t %*% coef)
pred.i.adj[pred.i.adj>9]<-9
pred.i.corr[pred.i.corr>9]<-9
# max(pred.i.adj,na.rm = T) 
# min(pred.i.adj,na.rm = T) 
# max(pred.i.corr,na.rm = T) 
# min(pred.i.corr,na.rm = T) 
# max(true,na.rm = T)
# min(true,na.rm = T)

diff.i.adj <- true-pred.i.adj
diff.i.corr <- true-pred.i.corr
colnames(diff.i.adj) <- colnames(pred.adj)
colnames(diff.i.corr) <- colnames(pred.corr)
rm(coef,vegs)
####### New pred - 3: evaluation metrics ###### WARN #########
mean(colMeans(diff.i.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.i.corr, na.rm=T),na.rm = T)
# In mean, true ratings are 2.643205 and 0.6511787 points higher than predicted ones

k <- nrow(pred.i.adj)*ncol(pred.i.adj)
# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
RMSE.i.adj <- sqrt(sum(rowSums(diff.i.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.i.adj = 3.213412
RMSE.i.corr <- sqrt(sum(rowSums(diff.i.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.i.corr = 3.141142

# MAE: sum(abs(pred-true))/k
MAE.i.adj <- sum(abs(pred.i.adj-true),na.rm = T)/k
# MAE.i.adj = 2.824451
MAE.i.corr <- sum(abs(pred.i.corr-true),na.rm = T)/k
# MAE.i.corr = 2.647335
rm(k)

####### Comparison of models results and evaluation metrics ###### WARN ########

eval <- cbind.data.frame(
  Metrics = c(
    MAE.adj,MAE.b.adj,MAE.b2.adj,MAE.i.adj,
    MAE.corr,MAE.b.corr,MAE.b2.corr,MAE.i.corr,
    RMSE.adj,RMSE.b.adj,RMSE.b2.adj,RMSE.i.adj,
    RMSE.corr,RMSE.b.corr,RMSE.b2.corr,RMSE.i.corr),
  Model = rep(c("Base","B","B2","I"),4),
  Error = c(rep("MAE",8),rep("RMSE",8)),
  Distance = c(rep("Adjusted Cosine",4),rep("Correlation",4),rep("Adjusted Cosine",4),rep("Correlation",4)))
eval.plot <- ggplot(eval,aes(x=Model,y=Metrics,group=Error,color=Error))+geom_line()+geom_point()+
  # theme_minimal() +
  scale_x_discrete(limits=c("Base","B","B2","I")) +
  facet_wrap(~ Distance)
eval.plot

# eval <- cbind.data.frame(
#   MAE.adj.list = rbind(MAE.adj,MAE.b.adj,MAE.b2.adj,MAE.i.adj),
#   MAE.corr.list = rbind(MAE.corr,MAE.b.corr,MAE.b2.corr,MAE.i.corr),
#   RMSE.adj.list = rbind(RMSE.adj,RMSE.b.adj,RMSE.b2.adj,RMSE.i.adj),
#   RMSE.corr.list = rbind(RMSE.corr,RMSE.b.corr,RMSE.b2.corr,RMSE.i.corr))
# rownames(eval) <- c("Base","B","B2","I")
# eval.plot <- ggplot(eval,aes(seq_along(rownames(eval)))) +
#   geom_line(aes(y=MAE.adj.list,colour="MAE.adj.list")) +
#   geom_point(aes(y=MAE.adj.list),size = 3, shape="x") +
#   geom_line(aes(y=MAE.corr.list,colour="MAE.corr.list")) +
#   geom_point(aes(y=MAE.corr.list),size = 2) +
#   geom_line(aes(y=RMSE.adj.list,colour="RMSE.adj.list")) +
#   geom_point(aes(y=RMSE.adj.list),size = 3,shape="x") +
#   geom_line(aes(y=RMSE.corr.list,colour="RMSE.corr.list")) +
#   geom_point(aes(y=RMSE.corr.list),size = 2) +
#   theme_minimal() +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   scale_x_discrete(labels=rownames(eval)) +
#   ylab("Metrics") +
#   xlab("Model") + 
#   ggtitle("Evaluation metrics") +
#   labs(colour = "Metrics")
# eval.plot

#########       Rank and classify the ratings      ######### WARN ########
######### in order to show users top N vegetables ###
N <- 10
col_to_name <- function(col,mat) {
  dimnames(mat)[[2]][col]
}

# TOP N for adjcos sim
p.adj <- apply(pred.adj,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.adj <- apply(p.adj,2,function(x) col_to_name(x,pred.adj))
# View(pp.adj)

# TOP N for corr sim
p.corr <- apply(pred.corr,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.corr <- apply(p.corr,2,function(x) col_to_name(x,pred.corr))
# View(pp.corr)

# TOP N for adjcos sim b
p.b.adj <- apply(pred.b.adj,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.b.adj <- apply(p.b.adj,2,function(x) col_to_name(x,pred.b.adj))
# View(pp.b.adj)

# TOP N for corr sim b
p.b.corr <- apply(pred.b.corr,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.b.corr <- apply(p.b.corr,2,function(x) col_to_name(x,pred.b.corr))
# View(pp.b.corr)

# TOP N for adj sim b2
p.b2.adj <- apply(pred.b2.adj,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.b2.adj <- apply(p.b2.adj,2,function(x) col_to_name(x,pred.b2.adj))
# View(pp.b2.adj)

# TOP N for corr sim b2
p.b2.corr <- apply(pred.b2.corr,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.b2.corr <- apply(p.b2.corr,2,function(x) col_to_name(x,pred.b2.corr))
# View(pp.b.corr)

# TOP N for adj sim i
p.i.adj <- apply(pred.i.adj,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.i.adj <- apply(p.i.adj,2,function(x) col_to_name(x,pred.i.adj))
# View(pp.i.adj)

# TOP N for corr sim i
p.i.corr <- apply(pred.i.corr,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
pp.i.corr <- apply(p.i.corr,2,function(x) col_to_name(x,pred.i.corr))
# View(pp.b.corr)

# View(true)
# TRUE TOP N 
t <- apply(true,1,function(x){head(order(na.omit(x),decreasing = T),N) })
# View(p)
tt <- apply(t,2,function(x) col_to_name(x,true))
# View(tt)
rm(p.adj,p.corr,p.b.adj,p.b.corr,p.b2.adj,p.b2.corr,p.i.adj,p.i.corr,t)

# TOP N PREDICTIONS VS TRUE TOP N
count <- matrix(NA,nrow = 8,ncol = ncol(tt))
for (i in 1:ncol(tt)){
  count[1,i] <- length(intersect(tt[,i], pp.adj[,i]))
  count[2,i] <- length(intersect(tt[,i], pp.b.adj[,i]))
  count[3,i] <- length(intersect(tt[,i], pp.b2.adj[,i]))
  count[4,i] <- length(intersect(tt[,i], pp.i.adj[,i]))
  count[5,i] <- length(intersect(tt[,i], pp.corr[,i]))
  count[6,i] <- length(intersect(tt[,i], pp.b.corr[,i]))
  count[7,i] <- length(intersect(tt[,i], pp.b2.corr[,i]))
  count[8,i] <- length(intersect(tt[,i], pp.i.corr[,i]))
}
# View(count)

rightprods <- apply(count,1,mean)
res <- data.frame(AvgNumRightProds = rightprods, 
                  ModelClass = as.factor(c("Base","B","B2","I","Base","B","B2","I")), 
                  DistanceMeas = c(rep("Adjusted Cosine",4),rep("Correlation",4)))

res.plot <- ggplot(res, aes(ModelClass,AvgNumRightProds)) + 
  # theme_minimal() +
  geom_bar(aes(ModelClass,AvgNumRightProds,fill=ModelClass), stat="identity",width=.3) +
  scale_x_discrete(limits=c("Base","B","B2","I")) +
  facet_wrap(~ DistanceMeas) + 
  ylab("") +
  xlab("Models") + 
  theme(legend.position="none") +
  ggtitle("Average Number of Retrieved Vegs \n for Top 10 prediction")
res.plot

