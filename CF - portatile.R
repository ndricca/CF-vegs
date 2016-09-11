############ START ############ 
rm(list=ls())
set.seed(123)
par(mfrow=c(1,1))
# Load libraries
library(recommenderlab)
library(ggplot2)
library(devtools)
# install_github("ggbiplot", "vqv")
library(ggbiplot)
library(pls)

############ Data Preparation ############
# Loading data
# Desktop PC
# data <- read.csv("C:\\Users\\Casa\\Desktop\\Andrea\\italian taste\\IT_TN1.csv",header=T,sep=";")
# Notebook
data <- read.csv("C:\\Users\\Andrea\\Desktop\\FEM\\Italian Taste\\Modello\\IT_TN1.csv",header=T,sep=";")
data <- data[,1:883]
# split data into training and test set
train_ind <- sample(seq_len(nrow(data)), size = 0.8*nrow(data))
train <- data[train_ind, ]
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
# Input rating = NA every time familiarity is low (ie fam < 3)
for (i in 1:nrow(rat)) {
  for (j in 1:ncol(rat)) {
    if (fam[i,j]==1 |fam[i,j]==2) {
      rat[i,j] <- NA
    }
  }
}
ratings <- as(rat, "realRatingMatrix")
# ratings_n <- normalize(ratings) 
# head(ratings)
# image(normalize(ratings))
# View(as(ratings,"matrix"))

# avg.rat.prod <- colMeans(ratings,na.rm = TRUE)
# qplot(seq_along(avg.rat.prod), avg.rat.prod,main = "Product average ratings")
# avg.rat.user <- rowMeans(ratings,na.rm = TRUE) ## WARN ##
# # qplot(seq_along(avg.rat.user), avg.rat.user,main = "User average ratings")
# u <- as(avg.rat.user,'matrix')

# ############ RecommenderLab ############
# rec=Recommender(ratings[1:nrow(ratings)],method="IBCF", param=list(normalize = "Z-score",method="COsine"))
# print(rec)
# names(getModel(rec))
# recom <- predict(rec, ratings[1:nrow(ratings)], type="ratings")

############ My algorithm:  ############
############ Similarity matrices ############
# Function: Adjusted Cosine similarity function
avg.rat.user <- rowMeans(ratings,na.rm = TRUE)
# qplot(seq_along(avg.rat.user), avg.rat.user,main = "User average ratings")
u <- as(avg.rat.user,'matrix')

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
AdjCos.similarity  <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
Corr.similarity <- matrix(NA, nrow=ncol(rat), ncol=ncol(rat), dimnames=list(colnames(rat),colnames(rat)))
a <- Sys.time()
pb.sim <- txtProgressBar(min = 1, max = ncol(rat), initial = 0, style = 3) 
for (i in 1:ncol(rat)) {
  for (j in 1:ncol(rat)) {
    if (i==j) {
      AdjCos.similarity[i,j] <- NA
      Corr.similarity[i,j] <- NA
    } else {
      # i=3
      # j=5
      AdjCos.similarity[i,j] <- getAdjCos(as.matrix(rat[,i]),as.matrix(rat[,j]))
      Corr.similarity[i,j] <- getCorr(as.matrix(rat[,i]),as.matrix(rat[,j]))
      # Corr.similarity[i,j]
      # AdjCos.similarity[i,j]
      }
    setTxtProgressBar(pb.sim,i)
  }
}
b <- Sys.time()
b-a # Time difference of 31.98095 secs
rm(pb.sim,a,b)
# #  Plot similarities
# View(Corr.similarity)
# View(AdjCos.similarity)
# image(AdjCos.similarity,main="AdjCos similarity matrix")
# image(Corr.similarity,main="Corr similarity matrix")
# # Spot check
# summary(AdjCos.similarity)[,1:5]
# plot(AdjCos.similarity[,60])

############ Predictions ############

# Select for every product the most similar items 

########### Most similar (Adjusted cosine) ###########
most.similar.adj <- matrix(NA, nrow=nrow(AdjCos.similarity), ncol=ncol(AdjCos.similarity), dimnames = list(colnames(AdjCos.similarity),colnames(AdjCos.similarity)))
a <- Sys.time()
pb.similar <- txtProgressBar(min = 1, max = ncol(rat), initial = 0, style = 3)
for (i in 1:ncol(AdjCos.similarity)) {
  for (j in 1:ncol(AdjCos.similarity)) {
    if (!is.na(AdjCos.similarity[i,j])){
      if (AdjCos.similarity[i,j] >= quantile(AdjCos.similarity[,j],0.5,na.rm = T)) {
        most.similar.adj[i,j] <- AdjCos.similarity[i,j]
      }
    }
    setTxtProgressBar(pb.similar,i)
  }
}
b <- Sys.time()
b-a # Time difference of 17.14373 secs
rm(pb.similar,a,b)

# View(most.similar.adj)

########### Most similar (Correlation) ###########
# Adjusted cosine similarity
most.similar.corr <- matrix(NA, nrow=nrow(Corr.similarity), ncol=ncol(Corr.similarity), dimnames = list(colnames(Corr.similarity),colnames(Corr.similarity)))
a <- Sys.time()
pb.similar <- txtProgressBar(min = 1, max = ncol(rat), initial = 0, style = 3)
for (i in 1:ncol(Corr.similarity)) {
  for (j in 1:ncol(Corr.similarity)) {
    if (!is.na(Corr.similarity[i,j])){
      if (Corr.similarity[i,j] >= quantile(Corr.similarity[,j],0.5,na.rm = T)) {
        most.similar.corr[i,j] <- Corr.similarity[i,j]
      }
    }
    setTxtProgressBar(pb.similar,i)
  }
}
b <- Sys.time()
b-a # Time difference of 17.14373 secs
rm(pb.similar,a,b)

# View(most.similar.corr)

# Check what is the number of most similar items  ############
# # and that it is the same for every product
# a <- rep(NA,184)
# for (i in 1:ncol(most.similar)){
#   a[i] <- sum(!is.na(most.similar[,i]))
# }
# table(a)
# rm(a)
# # If quantile = 0.95 --> number of most.similar[,i] = 10
# # If quantile = 0.90 --> number of most.similar[,i] = 19
# # If quantile = 0.85 --> number of most.similar[,i] = 28
# # If quantile = 0.80 --> number of most.similar[,i] = 37
# # If quantile = 0.75 --> number of most.similar[,i] = 46

########### Test set ###########
# Familiarity matrix - test set
fam.t <- as.matrix(test[,185:368])
# Rating matrix - test set
rat.t <- as.matrix(test[,369:552])
# Input rating = na every time familiarity is low
for (i in 1:nrow(rat.t)) {
  for (j in 1:ncol(rat.t)) {
    if (fam.t[i,j]==1 |fam[i,j]==2) {
      rat.t[i,j] <- NA
    }
  }
}

# Remove vegetables ratings from the test set
rat.t[,9:37] <- NA

# Comparison matrix
rat.t1 <- as.matrix(test[,369:552])

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
true <- matrix(NA, nrow = nrow(rat.t),ncol = ncol(rat.t))
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
      kk <- rat.t[u,!is.na(most.similar.adj[,i])]
      zz <- most.similar.corr[!is.na(most.similar.adj[,i]),i]
      # pred.corr[u,i] <- round(getWeightedSum(kk,zz))
      pred.corr[u,i] <- getWeightedSum(kk,zz)
      # diff[u,i] <- true[u,i] - pred[u,i]
    }
  }
}
b <- Sys.time()
b-a
rm(a,b)
# Time difference of 0.1685388 secs

# Name the products
colnames(pred.adj) <- colnames(rat.t)
colnames(pred.corr) <- colnames(rat.t)
# Select predictions for the veggies
pred.adj <- pred.adj[,9:37]
pred.corr <- pred.corr[,9:37]
# Difference matrices
true <- true[,9:37]
diff.adj <- matrix(NA, nrow = nrow(true),ncol = ncol(true))
diff.adj <- true-pred.adj
diff.corr <- matrix(NA, nrow = nrow(true),ncol = ncol(true))
diff.corr <- true-pred.corr
# diff.adj <- diff.adj[,9:37]
# diff.corr <- diff.corr[,9:37]

# # View(pred.adj)
# # View(pred.corr)
# # View(true)

######### Evaluation metrics #########
colnames(diff.adj) <- colnames(pred.adj)
colnames(diff.corr) <- colnames(pred.corr)
# View(diff.adj)
# View(diff.corr)
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

# Checking different numbers of similar products, it seems that
# the mean differences mean(colMeans(diff, na.rm=T),na.rm = T) 
# (between predicted and actual ratings (averaged for both users
# and products) are minimized for quantile = 0.5 , i.e. the 92 
# most similar products.

######### Bitter taste #########
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
# Load ratings users did to 4 chocolate creams with different amounts of sugar
bit.lik.train <- as.matrix(train[,110:113])
bit.lik.test <- as.matrix(test[,110:113])

# PCA on chocolate liking (with different sugar concentration)
bit.lik.pca <- prcomp(bit.lik.train, center = TRUE, scale. = TRUE)
print(bit.lik.pca)
plot(bit.lik.pca, type = "l")
summary(bit.lik.pca)

# Biplot 
pca.lik.plot <- ggbiplot(bit.lik.pca, obs.scale = 1, var.scale = 1)
# pca.lik.plot <- pca.lik.plot + scale_color_discrete(name = '')
# pca.lik.plot <- pca.lik.plot + theme(legend.direction = "horizontal", legend.position = "topright")
print(pca.lik.plot)

######### New pred - 1: scaled bitter score added to the predicted rating ######### 
sweet.pc <- predict(bit.lik.pca, bit.lik.test)[,2]
bitter.pc <- -1*sweet.pc
# scale bitter.pc in terms of rating points (1:9)
bitter.pc.n <- ((9-1)/(max(bitter.pc)-min(bitter.pc)))*(bitter.pc-max(bitter.pc))+9
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
for (j in 1:ncol(pred.corr)){
  for (i in 1:nrow(pred.corr)) {
      if (pred.b.adj[i,j] >= 9 & !is.na(pred.b.adj[i,j])){
        pred.b.adj[i,j] <- 9
      }
      if (pred.b.corr[i,j] >= 9 & !is.na(pred.b.corr[i,j])){
        pred.b.corr[i,j] <- 9
      }
  }
}
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

######### New pred - 1: evaluation metrics #########
mean(colMeans(diff.b.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.b.corr, na.rm=T),na.rm = T)
# In mean, true ratings are higher than predicted of 2.659349 points

# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
k <- nrow(pred.adj)*ncol(pred.adj)
RMSE.b.adj <- sqrt(sum(rowSums(diff.b.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b.adj = 2.807011 
RMSE.b.corr <- sqrt(sum(rowSums(diff.b.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b.corr = 2.916551 

# MAE: sum(abs(pred-true))/k
MAE.b.adj <- sum(abs(pred.b.adj-true),na.rm = T)/k 
# MAE.b.adj = 2.252351 
MAE.b.corr <- sum(abs(pred.b.corr-true),na.rm = T)/k 
# MAE.b.corr = 2.34953 

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
diff.b2.adj <- true-pred.b2.adj
diff.b2.corr <- true-pred.b2.corr
colnames(diff.b2.adj) <- colnames(pred.adj)
colnames(diff.b2.corr) <- colnames(pred.corr)

######### New pred - 2: evaluation metrics #########
mean(colMeans(diff.b2.adj, na.rm=T),na.rm = T)
mean(colMeans(diff.b2.corr, na.rm=T),na.rm = T)
# In mean, true ratings are 2.643205 and 0.6511787 points higher than predicted ones

# RMSE: sqrt(sum((true[i,j]-pred[i,j])^2)/abs(k))
k <- nrow(pred.adj)*ncol(pred.adj)
RMSE.b2.adj <- sqrt(sum(rowSums(diff.b2.adj^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b2.adj = 3.213412 
RMSE.b2.corr <- sqrt(sum(rowSums(diff.b2.corr^2,na.rm = T),na.rm = T)/abs(k))
# RMSE.b2.corr = 3.141142 

# MAE: sum(abs(pred-true))/k
MAE.b2.adj <- sum(abs(pred.b2.adj-true),na.rm = T)/k 
# MAE.b2.adj = 2.824451 
MAE.b2.corr <- sum(abs(pred.b2.corr-true),na.rm = T)/k 
# MAE.b2.corr = 2.647335 

####### COMPARISON OF MODELS RESULTS AND EVALUATION METRICS ###### WARN ########
MAE.eval <- cbind.data.frame(Model = c("Classic","New Pred 1","New Pred 2"),
                      MAE.adj.list = c(MAE.adj,MAE.b.adj,MAE.b2.adj),
                      MAE.corr.list = c(MAE.corr,MAE.b.corr,MAE.b2.corr))
RMSE.eval <- cbind.data.frame(Model = c("Classic","New Pred 1","New Pred 2"),
                              RMSE.adj.list = c(RMSE.adj,RMSE.b.adj,RMSE.b2.adj),
                              RMSE.corr.list = c(RMSE.corr,RMSE.b.corr,RMSE.b2.corr))
# e <- ggplot() + 
#   geom_line(data = MAE.eval, aes(x = Model, y = MAE.adj.list, color = "red")) +
#   geom_line(data = MAE.eval, aes(x = Model, y = MAE.corr.list, color = "blue"))  +
#   xlab('Model') +
#   ylab('MAE')

eval <- cbind.data.frame(Model = c("Classic","New Pred 1","New Pred 2"),
                         MAE.adj.list = c(MAE.adj,MAE.b.adj,MAE.b2.adj),
                         MAE.corr.list = c(MAE.corr,MAE.b.corr,MAE.b2.corr),
                         RMSE.adj.list = c(RMSE.adj,RMSE.b.adj,RMSE.b2.adj),
                         RMSE.corr.list = c(RMSE.corr,RMSE.b.corr,RMSE.b2.corr))
# eval.plot <- ggplot(eval, aes(x=Model,y=MAE.adj.list)) + geom_point()
# eval.plot <- eval.plot + expand_limits(y = c(1, 4))
# eval.plot <- eval.plot + geom_line()
# print(eval.plot)

# 
# aa <- rep(NA,29)
# for (i in 1:ncol(true)) {
#   aa[i] <- cor(pred.adj[,i],true[,i],use = "pairwise")
# }
# names(aa) <- colnames(true)
# cor(aa,bitterveg)
#


# #
# b <- rep(NA,22)
# for (i in 1:nrow(true)) {
#   b[i] <- cor(pred.b.adj[i,],true[i,],use = "pairwise")
# }
# b
# summary(b)

######### Rank and classify the ratings ######### WARN ########
######### in order to show users top 5 vegetables #
# View(pred.b.adj)
# TOP 10 for adjcos sim
col_to_name.adj <- function(col) {
  dimnames(pred.adj)[[2]][col]
}
p.adj <- apply(pred.adj,1,function(x){head(order(na.omit(x),decreasing = T),10) })
# View(p)
pp.adj <- apply(p.adj,2,function(x) col_to_name.adj(x))
# View(pp.adj)
# PER QUASI TUTTI VIENE SCELTO Bastoncini.di.verdure.con.salsine!!! :-( #
table(true[2,])
# TOP 10 for corr sim
col_to_name.corr <- function(col) {
  dimnames(pred.corr)[[2]][col]
}
p.corr <- apply(pred.corr,1,function(x){head(order(na.omit(x),decreasing = T),10) })
# View(p)
pp.corr <- apply(p.corr,2,function(x) col_to_name.corr(x))
# View(pp.corr)
# PER QUASI TUTTI VIENE SCELTO Pomodori!!! :-( #

# TOP 10 for adjcos sim b
col_to_name.b.adj <- function(col) {
  dimnames(pred.b.adj)[[2]][col]
}
p.b.adj <- apply(pred.b.adj,1,function(x){head(order(na.omit(x),decreasing = T),10) })
# View(p)
pp.b.adj <- apply(p.b.adj,2,function(x) col_to_name.b.adj(x))
# View(pp.b.adj)
# PER QUASI TUTTI VIENE SCELTO Broccoli!!! :-( #

# TOP 10 for corr sim b
col_to_name.b.corr <- function(col,mat) {
  dimnames(pred.b.corr)[[2]][col]
}
p.b.corr <- apply(pred.b.corr,1,function(x){head(order(na.omit(x),decreasing = T),10) })
# View(p)
pp.b.corr <- apply(p.b.corr,2,function(x) col_to_name.b.corr(x))
# View(pp.b.corr)
# PER QUASI TUTTI VENGONO SCELTE LE RAPE ROSSE!!! :-( #

# View(true)
# TRUE TOP 10 
col_to_name.true <- function(col) {
  dimnames(true)[[2]][col]
}
t <- apply(true,1,function(x){head(order(na.omit(x),decreasing = T),10) })
# View(p)
tt <- apply(t,2,function(x) col_to_name.b.corr(x))
# View(tt)

count <- rep(NA,ncol(tt))
for (i in 1:ncol(tt)){
  count[i] <- length(intersect(tt[,i], pp.b.adj[,i]))
}
count





# head(pred.b.adj[i,order(pred.b.adj[i,],na.last = T,decreasing = T)],5)
# head(true[i,order(true[i,],na.last = T,decreasing = T)],5)

######### PLS ######### WARN #########
# PLS regression of sensory taste on liking
# library(pls)
# bit.sens.train <- as.matrix(train[,141:156])
# cons.taste.train <- data.frame(liking = I(bit.lik.train))
# cons.taste.train$sensory <- bit.sens.train
# # str(cons.taste.train)
# 
# res <- plsr(liking ~ sensory, ncomp = 10, data = cons.taste.train, validation = "CV")
# summary(res)
# plot(RMSEP(res), legendpos = "topright") # 3 components are selected (min RMSEP)
# plot(res, plottype = "scores", comps = 1:3)
# # par(mfrow=c(2,2))
# plot(res, plottype = "biplot", comps = 1:2)
# plot(res, plottype = "biplot", comps = c(1,3))
# plot(res, plottype = "biplot", comps = 2:3)
# # par(mfrow=c(1,1))
# plot(res, plottype = "correlation", comps = 1:3)
# axis(1,las=3)
# plot(res, "loadings", comps = 1:3,
#      labels = "names",      xlab = colnames(bit.sens.train)
#      )
# abline(h = 0)

######### ROC curve ######### WARN #########
# Choose a threshold in order to consider a vegetable as favourite for the consumer or not
# (e.g.  if the predicted rating is >= 5)

# Users rated more or less 29 products of the test set. Some of them with more than the threshold.
# They are part of the "relevant information set".
# The algorithm predict ratings for each user for each product, more or less. Some of those above 
# the threshold. These products are part of the "retrieved information set".
# These two set could have different dimension and they can contain different products.
# Hence, it is possible to compute sensibility and specificity, and a ROC curve can be obtained
# using different thresholds.

p.b.a <- t(apply(pred.b.adj,1,function(x){ ifelse(x >= mean(na.omit(x)),1,0)}))
# View(pred.b.adj)
View(p.b.a)
rowMeans(pred.b.adj,na.rm = T)
View(pred.b.adj)
t <- t(apply(true,1,function(x){ ifelse(x >= mean(na.omit(x)),1,0)}))
# View(pred.b.adj)
View(t)
rowMeans(true,na.rm = T)
View(true)
p.b2.a <- apply(pred.b2.adj,2,function(x){ ifelse(x >= 5,1,0)})
View(pred.b.adj)
View(p.b2.a)








###################################### TRASH #########
# 
# rowCounts()
# a <- names(ratings)
# names(ratings)<- seq(1,184)
# 
# table(ratings$`1`)
# 
# for (j in 1:184) {
#     ratings$`j`[ratings$`j` %in% "N"] <- NA
# }
# 
# 
# for (i in 1:nrow(ratings)) {
#   ratings[,i] <- as.numeric(ratings[,i])
# }
# ratings$"1"
# 
# ratings[,i] <- as.character(ratings[,i])  
# ratings$i[ratings$i == "N"] <- NA
# ratings$i[ratings$i == "N"] <- NA
# ratings[,i] <- as.character(ratings[,i])  
# ratings[,1]
# table(data[,368],data[,883])
# table(data[,185],data[,369])
# plot(data[,185:221])
# 
# getCorrDist <- function(x,y)
# {
#   this.cosine <- sum((x-mean(x))*(y-mean(y))) / (sqrt(sum((x-mean(x))*(x-mean(x)))) * sqrt(sum((y-mean(y))*(y-mean(y)))))
#   return(this.cosine)
# }
# 
# rowMeans(as.matrix(ratings))
# 
# getAdjCosine <- function(i,j) {
#   avg.rate <- rowMeans(ratings)
#   adj.cos <- 
# }
# 
# 
# similarity  <- matrix(NA, nrow=ncol(ratings),
#                       ncol=ncol(ratings),
#                       dimnames=list(colnames(ratings),colnames(ratings)))
# 
# similarity.1  <- matrix(NA, nrow=ncol(ratings),
#                       ncol=ncol(ratings),
#                       dimnames=list(colnames(ratings),colnames(ratings)))
# 
# 
# for(i in 1:ncol(ratings)) {
#   for(j in 1:ncol(ratings)) {
#     similarity.1[i,j] <- Adjusted_cosine_similarity(ratings[,i],ratings[,j])
#   }
# }
# 
# for(i in 1:ncol(ratings)) {
#   for(j in 1:ncol(ratings)) {
#     similarity[i,j] <- getCorrDist(as.matrix(ratings[,i]),as.matrix(ratings[,j]))
#   }
# }
# 
# 
# # Back to dataframe
# similarity <- as.data.frame(similarity)
# #
# View(similarity)
# sim <- as(similarity, "realRatingMatrix")
# sim_m <- normalize(sim)
# image(similarity)
# str(similarity)
# 
# str(r)
# rr <- as.matrix(ratings)
# 
# str(rr)
# sum(!is.character(ratings))
# 
# 
# 
# 
# View(similarity)
# # 
# # # library(recommenderlab)
# # 
# # r <- as(ratings, "realRatingMatrix")
# # r_m <- normalize(r)
# # image(r_m, main = "Normalized Ratings")
# # table(ratings)
# # 
# 
# # head(r_m)
# # image(r_m, main = "Normalized Ratings")
# 
# var <- as.vector(colMeans(ratings))
# rrrr <- as.vector(colnames(ratings))
# fff <- as.data.frame(c(var,rrrr))
# fff <- as.data.frame(fff)
# qplot(colnames(ratings),colMeans(ratings)) 
# + geom_point() 
# + geom_text(data=subset(ratings, colMeans(ratings) < 3))
# 
#     data=subset(ratings, colMeans(ratings) < 4),
#             aes(colMeans(ratings),colnames(ratings),label=name))
# 
# # 
# # b <- as.vector(colMeans(ratings))
# # b$names <- colnames(ratings)
# # str(b)
# # plot(b)
# # View(b[b<4])
# 
# # a <- as.data.frame(colSums(ratings))
# # a$mean <- as.data.frame(colMeans(ratings))
# # levels(a$mean) <- colnames(ratings)
# # str(a)
# # View(a)
# # 
# # library(ggplot2)
# # # library("devtools")
# # # install.packages("ggvis")
# # # install.packages("googleVis")
# # # install_github("ramnathv/rCharts")
# # # # library("ggvis")
# # # # library("googleVis")
# # # library("rCharts")
# # rm(rat)
# # scatter.ggplot <- ggplot(a, aes(x = mean))
# # scatter.ggplot
# # 
# # 
# # if (AdjCos.similarity[i,j] >= quantile(AdjCos.similarity[,j],0.95,na.rm = T)) {
# #   pred[i,j] <- AdjCos.similarity[i,j]
# # }
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # pred <- matrix(NA, nrow=11, ncol=ncol(AdjCos.similarity))
# # 
# # for (i in 1:ncol(AdjCos.similarity)) {
# #   pred[,i] <- AdjCos.similarity[,i][AdjCos.similarity[,i] > quantile(AdjCos.similarity[,i],0.95,na.rm = T)]
# #   names(pred[,i]) <- names(AdjCos.similarity[,i][AdjCos.similarity[,i] > quantile(AdjCos.similarity[,i],0.95,na.rm = T)])
# # }
# # View(pred)
# # dimnames(pred)[[2]] <- names()
# # names(pred[,5]) <- names(AdjCos.similarity[,5][AdjCos.similarity[,5] > quantile(AdjCos.similarity[,5],0.95,na.rm = T)])
# # pred[,164]
# # 
# # df <- as.data.frame(AdjCos.similarity)
# # 
# # for (i in ncol(AdjCos.similarity)) {
# #   df$i <-
# #     > quantile(df$i,0.75,na.rm = T)]
# # }
# # View(df)
# # a[a > quantile(a,0.95,na.rm = T)]
# # library(magrittr)
# # 
# # AdjCos.similarity[,AdjCos.similarity[,3] > quantile(AdjCos.similarity[,3],0.99,na.rm = T)]
# # 
# # sim <- AdjCos.similarity
# # 
# # a <- c(0,2,324,512,3,24,1,43,234,2,55,23,3,5,2,5,2,56,6,34)
# # quantile(a,0.75)
# # a[a < quantile(a,0.75)]
# # d[d$Point < quantile(d$Point, 0.95), ]
# # 
# # 
# # rm(i,j)
# # 
# # AdjCos.similarity[4,4] >= quantile(AdjCos.similarity[,4],0.95,na.rm = T)
