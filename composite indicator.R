### In this code, we try a few dimension reduction method and
### employ ridge regression in the final.
### In addition, the code generate data with housing condition index only to build 
### predictive model.

### Headlines
# Try PCA, MCA, Penalized LM, Single index model
# Employ ridge regression and construct the housing index with standardization

library(readr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(glmnet)
library(corrplot)
library(dr)

### Data preparation
data <- read_csv("nycRent.csv")
rent=data$Rent
df=data[,13:64]
df=as.matrix(df)

hist(as.numeric(apply(df!=0,2,sum)/104565),breaks=10)
geom_histogram(as.numeric(apply(df!=0,2,sum)/104565),breaks=10)

d1 <- data.frame(as.numeric(apply(df!=0,2,sum)/104565))
colnames(d1) <- 'frequency'
ggplot(d1,aes(x=frequency))+geom_histogram(binwidth = 0.05)


### PCA
pca_result <- prcomp(df, scale = TRUE)
summary(pca_result)

0.1081### MCA
dffac = factor(df[,1], ordered = TRUE)
for (i in 1:52){
  tfac = factor(df[,i], ordered = TRUE)
  dffac = data.frame(dffac, tfac)
}
colnames(dffac)=labels(df[1,])
dffac[,53] <- NULL
mca <- MCA(dffac, ncp = 1)
summary(mca)

### SIR
#dr_sir <- dr(rent~ df, method="sir", nslices=1000)
#summary(dr_sir)


## Ridge Regression
fit1 <- cv.glmnet(as.matrix(df), rent, family = 'gaussian', nfold = 10, alpha = 0)
plot(fit1)
ridge <- glmnet(as.matrix(df),rent,family='gaussian',alpha=0,lambda = exp(10))
ridge_weight <- ridge$beta
data$index_ridge <- as.matrix(data[,13:64]) %*% as.matrix(ridge_weight)
data$index <- (data$index_ridge+250)/500
data$condition_rating <- ceiling(data$index*20)-5
data$condition_rating[data$condition_rating<0]=0
data$condition_rating[data$condition_rating>10]=10
data$index_ridge <- NULL
data[,13:64] <- NULL

write.csv(data,file="new_data.csv",row.names = F)

# Visulization
colnames(df) <- 1:52
corrplot(cor(df))

## Statistical test for the rent difference regarding immgrant status
im1 <- data$index[data$is_im1==1]
im2 <- data$index[data$is_im2==1]
im0 <- data$index[(data$is_im1+data$is_im2)==0]
t.test(im1,im2)
t.test(c(im1,im2),im0)

im1_rent <- data$Rent[data$is_im1==1]
im2_rent <- data$Rent[data$is_im2==1]
im0_rent <- data$Rent[(data$is_im1+data$is_im2)==0]
t.test(im1_rent,im2_rent)
t.test(c(im1_rent,im2_rent),im0_rent)

