library(tidyverse)
library(data.table)

# Deliverables:1. seed (vector) random number generator seed
seed <- 436604030

data(airquality)
df <- airquality
df <- na.omit(df)
df <- df[, -6]
df$Month <- as.factor(df$Month)
rownames(df) <- seq_len(nrow(df))

# Deliverables:2. raw (data.table) see Notes
raw <- df

# Deliverables:3. base.model (lm) non-CV OLS model using raw (all observations)
base.model <- lm(Ozone ~ .,data = raw)
summary(base.model)

#α = 0.05, confidence interval = 1-0.05 = 95%
t1 <- base.model$residuals
Q1 <- mean(t1) # average residual reported from the baseline model
Q2 <- mean(t1) - 1.96 *sd(t1) #lower bound of the residuals from the baseline model
Q3 <- quantile(t1,0.975) #non-parametric estimate of the upper bound of base model

# Traning data set preparation 
set.seed(seed)
# Deliverables:4. tst (vector) 10% validation set index values
tst <- as.integer(sample(rownames(raw),ceiling(dim(raw)[1]*0.1)))
tst_df <-  df[tst,]

# Training data set preparation
row_train <- as.integer(setdiff(rownames(df),tst))
train_df <- df[row_train,]

#Double-check for traning & validating data set
rownames(tst_df)
rownames(train_df)

# Deliverables:5. cv.model (lm) OLS model using simple CV
cv.model <- lm(Ozone ~ .,data = train_df)

# Deliverables:6. cv.fitted (vector) tst fitted values from cv.model; see Notes
cv.fitted <- as.vector(predict(cv.model, tst_df))

loc <- as.integer(rownames(tst_df))
# Deliverables:7. cv.resid (data.table) tst residuals from simple CV; see Notes
cv.resid <-  data.table(loc,diff = tst_df$Ozone - cv.fitted)

#confidence interval = 1-0.05 = 95%
t2 <- cv.resid$diff
# average residual reported from the simple Cross-Validation model
Q4 <- mean(t2)
# parametric estimate of the lower bound of the residuals from the simple CV model
Q5 <- mean(t2) - 1.96 *sd(t2) 
# non-parametric estimate of the upper bound of the residuals from the simple CV model
Q6 <- quantile(t2,0.975) 

#### Leave-One-Out Cross-validation - LOOCV #### 

# copy to a new data set
df_LOOCV <- df

#add group number to the new data set 
df_LOOCV$group <- rep(1:nrow(df_LOOCV), 1)

# parameters
n <- nrow(df_LOOCV)

# Shuffle data  
# NO NEED TO SHUFFLE IN LOOCV algorithm, THINK ABOUT IT! 
#set.seed(seed)
#loocv_x <- sample(rep(1:n, length.out = n))

#pre-allocated data set for loop 
loop_loocv_model <- rep(list(NA),n)
loop_loocv_fitted <- rep(list(NA),n)
loop_loocv_resid <- rep(list(NA),n)

for (i in 1:n) {
  train_xy <- df_LOOCV[-i,]
  test_xy <- df_LOOCV[i,]
  loop_loocv_model[[i]] <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data = train_xy)
  loop_loocv_fitted[[i]] <- predict(loop_loocv_model[[i]],test_xy)
  loop_loocv_resid[[i]] <- test_xy$Ozone - loop_loocv_fitted[[i]]
}

resid_loocv_tr <- as.vector(unlist(loop_loocv_resid))

# Deliverables:8. jk.resid (data.table) validation residuals from LOOCV; see Notes
jk.resid <- data.table(loc = df_LOOCV$group,diff = resid_loocv_tr)

# confidence interval = 1-0.05 = 95%
t3 <- jk.resid$diff
# average residual reported from the Leave-One-Out Cross-Validation model
Q7 <- mean(t3)
# parametric estimate of the lower bound of the residuals from the LOOCV model
Q8 <- mean(t3) - 1.96 *sd(t3) 
# non-parametric estimate of the upper bound of the residuals from the  LOOCV model
Q9 <- quantile(t3,0.975) 

#### K-fold cross-validation ####
# Shuffle first, then add group number 

# Copy dt for looping
dt_for_loop <- raw

# Key Parameters
n <-  nrow(dt_for_loop)
k <- 10

set.seed(seed)
# Shuffle index number of data
#sample(rownames(raw),ceiling(dim(raw)[1]*0.1))
(folds_x <- as.integer(sample(rownames(dt_for_loop))))

#Generating group number for later use
(group_number <- rep(1:k,each= ceiling(n/k),length.out = n))

#new shuffled data set with group number
Ndt_for_loop <- dt_for_loop[folds_x,]
Ndt_for_loop$Grp <- group_number

#Empty Container Generate
loop_kf_model <- rep(list(NA),k)
loop_fitted <- rep(list(NA),k)
loop_resid <- rep(list(NA),k)

for (i in 1:k) {
  train_xy <-  Ndt_for_loop[Ndt_for_loop$Grp != i,]
  test_xy <- Ndt_for_loop[Ndt_for_loop$Grp == i,]
  loop_kf_model[[i]] <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data = train_xy[,1:5])
  loop_fitted[[i]] <- predict(loop_kf_model[[i]],test_xy[,1:5])
  loop_resid[[i]] <- test_xy$Ozone - loop_fitted[[i]]
}

resid_tr <- as.vector(unlist(loop_resid))

# Deliverables:9. kf.resid (data.table) validation residuals from K-fold; see Notes
kf.resid <- data.table(k = group_number,loc = folds_x,diff = resid_tr)

# Confidence interval = 1-0.05 = 95%
t4 <- kf.resid$diff
# Average residual reported from the K-fold Cross-Validation model
Q10 <- mean(t4)
# Parametric estimate of the lower bound of the residuals from the K-F CV model
Q11 <- mean(t4) - 1.96 *sd(t4) 
# Non-parametric estimate of the upper bound of the residuals from the K-F CV model
Ql2 <- quantile(t4, 0.975) 