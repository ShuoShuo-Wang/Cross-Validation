library(tidyverse)
library(broom)
library(data.table)
options(scipen =10)
# 1. seed (vector) random number generator seed
seed <-  436604030

data(airquality)
df <- airquality
df$Day <- NULL
df <- na.omit(df)
df$Month <- as.factor(df$Month)
rownames(df) <- seq_len(nrow(df))

# 2. raw (data.frame)
raw <- df
#check
all.equal(raw,df)

# 3. n (vector) number of observations in “raw”
n <- nrow(raw)

# 4. b (vector) number of bootstrap iterations
b <- 500

#### Classic Bootstrapping ####
set.seed(seed)

# shuffled row numbers for each experiment
t_classic  <-  replicate(b, sample(n,replace = T),simplify = F)

# Generate Empty Container
m_cl <- rep(list(NA), 500)

for (j in 1:500) {
  nw_df <- raw[t_classic[[j]],]
  m_cl[[j]] <- tidy(lm(Ozone ~ . ,data = nw_df))
}

#Empty df for the result 
df_cl <- data.frame(va=numeric(), vb=numeric(), vc=numeric(),vd=numeric(),ve=numeric())

for (index in 1:500) {
  df_cl <- as.data.table(rbind(df_cl,m_cl[[index]]))
  index <- index + 1
}
# 5. cl.loop (data.table) looped “classic” bootstrap result
cl.loop <-  as.data.table(df_cl)

#### PartB.Q1 - Q7 Code #### 
#PartB.Q1-Q3,Q7 values for the Wind estimate
cla_cal1 <- cl.loop %>% 
  filter(term == "Wind") %>% 
  select(estimate) 

mean(cla_cal1$estimate) # Q1. Average value of Wind estimate
mean(cla_cal1$estimate) + 1.96*sd(cla_cal1$estimate) # Q3. Parametric w/ classic 
quantile(cla_cal1$estimate, 0.025) # Q2. Non- parametric w/ classic

# Q7. Proportion of p.values < alpha=0.05 for the Wind estimate w/ Classic Bootstrap
cla_cal1_1 <- cl.loop %>% 
  filter(term == "Wind") %>% 
  select(p.value)  %>% 
  filter(p.value < 0.05)

cla_cal1_1 %>% 
  summarize(prop = nrow(.)/dim(cla_cal1)[1])

#PartB.Q4-Q6 values for the Temp estimate#
cla_cal2 <- cl.loop %>% 
  filter(term == "Temp") %>% 
  select(estimate) 

sd(cla_cal2$estimate) # Q4. Average value of Temp estimate
mean(cla_cal2$estimate) - 1.96*sd(cla_cal2$estimate) # Q6. Parametric w/ classic
quantile(cla_cal2$estimate, 0.975) # Q5. Non- parametric w/ classic

#### DT method for Classical Bootstrapping ####
set.seed(seed)

dt_classic  <-  replicate(b, sample(n,replace = T),simplify = T)

g_cl <-  rep(1:b, each=n)

##Data set preparation
D <- data.frame(va=numeric(), vb=numeric(), vc=numeric(),vd=numeric(),ve=numeric())
for (s in 1:500) {
  D <- as.data.table(rbind(D,raw[dt_classic[,s],]))
} 

rownames(D) <- seq_len(nrow(D))
D$grp <- g_cl

# 6. cl.dt (data.table) DT “classic” bootstrap result
cl.dt <- D[,tidy(lm(Ozone ~ Solar.R + Wind + Temp + Month)),by = grp]


#### Balanced Bootstrapping ####
set.seed(seed)

#draw b=500 samples, each sample with n=111 obversvation(rows) 
t_balanced  <-  sample(rep(1:n,b))

# check with results
table(t_balanced)

g_bl <- rep(1:b, 
            each=n)

# Generate Empty Container  
m_bl <- rep(list(NA), max(g_bl)) #max(g_bl)=500

for (j in 1:max(g_bl)) {
  k    <- t_balanced[ g_bl == j ]     # index values for sample
  m_bl[[j]] <- tidy(lm(Ozone ~ . ,data = raw[k,]))
}

df_bal <- data.frame(va=numeric(), vb=numeric(), vc=numeric(),vd=numeric(),ve=numeric())
# 7. bal.loop (data.table) looped “balanced” bootstrap result; see Notes
for (idx in 1:500) {
  df_bal <- as.data.table(rbind(df_bal,m_bl[[idx]]))
  idx <- idx + 1
}
bal.loop <-  as.data.table(df_bal)

#### PartB.Q8 - Q14 Code #### 
#PartB.Q8-Q10, values for the Wind estimate
b_cal1 <- bal.loop %>% 
  filter(term == "Wind") %>% 
  select(estimate) 

sd(b_cal1$estimate) #Q8. sd of Wind estimate w/ Balanced bootstrap
mean(b_cal1$estimate) - 1.96*sd(b_cal1$estimate) # Q10. Parametric w/ Balanced
quantile(b_cal1$estimate, 0.975) # Q9. Non-parametric w/ Balanced

#PartB.Q11-Q14, values for the Temp estimate#
b_cal2 <- bal.loop %>% 
  filter(term == "Temp") %>% 
  select(estimate) 

mean(b_cal2$estimate) #Q11. Average value of Temp estimate w/ Balanced
mean(b_cal2$estimate) + 1.96*sd(b_cal2$estimate)  #Q13. Parametric w/ Balanced
quantile(b_cal2$estimate, 0.025) # Q12. Non-parametric w/ Balanced

#Q14. Proportion of p.values < alpha=0.05 for the Temp estimate w/ Classic Bootstrap
b_cal2_1 <- bal.loop %>% 
  filter(term == "Temp") %>% 
  select(p.value)  %>% 
  filter(p.value < 0.05) 

b_cal2_1 %>% 
  summarize(prop = nrow(.)/dim(b_cal2_1)[1])

#### DT method for Balanced Bootstrapping ####
set.seed(seed)

dt_balance  <-  sample(rep(1:n,b))

g_cl <-  rep(1:b, each=n)

i_g <- data.table(idx = dt_balance, grp = g_cl) 

n_raw <- data.table(cbind(raw[dt_balance,],idx = dt_balance, grp = g_cl))

# 8. bal.dt (data.table) DT “balanced” bootstrap result;
bal.dt <- n_raw[, tidy(lm(Ozone ~ Solar.R + Wind + Temp + Month)), by=grp] 

