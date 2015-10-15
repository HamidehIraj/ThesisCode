require(foreign,    quietly =TRUE)
require(ggplot2,    quietly =TRUE)
require(MASS,       quietly =TRUE)       ## function polr
require(splines,    quietly =TRUE)
require(Hmisc,      quietly =TRUE)
require(reshape2,   quietly =TRUE)


##names (df)

demography_index <- names(df)[c(4,5,7,10:11,13:15,49:52)] 
skills_index     <- names(df)[16:37]
tools_index      <- names(df)[54:77] 
##additional_index <- c("degree2")
outcome_index    <- c("base")
  


var_reg <- c(demography_index , skills_index , tools_index , outcome_index)
##additional_index
##var_reg <- c(demography_index,tools_index,outcome_index)
##var_reg <- c(tools_index,outcome_index)
##var_reg <- c(demography_index,outcome_index)


df_reg <- df[,var_reg ]

#str(df_reg)
#unique(df_reg$base)

# for (regrow in demography_index)
# 
#   {df_reg[,regrow] <- as.factor(df_reg[,regrow])}

##str(df_reg)

##------------------------------------------------------------------
## building the model
##m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
# 
# require(MASS,       quietly =TRUE) 
# ord_reg <- polr(base ~ ., data = df_reg, method="logistic")
# 
# ,Hess=TRUE)
# ##response must have 3 or more levels
# 
# 
# summary(ord_reg)[1]
# 
# ##---------------------------------
# ## interpreting results
# 
# 
# ord_reg$coefficients
# ord_reg$zeta
# ord_reg$lev
# ord_reg$method
# ord_reg$terms
# ord_reg$tvalue
# 
# pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 
# 
# pr <- profile(ord_reg)
# plot(pr)
# pairs(pr)

##----------------------------------------------------------------
require(ordinal,quietly=TRUE)

##fm1 <- clm(base ~ . , data = df_reg)
##accepts two levels of response variable


# 
# fm2 <- clm(base ~level+degree+backend+rapid, data=df_reg)  ## no p-value
# 
# 
# fm3 <- clm(base ~ level+backend+rapid, data=df_reg)     ## gave p-value
# fm4 <- clm(base ~ level+degree,data=df_reg)             ## no p-value
# fm5 <- clm(base ~ level+degree2,data=df)                ## gave p-value
# fm6 <- clm(base ~ level+degree2+level*degree2,data=df)  ## gave p-value
# 
# summary(fm6)
##------------------------------
df$degree2 <- NA

for (i in 1:nrow(df))
 {if (df[i,"degree"] %in% c("bachelor","master")   ) df[i,"degree2"] <- "non-phd" else df[i,"degree2"] <- "phd"}
  
##------------------------------

## 
table1 <- table(df$skills_cluster , df$base)
fm1    <- clm  (base ~ skills_cluster, data=df)


addmargins(table1)
summary(fm1)

##odds ratios
round(exp(fm1$beta), 1)

##--------------
table2 <- table(df$klar_cluster , df$base)
addmargins(table2)

fm2    <- clm  (base ~ klar_cluster, data=df)  
## 5 clusters- no significant variable
## 4 clusters- no significant variable
## 3 clusters- no significant variable
## 2 clusters- no significant variable

summary(fm2)


##----------------------------------
table3 <- table(df$degree , df$base)
addmargins(table3)
rm(table3_df) <- as.data.frame(table3)

##-----------------------------------------------
##simple regression

# lm_all <- lm(base_num ~ . , data=df_reg)
# summary(lm_all)
# 
# lm_step <- step(lm_all)
# 
# lm_step_final <- lm(base_num ~ level + degree + gender + personnel + establish + 
#   ownership + nationality + activity + major_man + major_science + 
#   major_computer + major_stat + algorithm + backend + baysian + 
#   bigdata + business + stat + manipulation + frontend + vis + 
#   ml + math + optimization + product + science + simulation + 
#   spatial + strdata + marketing + admin + temporal + unstrdata + 
#   graphical + open_modeling + com_modeling + total_modeling + 
#   python + r + weka + java + javascript + mahout + scala + 
#   perl + spss + ssas ,data=df_reg)
# 
# summary(lm_step_final)
