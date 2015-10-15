
##Checking Correlations
#pairs(c(df$open_modeling,df$com_modeling,df$total_modeling,df$base,df$gross))

##base correlations
base_cor <- cor(df[,c("base_num","open_modeling","com_modeling","total_modeling")])
## ,method = c("pearson", "kendall", "spearman"))

base_cor

require("psych",quietly = TRUE) ##corr.test

base_corr <- corr.test (df[,c("base_num","open_modeling","com_modeling","total_modeling")],
                        method="pearson")

# Correlation matrix 
# base_num open_modeling com_modeling total_modeling
# base_num           1.00         -0.02         0.01           0.00
# open_modeling     -0.02          1.00         0.28           0.79
# com_modeling       0.01          0.28         1.00           0.79
# total_modeling     0.00          0.79         0.79           1.00
# Sample Size 
# [1] 33
# Probability values (Entries above the diagonal are adjusted for multiple tests.) 
# base_num open_modeling com_modeling total_modeling
# base_num           0.00          1.00         1.00              1
# open_modeling      0.89          0.00         0.48              0
# com_modeling       0.94          0.12         0.00              0
# total_modeling     0.98          0.00         0.00              0
##-----------------------------------------

##gross correlation
gross_cor <- cor(df[,c("gross_num","open_modeling","com_modeling","total_modeling")])
gross_cor


gross_corr <- corr.test (df[,c("gross_num","open_modeling","com_modeling","total_modeling")],
                         method="pearson")

# gross_num open_modeling com_modeling total_modeling
# gross_num           1.00          0.06         0.16           0.13
# open_modeling       0.06          1.00         0.28           0.79
# com_modeling        0.16          0.28         1.00           0.79
# total_modeling      0.13          0.79         0.79           1.00
# Sample Size 
# [1] 33
# Probability values (Entries above the diagonal are adjusted for multiple tests.) 
# gross_num open_modeling com_modeling total_modeling
# gross_num           0.00          1.00         1.00              1
# open_modeling       0.74          0.00         0.48              0
# com_modeling        0.38          0.12         0.00              0
# total_modeling      0.46          0.00         0.00              0
##-----------------------------------------------------------------------------
skills_names <- c("algorithm","backend","baysian","bigdata","business","stat","manipulation","frontend","vis","ml",            
                  "math","optimization","product","science","simulation","spatial","strdata","marketing","admin","temporal"      
                  ,"unstrdata","graphical")

skills_cor <- as.data.frame(cor(df[,c(skills_names,"base_num","gross_num")]  ))
#skills_cor 









## performing ch-squre test of independence

df$base  <- as.factor(df$base)
df$gross <- as.factor(df$gross)

modeling_soft <- c("python","r","weka","java","javascript","mahout","scala","perl","spss","ssas",
                   "rapid","ibm","tsql","vba","oracle","c","cplus","csharp", "matlab","bash","stata" )

for (na_row in (1:nrow(df)))
  for (na_col in modeling_soft)
  { if (is.na(df[na_row,na_col])) df[na_row,na_col]<- "no"}

#if (is.na(df[1,"python"])) df[1,"python"]<- "no"


for (fact_col in modeling_soft)
{ df[,fact_col] <- as.factor(df[,fact_col] )}

# str(df)
# names(df)

##-----------------------------------------------

