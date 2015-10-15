

source("loading data.R")
source("demography.R")
source("skills.R")
source("kmeans-rattle.R")
source("tools.R")
source("pay.R")
source("tool correlations.R")
source("tools pay.R")
source("silhouette on cluster.R")

source("inference.R")
source("ordinal regression.R")
source("regression-test.R")

##---------------------------------------------------------------------


## deleting junk data


# df <- df[!(id %in% c(15,22,27)),]
# 
# 
# ## 15 moshavere IT sd= 36.47 
# ## 22 کارشناس مدیریت دانش  sd=38.53
# ## 27 سرپرست حسابرسی داخلی  sd=28.4
# 
# ## deleting rows with skill standard deviation less than 10 
# df <- df[which(df$skills_sd > 10),]



##---------------------------------------------------------------------
##rattle_df <- df[,c(1,16:37)]
##write.csv(rattle_df,"rattle.csv")
