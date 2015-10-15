##------------------
##implementing kmeans on rattle

set.seed(42)

# The 'reshape' package provides the 'rescaler' function.

require(reshape, quietly=TRUE)

# The 'fpc' package provides the 'kmeansruns' function.

require(fpc, quietly=TRUE)

# Generate a kmeans cluster of size 3 choosing the best from 100.
# numeric <- c("algorithm", "backend", "baysian", "bigdata",
#                  "business", "stat", "manipulation", "frontend",
#                  "vis", "ml", "math", "optimization",
#                  "product", "science", "simulation", "spatial",
#                  "strdata", "marketing", "admin", "temporal",
#                  "unstrdata", "graphical")

fpc.kmeans <- kmeansruns(sapply(na.omit(df[,16:37]), rescaler, "range"), 3, runs=100)

# Cluster sizes:

paste(fpc.kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(df[,16:37]), rescaler, "range"))

# Cluster centers:

fpc.kmeans$centers

fpc <- as.data.frame(fpc.kmeans$centers)
# Within cluster sum of squares:

fpc.kmeans$cluster
fpc.kmeans$withinss
fpc.kmeans$crit

fpc.kmeans$bestk

# objects
# cluster A vector of integers indicating the cluster to which each point is allocated.
# centers A matrix of cluster centers.
# withinss The within-cluster sum of squares for each cluster.
# size The number of points in each cluster.
# bestk The optimal number of clusters.
# crit Vector with values of the criterion for all used numbers of clusters (0 if number not tried).


##------------------------------------------------------

df$skills_cluster <- fpc.kmeans$cluster

df$skills_cluster <- as.factor(df$skills_cluster)
##class(df$skills_cluster)


df.dev  <- subset(df,skills_cluster==1)
df.low  <- subset(df,skills_cluster==2)
df.high <- subset(df,skills_cluster==3)

df.high$skills_sd

summary(df.dev$skills_sd)
summary(df.low$skills_sd)
summary(df.high$skills_sd)

##--------------------------------------------------------

boxplot(skills_sd ~ as.factor(skills_cluster), data=df,varwidth=TRUE ,
        names =c("Software Developers","High Level Data Scientists","Low Level Data Scientists"),col=c("red","blue","darkgreen")
        ,xlab="Clusters",ylab="Standard Deviation")  

# boxplot(skills_sd ~ as.factor(skills_cluster), data=df,varwidth=TRUE ,
#         names =c("برنامه نویس ها","متخصصین علم داده سطح بالا","متخصصین علم داده سطح پایین"),col=c("red","blue","darkgreen")
#         ,xlab="خوشه ها",ylab="انحراف معیار") 

# require(extrafont,quietly=TRUE)
# font_import()
# 
# loadfonts(device = "win")
# plot()



testfpc <- df[,16:37]