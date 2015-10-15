require(cluster,quietly=TRUE)

##----------------------------
## example

# data(ruspini)
# pr4 <- pam(ruspini, 4)
# str(si <- silhouette(pr4))
# (ssi <- summary(si))
# plot(si) # silhouette plot
# plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring


##-----------------------------
clust3 <-  pam(df[,16:37], 3)
str(si <- silhouette(clust3))
(ssi <- summary(si))
##plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue"))# with cluster-wise coloring

# Silhouette of 75 units in 3 clusters from pam(x = df[, 16:37], k = 3) :
#   Cluster sizes and average silhouette widths:
#   27           33           15 
# 0.304813427 -0.003011358  0.267987891 
# Individual silhouette widths:
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.24620  0.02981  0.11820  0.16200  0.33120  0.49060 


