## Step 1: creating the data frame

toolsdf <- data.frame(user=character(),
                 r=numeric(),
                 python=numeric(),
                 sql=numeric(),
                 rapidminer=numeric(),
                 ssas=numeric(),
                 tableau=numeric(),
                 stringsAsFactors=FALSE)
##-----------------------
## filling user ids
for (i in 1:10)
{toolsdf[i,1] <- i }
##-----------------------
## filling data frame
toolsdf[1,c(2,3)]=80
toolsdf[2,2]=50 ; toolsdf[2,4]=70 ; toolsdf[2,5]=95
toolsdf[3,4]=45 ; toolsdf[3,6]=90
toolsdf[4,2]=75 ; toolsdf[4,5]=65 ; toolsdf[4,7]=85
toolsdf[5,4]=80 ; toolsdf[5,6]=70
toolsdf[6,3]=60 ; toolsdf[6,4]=60 ; toolsdf[6,5]=45
toolsdf[7,2]=20 ; toolsdf[7,3]=65 ; toolsdf[7,6]=35
toolsdf[8,4]=75 ; toolsdf[8,7]=25
toolsdf[9,2]=80 ; toolsdf[9,3]=60 ; toolsdf[9,4]=65
toolsdf[10,2]=45; toolsdf[10,7]=85

## turn null values into zero
for (i in 1:10){
  for (j in 1:7)
  { if (is.na(toolsdf[i,j]) ) toolsdf[i,j]<- 0 }
}
#============================================================

## Step 2: Exploring and preparing the data ----
#============================================================

## Step 3: Training a model on the data ----
## selecting variables to include in the model
tools <- toolsdf[,2:7]

## determining number of clusters
l <- 3

## Implementing the algorithm
tools_clusters <- kmeans(tools, l)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
tools_clusters$size

# look at the cluster centers
tools_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
toolsdf$cluster <- tools_clusters$cluster
tools$cluster   <- tools_clusters$cluster

#============================================================
#kmeans with 10 runs
set.seed(12345)

# The 'fpc' package provides the 'kmeansruns' function.

require(fpc, quietly=TRUE)

# Generate a kmeans cluster of size 3 choosing the best from 10.

tools_clusters10 <- kmeansruns(na.omit(tools), 3, runs=10)
#--------------------------------------------
# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(na.omit(tools))

# Cluster centers:

tools_clusters10$centers

# Within cluster sum of squares:

tools_clusters10$withinss


#============================================================
##Goodness of Fit Test

## aggregating based on clusters
## resource:http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
library(MASS)
ag <- aggregate(. ~ cluster,FUN=sum ,data=tools)

## show aggregation table
ag


chisq.test(ag) 
#============================================================
# Rattle 


# Build the training/validate/test toolsdfs.

##set.seed(crv$seed) 
nobs <- nrow(toolsdf) # 10 observations 
sample <- train <- sample(nrow(toolsdf), 0.7*nobs) # 7 observations
validate <- sample(setdiff(seq_len(nrow(toolsdf)), train), 0.1*nobs) # 1 observations
test <- setdiff(setdiff(seq_len(nrow(toolsdf)), train), validate) # 2 observations

# The following variable selections have been noted.

input <- c("X", "user", "r", "python","sql", "rapidminer", "ssas")

# Build the training/validate/test toolsdfs.

set.seed(crv$seed) 
nobs <- nrow(toolsdf) # 10 observations 
sample <- train <- sample(nrow(toolsdf), 0.7*nobs) # 7 observations
validate <- sample(setdiff(seq_len(nrow(toolsdf)), train), 0.1*nobs) # 1 observations
test <- setdiff(setdiff(seq_len(nrow(toolsdf)), train), validate) # 2 observations

# The following variable selections have been noted.

input <- c("r", "python", "sql", "rapidminer", "ssas", "tableau")


# The 'corrplot' package provides the 'corrplot' function.

require(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

cor <- cor(toolsdf[sample, 2:7], use="pairwise", method="pearson")

# Order the correlations by their strength.

ord <- order(cor[1,])
cor <- cor[ord, ord]

# Display the actual correlations.

print(cor)

# Graphically display the correlations.

corrplot(cor, mar=c(0,0,1,0))
title(main="Correlation toolsdf.csv using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

