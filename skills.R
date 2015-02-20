## Step 1: creating the data frame

df <- data.frame(user=character(),
                 stat=numeric(),
                 math=numeric(),
                 prog=numeric(),
                 ml=numeric(),
                 vis=numeric(),
                 domain=numeric(),
                 stringsAsFactors=FALSE)
##-----------------------
## filling user ids
for (i in 1:10)
{df[i,1] <- i }
##-----------------------
## filling data frame
df[1,c(2,3)]=80
df[2,2]=50 ; df[2,4]=70 ; df[2,5]=95
df[3,4]=45 ; df[3,6]=90
df[4,2]=75 ; df[4,5]=65 ; df[4,7]=85
df[5,4]=80 ; df[5,6]=70
df[6,3]=60 ; df[6,4]=60 ; df[6,5]=45
df[7,2]=20 ; df[7,3]=65 ; df[7,6]=35
df[8,4]=75 ; df[8,7]=25
df[9,2]=80 ; df[9,3]=60 ; df[9,4]=65
df[10,2]=45; df[10,7]=85

## turn null values into zero
for (i in 1:10){
  for (j in 1:7)
  { if (is.na(df[i,j]) ) df[i,j]<- 0 }
}
##-----------------------
## Step 2: Exploring and preparing the data ----

##-----------------------
## Step 3: Training a model on the data ----
## selecting variables to include in the model
skills <- df[,2:7]

## determining number of clusters
k <- 3

## Implementing the algorithm
skills_clusters <- kmeans(skills, k)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
skills_clusters$size

# look at the cluster centers
skills_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
df$cluster <- skills_clusters$cluster

