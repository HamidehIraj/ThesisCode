##cleaning base pay and gross pay

df[which(df$gross< df$base),]

which(df$gross <  df$base) ##id=23
##df[20,44:45]

## update id=23 set gross=base
#df[20,45] <- df[20,44] 
##---------------------------------------
##converting basepay and grosspay to the mean number in the range

dfbackup <- df
## creating new variables for base-numeric and gross-numeric
df$base_num  <- 0
df$gross_num <- 0

for (payrow in (1:nrow(df)))

  { if (df[payrow,"base"]=="0-1,150,000")           df[payrow,"base_num"]=mean(c(0,1150000))         else
    if (df[payrow,"base"]=="1,150,001-2,300,000")   df[payrow,"base_num"]=mean(c(1150000,2300000 ))  else 
    if (df[payrow,"base"]=="2,300,001-3,450,000")   df[payrow,"base_num"]=mean(c(2300000,3450000 ))  else 
    if (df[payrow,"base"]=="3,450,001-4,600,000")   df[payrow,"base_num"]=mean(c(3450000,4600000 ))  else
    if (df[payrow,"base"]=="4,600,001-5,750,000")   df[payrow,"base_num"]=mean(c(4600000,5750000 ))  else 
    if (df[payrow,"base"]=="5,750,001-6,900,000")   df[payrow,"base_num"]=mean(c(5750000,6900000 ))  else 
    if (df[payrow,"base"]=="6,900,001-8,050,000")   df[payrow,"base_num"]=mean(c(6900000,8050000 ))  else 
    if (df[payrow,"base"]=="8,050,001-9,200,000")   df[payrow,"base_num"]=mean(c(8050000,9200000 ))  else 
    if (df[payrow,"base"]=="9,200,001-10,350,000")  df[payrow,"base_num"]=mean(c(9200000,10350000))  else 
                                                    df[payrow,"base_num"]=10350000
    
    if (df[payrow,"gross"]=="0-1,150,000")           df[payrow,"gross_num"]=mean(c(0,1150000))         else
    if (df[payrow,"gross"]=="1,150,001-2,300,000")   df[payrow,"gross_num"]=mean(c(1150000,2300000 ))  else 
    if (df[payrow,"gross"]=="2,300,001-3,450,000")   df[payrow,"gross_num"]=mean(c(2300000,3450000 ))  else 
    if (df[payrow,"gross"]=="3,450,001-4,600,000")   df[payrow,"gross_num"]=mean(c(3450000,4600000 ))  else
    if (df[payrow,"gross"]=="4,600,001-5,750,000")   df[payrow,"gross_num"]=mean(c(4600000,5750000 ))  else 
    if (df[payrow,"gross"]=="5,750,001-6,900,000")   df[payrow,"gross_num"]=mean(c(5750000,6900000 ))  else 
    if (df[payrow,"gross"]=="6,900,001-8,050,000")   df[payrow,"gross_num"]=mean(c(6900000,8050000 ))  else 
    if (df[payrow,"gross"]=="8,050,001-9,200,000")   df[payrow,"gross_num"]=mean(c(8050000,9200000 ))  else 
    if (df[payrow,"gross"]=="9,200,001-10,350,000")  df[payrow,"gross_num"]=mean(c(9200000,10350000))  else 
                                                     df[payrow,"gross_num"]=10350000  
}

## converting text to numeric for base and gross variables

df[,"base_num"]   <- as.numeric(df[,"base_num"])
df[,"gross_num"]  <- as.numeric(df[,"gross_num"])

##-----------------------------------------------
##working on base itself

class(df$base)
summary(df$base_num)
levels(df$base)
sort(unique(df$base_num))



# levels(df$base)<- sort(levels(df$base))
# levels(df$base)<- levels(df$base)[c(1,2,4:8,3)]
##------------------------------------------
df$base <- as.character(df$base)

##------------------------------------------
##two levels


# df[which(df$base_num>2300000),"base"] <- "2,300,001 and more"
# df[which(df$base_num<=2300000),"base"] <- "0-2,300,000"
# 
# df$base <- factor(df$base,levels=c("0-2,300,000","2,300,001 and more"))
# 
# length(which(df$base_num<=2300000))
# length(which(df$base_num>2300000))

##------------------------------------------
##three levels
# df[which(df$base_num <  1150000),"base"]  <- "0-1,150,000"
# df[which(df$base_num <  2300000),"base"]  <- "1,150,001-2,300,000"
# df[which(df$base_num >= 2300000),"base"]  <- "2,300,001 and more"
# 
# df$base <- factor(df$base,levels=c("0-1,150,000", "1,150,001-2,300,000","2,300,001 and more"))


