
## converting to lower case

for (r in ( 1:nrow(df) )  )  {
df[r,"modeling"]   <- tolower(df[r,"modeling"])
df[r,"reporting"]  <- tolower(df[r,"reporting"])
df[r,"relational"] <- tolower(df[r,"relational"])
df[r,"dataman"]    <- tolower(df[r,"dataman"])
df[r,"hadoop"]     <- tolower(df[r,"hadoop"])
}


## creating variables for monitoring the number of tools
df$open_modeling   <- 0
df$com_modeling    <- 0
df$total_modeling  <- 0

##opensource
df$python     <- NA; df$r          <- NA; df$weka       <- NA; df$java       <- NA; df$javascript <- NA
df$weka       <- NA; df$mahout     <- NA; df$scala      <- NA; df$perl       <- NA

##commercial
df$spss       <- NA; df$ssas       <- NA; df$rapid      <- NA; df$ibm        <- NA;
df$tsql       <- NA; df$vba        <- NA; df$oracle     <- NA; df$c          <- NA;
df$cplus      <- NA; df$csharp     <- NA; df$matlab     <- NA; df$bash       <- NA;
df$stata      <- NA

for(trow in (1:nrow(df)))
{
  
##opensource
  
  if (length (grep(pattern="python",x=df[trow,"modeling"]))  > 0) 
  {df[trow,"python"] <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  
  if (length (grep(pattern="r language",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"r"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}

  if (length (grep(pattern="weka",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"weka"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}

  if (length (grep(pattern="java language",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"java"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}

  if (length (grep(pattern="java script",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"javascript"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  
  if (length (grep(pattern="mahout",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"mahout"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  
  if (length (grep(pattern="bash",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"bash"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  
  if (length (grep(pattern="scala",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"scala"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  
  if (length (grep(pattern="perl",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"perl"]      <- "yes" ;df[trow,"open_modeling"] <- df[trow,"open_modeling"]+1}
  ##commercial
  
  if (length (grep(pattern="spss/ clementine",     x=df[trow,"modeling"]))  > 0) 
    {df[trow,"spss"]   <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}
  
  if (length (grep(pattern="microsoft analysis services",     x=df[trow,"modeling"]))  > 0) 
    {df[trow,"ssas"]   <- "yes"  ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}
  
  if (length (grep(pattern="rapid miner",     x=df[trow,"modeling"]))  > 0) 
    {df[trow,"rapid"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="ibm big insights",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"ibm"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="tsql",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"tsql"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="visual basic/vba",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"vba"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="oracle bi",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"oracle"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="c language",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"c"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="c[+][+]",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"cplus"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="c#",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"csharp"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="matlab",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"matlab"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

  if (length (grep(pattern="stata",     x=df[trow,"modeling"]))  > 0) 
  {df[trow,"stata"]  <- "yes" ;df[trow,"com_modeling"] <- df[trow,"com_modeling"]+1}

}

##-----------------------------------------
## calculating total number of tools
library(stringr)

for (z in 1:nrow(df))

  {df[z,"total_modeling"] <-  str_count(df[z,"modeling"], ',') + 1}
##-----------------------------------------

df[,c("id","open_modeling","com_modeling","total_modeling")]

length(which(df$open_modeling + df$com_modeling == df$total_modeling))
length(which(df$open_modeling + df$com_modeling != df$total_modeling))

which(df$open_modeling + df$com_modeling != df$total_modeling)

##"modeling","open_modeling","com_modeling","total_modeling"

#df[which(df$open_modeling + df$com_modeling != df$total_modeling),"total_modeling"] <- 5

