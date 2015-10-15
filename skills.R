
## Transforming text to numbers

for (row in (1:nrow(df)))
  for (col in (16:37))
  { if (df[row,col]=="0-9")    df[row,col]=5  else
    if (df[row,col]=="10-19")  df[row,col]=15 else 
    if (df[row,col]=="20-30")  df[row,col]=25 else 
    if (df[row,col]=="31-40")  df[row,col]=35 else
    if (df[row,col]=="41-50")  df[row,col]=45 else 
    if (df[row,col]=="51-60")  df[row,col]=55 else
    if (df[row,col]=="61-70")  df[row,col]=65 else 
    if (df[row,col]=="71-80")  df[row,col]=75 else 
    if (df[row,col]=="81-90")  df[row,col]=85 else 
    if (df[row,col]=="91-100") df[row,col]=95 
  }

names(df)[16:37]

## converting strings to numbers for further processing

for (var in (16:37))
  df[,var] <- as.numeric(df[,var])

##str(df)

##------------------------------------------------------------------

## caculating standard deviation of skills columns

## creating a new column for standard deviation of skills columns
df$skills_sd <- "NA"

## filling the column: creating standard deviation of skills
for (i in 1:nrow(df))
  df [i,"skills_sd"] <-  sd(df[i,16:37])

df[,"skills_sd"] <- as.numeric(df[,"skills_sd"])


