##df_backup <- read.csv("Data Science 2015-05-17.csv",stringsAsFactors=FALSE)
##df <- read.csv("Data Science 2015-06-11.csv",stringsAsFactors=FALSE,nrows=45)
##df <- read.csv("Data Science 2015-06-20.csv",stringsAsFactors=FALSE)


df <- read.csv("Data Science 2015-06-27.csv",stringsAsFactors=FALSE)
## used for article submission n=75


names(df) <- c("time","title", "level","degree", "major","gender","province","age","personnel","establish",
               "industry","ownership","nationality","activity","algorithm","backend","baysian","bigdata",
               "business","stat","manipulation","frontend","vis","ml","math","optimization","product","science","simulation",
               "spatial","strdata","marketing","admin","temporal","unstrdata","graphical","os","modeling","reporting","relational",
               "dataman","hadoop","base","gross","resume","comment","email")

id <- rownames(df)
df <- cbind(id=id, df)
## converting factor to numeric
id <- as.numeric(id)

##str(df)
##head(df[,16:37])

