modeling_soft <- sort(c("python","r","weka","java","javascript","mahout","scala","perl","spss","ssas",
                   "rapid","ibm","tsql","vba","oracle","c","cplus","csharp", "matlab","bash","stata" ))

eda <- df[,c(modeling_soft,"base_num","gross_num")]

##--------------------------------
##reshaping the dataset

require(reshape,quietly = TRUE)
# use melt to convert from wide to long format 
eda_melt_all = melt(eda,id.vars=c("base_num","gross_num"))
names(eda_melt_all)[3] = "software" 

# subset to only select where value is "yes"
eda_melt = subset(eda_melt_all, value == 'yes')

# replace column name from "variable" to "software" 
names(eda_melt)[3] = "software" 

levels(eda_melt$software) <- rev(levels(eda_melt$software))

##----------------------------------
## viz  ok 
require(ggplot2,quietly = TRUE)


p1 <- ggplot(eda_melt, aes(factor(software))) + geom_bar() + coord_flip()
p2 <- ggplot(eda_melt, aes(factor(software),  base_num)) + geom_boxplot() + coord_flip()
p3 <- ggplot(eda_melt, aes(factor(software), gross_num)) + geom_boxplot() + coord_flip()

require('gridExtra',quietly = TRUE)
grid.arrange(p1,p2,p3,nrow=1)

##------------------------------


p1 <- ggplot(eda_melt, aes(factor(software))) + geom_bar() + coord_flip() + labs(x="modeling software","usage ratio") + aes(fill=software) + guides(fill=FALSE)

p2 <- ggplot(eda_melt, aes(factor(software),  base_num)) + geom_boxplot() + coord_flip() +labs(x="",y="base pay") + aes(fill=software) + guides(fill=FALSE)

p3 <- ggplot(eda_melt, aes(factor(software), gross_num)) + geom_boxplot() + coord_flip() +labs(x="",y="gross pay") + aes(fill=software) + guides(fill=FALSE)

require('gridExtra',quietly = TRUE) 
grid.arrange(p1,p2,p3,nrow=1)

##------------------------------
##viz for chi-squae test
#modeling_soft
# [1] "bash"       "c"          "cplus"      "csharp"     "ibm"        "java"       "javascript" "mahout"    
# [9] "matlab"     "oracle"     "perl"       "python"     "r"          "rapid"      "scala"      "spss"      
# [17] "ssas"       "stata"      "tsql"       "vba"        "weka"  

##removing outliers
##df <- df[which(df$base_num < 9775000),]


plot01  <- ggplot(df, aes(factor(bash),base_num))         + geom_boxplot(aes(fill=factor(bash))) + coord_flip()+ labs(x="bash",y=element_blank()) 
plot02  <- ggplot(df, aes(factor(c),base_num))            + geom_boxplot(aes(fill=factor(c))) + coord_flip()+ labs(x="c",y=element_blank()) 
plot03  <- ggplot(df, aes(factor(cplus),base_num))        + geom_boxplot(aes(fill=factor(cplus))) + coord_flip()+ labs(x="c++",y=element_blank()) 
plot04  <- ggplot(df, aes(factor(csharp),base_num))       + geom_boxplot(aes(fill=factor(csharp))) + coord_flip()+ labs(x="c#",y=element_blank()) 
plot05  <- ggplot(df, aes(factor(ibm),base_num))          + geom_boxplot(aes(fill=factor(ibm))) + coord_flip()+ labs(x="ibm",y=element_blank()) 
plot06  <- ggplot(df, aes(factor(java),base_num))         + geom_boxplot(aes(fill=factor(java))) + coord_flip()+ labs(x="java",y=element_blank()) 
plot07  <- ggplot(df, aes(factor(javascript),base_num))   + geom_boxplot(aes(fill=factor(javascript))) + coord_flip()+ labs(x="javascript",y=element_blank()) 
plot08  <- ggplot(df, aes(factor(mahout),base_num))       + geom_boxplot(aes(fill=factor(mahout))) + coord_flip()+ labs(x="mahout",y=element_blank()) 
plot09  <- ggplot(df, aes(factor(matlab),base_num))       + geom_boxplot(aes(fill=factor(matlab))) + coord_flip()+ labs(x="matlab",y=element_blank()) 
plot10  <- ggplot(df, aes(factor(oracle),base_num))       + geom_boxplot(aes(fill=factor(oracle))) + coord_flip()+ labs(x="oracle",y=element_blank()) 
plot11  <- ggplot(df, aes(factor(perl),base_num))         + geom_boxplot(aes(fill=factor(perl))) + coord_flip()+ labs(x="perl",y=element_blank()) 
plot12  <- ggplot(df, aes(factor(python),base_num))       + geom_boxplot(aes(fill=factor(python))) + coord_flip()+ labs(x="python",y=element_blank()) 
plot13  <- ggplot(df, aes(factor(r),base_num))            + geom_boxplot(aes(fill=factor(r))) + coord_flip()+ labs(x="r",y=element_blank()) 
plot14  <- ggplot(df, aes(factor(rapid),base_num))        + geom_boxplot(aes(fill=factor(rapid))) + coord_flip()+ labs(x="rapid",y=element_blank()) 
plot15  <- ggplot(df, aes(factor(scala),base_num))        + geom_boxplot(aes(fill=factor(scala))) + coord_flip()+ labs(x="scala",y=element_blank()) 
plot16  <- ggplot(df, aes(factor(spss),base_num))         + geom_boxplot(aes(fill=factor(spss))) + coord_flip()+ labs(x="spss",y=element_blank()) 
plot17  <- ggplot(df, aes(factor(ssas),base_num))         + geom_boxplot(aes(fill=factor(ssas))) + coord_flip()+ labs(x="ssas",y=element_blank()) 
plot18  <- ggplot(df, aes(factor(stata),base_num))        + geom_boxplot(aes(fill=factor(stata))) + coord_flip()+ labs(x="stata",y=element_blank()) 
plot19  <- ggplot(df, aes(factor(tsql),base_num))         + geom_boxplot(aes(fill=factor(tsql))) + coord_flip()+ labs(x="tsql",y=element_blank()) 
plot20  <- ggplot(df, aes(factor(vba),base_num))          + geom_boxplot(aes(fill=factor(vba))) + coord_flip()+ labs(x="vba",y=element_blank()) 
plot21  <- ggplot(df, aes(factor(weka),base_num))         + geom_boxplot(aes(fill=factor(weka))) + coord_flip()+ labs(x="weka",y=element_blank()) 

require('gridExtra',quietly = TRUE)
grid.arrange(plot01,plot02,plot03,plot04,plot05,plot06,plot07,plot08,plot09,plot10,
             plot11,plot12,plot13,plot14,plot15,plot16,plot17,plot18,plot19,plot20,
             plot21,nrow=6,ncol=4)

##-------------------------------
eda_table <- table(eda_melt$software , eda_melt$value)
addmargins(eda_table)
##addmargins(table)["sql","no"]
prop.table(eda_table,margin=1)
##---------------------------------

eda_melt_all_table <- table(eda_melt_all$software , eda_melt_all$value)
eda_melt_all_table
addmargins(eda_melt_all_table)
prop.table(eda_melt_all_table,margin=1)


