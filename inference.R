##inference

##--------------------------------------------------------------------------------------
##creating the table and structure

modeling_soft <- c("python","r","weka","java","javascript","mahout","scala","perl","spss","ssas",
                   "rapid","ibm","tsql","vba","oracle","c","cplus","csharp", "matlab","bash","stata" )

# [1] "bash"       "c"          "cplus"      "csharp"     "ibm"        "java"       "javascript" "mahout"     "matlab"    
# [10] "oracle"     "perl"       "python"     "r"          "rapid"      "scala"      "spss"       "ssas"       "stata"     
# [19] "tsql"       "vba"        "weka"

modeling_soft <- sort(modeling_soft)

s_table <- data.frame(toolname=character(),
                        base_method=character(), base_pvalue=numeric(0),
                        gross_method=character(),gross_pvalue=numeric(0)  ,stringsAsFactors = FALSE   )

id <- rownames(s_table)
s_table <- cbind(id=id, s_table)
## converting factor to numeric
s_table$id <- as.numeric(s_table$id )


str(s_table)

##------------------------------------------------------------------------------------
##Loading Functions

source("http://bit.ly/dasi_inference")

#Method should be 'theoretical' or 'simulation'
##----------------------------------------

##bash

inference(y = df$base, x = df$bash, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.1591, df = NA, p-value = 0.3428


inference(y = df$gross, x = df$bash, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.0657, df = NA, p-value = 0.6163


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="bash",
                               base_method="simulation",  base_pvalue=0.3428,  
                               gross_method="simulation", gross_pvalue=0.6163 )

##---------------------------------------------------------------------
##C

inference(y = df$base, x = df$c, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.0657, df = NA, p-value = 0.4871


inference(y = df$gross, x = df$c, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.1376, df = NA, p-value = 0.8007


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="c language",
                               base_method="simulation",  base_pvalue=0.4871,  
                               gross_method="simulation", gross_pvalue=0.8007 )
##------------------------------------------------------------------------
##C++

inference(y = df$base, x = df$cplus, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.1591, df = NA, p-value = 0.3343


inference(y = df$gross, x = df$cplus, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 6.2311, df = NA, p-value = 0.2802


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="c language",
                               base_method="simulation",  base_pvalue=0.4871,  
                               gross_method="simulation", gross_pvalue=0.8007 )
##--------------------------------------------------------------------------
##C#

inference(y = df$base, x = df$csharp, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 5.9991, df = NA, p-value = 0.5114


inference(y = df$gross, x = df$csharp, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.0186, df = NA, p-value = 0.7866


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="c sharp",
                               base_method="simulation",  base_pvalue=0.5114,  
                               gross_method="simulation", gross_pvalue=0.7866 )
##------------------------------------------------------------------------------
##ibm

inference(y = df$base, x = df$ibm, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 25.7197, df = NA, p-value = 0.007399


inference(y = df$gross, x = df$ibm, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 11.7992, df = NA, p-value = 0.05829


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="ibm",
                               base_method="simulation",  base_pvalue=0.007399,  
                               gross_method="simulation", gross_pvalue=0.05829 )
##------------------------------------------------------------------------------
##Java

inference(y = df$base, x = df$java, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 9.7692, df = NA, p-value = 0.1178


inference(y = df$gross, x = df$java, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 5.9217, df = NA, p-value = 0.3519


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="java language",
                               base_method="simulation",  base_pvalue=0.1178,  
                               gross_method="simulation", gross_pvalue=0.3519 )
##------------------------------------------------------------------------------
##Javascript

inference(y = df$base, x = df$javascript, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 5.0298, df = NA, p-value = 0.6878


inference(y = df$gross, x = df$javascript, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.9049, df = NA, p-value = 0.6515


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="java script",
                               base_method="simulation",  base_pvalue=0.6878,  
                               gross_method="simulation", gross_pvalue=0.6515 )

##---------------------------------------------------------------------------------
##mahout

inference(y = df$base, x = df$mahout, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.9755, df = NA, p-value = 0.5352


inference(y = df$gross, x = df$mahout, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 1.973, df = NA, p-value = 1


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="mahout",
                               base_method="simulation",  base_pvalue=0.5352,  
                               gross_method="simulation", gross_pvalue=1 )
##-------------------------------------------------------------------------------
##matlab

inference(y = df$base, x = df$matlab, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.9432, df = NA, p-value = 0.2839


inference(y = df$gross, x = df$matlab, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.9969, df = NA, p-value = 0.5773


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="matlab",
                               base_method="simulation",  base_pvalue=0.2839,  
                               gross_method="simulation", gross_pvalue=0.5773 )
##------------------------------------------------------------------------------
##oracle

inference(y = df$base, x = df$oracle, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 9.1858, df = NA, p-value = 0.151


inference(y = df$gross, x = df$oracle, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.4258, df = NA, p-value = 0.157


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="oracle",
                               base_method="simulation",  base_pvalue=0.151,  
                               gross_method="simulation", gross_pvalue=0.157 )
##--------------------------------------------------------------------------------
##perl

inference(y = df$base, x = df$perl, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 1.2224, df = NA, p-value = 1


inference(y = df$gross, x = df$perl, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 1.973, df = NA, p-value = 1


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="perl",
                               base_method="simulation",  base_pvalue=1,  
                               gross_method="simulation", gross_pvalue=1 )
##--------------------------------------------------------------------------------
##python

inference(y = df$base, x = df$python, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.9324, df = NA, p-value = 0.5811


inference(y = df$gross, x = df$python, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.0738, df = NA, p-value = 0.1779


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="python",
                               base_method="simulation",  base_pvalue=0.5811,  
                               gross_method="simulation", gross_pvalue=0.1779 )
##-------------------------------------------------------------------------------
## R

inference(y = df$base, x = df$r, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 12.474, df = NA, p-value = 0.0435


inference(y = df$gross, x = df$r, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 19.8958, df = NA, p-value = 0.0011


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="r language",
                               base_method="simulation",  base_pvalue=0.0435,  
                               gross_method="simulation", gross_pvalue=0.0011 )
##--------------------------------------------------------------------
## Rapid

inference(y = df$base, x = df$rapid, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 9.8194, df = NA, p-value = 0.1679


inference(y = df$gross, x = df$rapid, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.8306, df = NA, p-value = 0.6785


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="rapid",
                               base_method="simulation",  base_pvalue=0.1679,  
                               gross_method="simulation", gross_pvalue=0.6785 )
##-------------------------------------------------------------------
## scala

inference(y = df$base, x = df$scala, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 10.9804, df = NA, p-value = 0.2023


inference(y = df$gross, x = df$scala, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 7.9779, df = NA, p-value = 0.2349


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="scala",
                               base_method="simulation",  base_pvalue=0.2023,  
                               gross_method="simulation", gross_pvalue=0.2349 )
##---------------------------------------------------------------------
## spss

inference(y = df$base, x = df$spss, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 5.4861, df = NA, p-value = 0.5729


inference(y = df$gross, x = df$spss, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 8.0556, df = NA, p-value = 0.1554


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="spss",
                               base_method="simulation",  base_pvalue=0.5729,  
                               gross_method="simulation", gross_pvalue=0.1554 )
##-------------------------------------------------------------------------
## ssas

inference(y = df$base, x = df$ssas, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.4623, df = NA, p-value = 0.7549


inference(y = df$gross, x = df$ssas, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.2877, df = NA, p-value = 0.5521


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="ssas",
                               base_method="simulation",  base_pvalue=0.7549,  
                               gross_method="simulation", gross_pvalue=0.5521 )
##-------------------------------------------------------------------------
## stata

inference(y = df$base, x = df$stata, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##Error: Explanatory variable has only one level, should have at least two levels.


inference(y = df$gross, x = df$stata, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##Error: Explanatory variable has only one level, should have at least two levels.


# s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="stata",
#                                base_method="simulation",  base_pvalue=0.7549,  
#                                gross_method="simulation", gross_pvalue=0.5521 )
##-------------------------------------------------------------------------
## tsql

inference(y = df$base, x = df$tsql, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.5613, df = NA, p-value = 0.8748


inference(y = df$gross, x = df$tsql, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 9.923, df = NA, p-value = 0.06909


 s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="tsql",
                                base_method="simulation",  base_pvalue=0.8748,  
                                gross_method="simulation", gross_pvalue=0.06909 )
##_----------------------------------------------------------------------------
## vba
inference(y = df$base, x = df$vba, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 3.099, df = NA, p-value = 0.8421


inference(y = df$gross, x = df$vba, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 2.8125, df = NA, p-value = 0.869


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="vba",
                               base_method="simulation",  base_pvalue=0.8421,  
                               gross_method="simulation", gross_pvalue=0.869 )
##----------------------------------------------------------------------------
##weka

inference(y = df$base, x = df$weka, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 8.3191, df = NA, p-value = 0.2775


inference(y = df$gross, x = df$weka, est = "proportion", type = "ht", null = 0, 
          alternative = "twosided", method = "simulation")
##X-squared = 4.375, df = NA, p-value = 0.4298


s_table[nrow(s_table)+1,] <- c(id = nrow(s_table)+1, toolname="weka",
                               base_method="simulation",  base_pvalue=0.2775,  
                               gross_method="simulation", gross_pvalue=0.4298 )

