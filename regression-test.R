##names (df)

demography_index <- names(df)[c(4,5,7,10:11,13:15,49:52)] 
skills_index     <- names(df)[16:37]
tools_index      <- names(df)[54:77] 
outcome_index    <- c("base_num")



lm_degree_var <- c(demography_index,skills_index,tools_index,outcome_index)


lm_degree <- df[,var_reg]
lm_degree <- lm(base_num ~ degree,data=lm_degree)

summary(lm_degree)
##----------------------
## degree

ord_degree_index  <- c("base")
ord_degree_var    <- c(demography_index,skills_index,tools_index,ord_degree_index)
ord_degree        <- df[,ord_degree_var ]

class(ord_degree$base)
unique(ord_degree$base)
plot(base ~ degree,data=ord_degree,col=rainbow(3) )

require(MASS,quietly=TRUE)
ord_degree_model <- polr(base ~ degree, data = ord_degree, method="logistic")
summary(ord_degree_model)
##-------------------
## level

ord_level_index  <- c("base")
ord_level_var    <- c(demography_index,skills_index,tools_index,ord_level_index)
ord_level_df        <- df[,ord_level_var ]

class(ord_level_df $base)
unique(ord_level_df$base)
nlevels(ord_level_df$base)

plot(base ~ level,ord_level_df ,col=rainbow(3) )

require(MASS,quietly=TRUE)
ord_degree_model <- polr(base ~ degree, data = ord_degree, method="logistic")
summary(ord_degree_model)
##-----------------------
plot(base , gender,data=ord_level_df ,col=gender )
plot(ord_level_df$base , ord_level_df$gender,col=c("red","green") )



plot(base ~ major_stat,ord_level_df ,col=major_stat)
plot(base ~ major_computer,ord_level_df ,col=rainbow(3) )
plot(major_computer ~ base,ord_level_df ,col=base )
legend("topright"legend = c(" yes", "no"))
