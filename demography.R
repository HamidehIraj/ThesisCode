## Converting level to English characters

level_cat <- as.data.frame(unique(df$level))
names(level_cat)=c("level")
level_cat$level <- sort(level_cat$level)

# level
# 1	کارشناس
# 2	کارشناس ارشد / سرپرست
# 3	مدیر ارشد
# 4	مدیر فنی / مدیر پروژه

level_1 <- as.character(level_cat$level[1]) 
level_2 <- as.character(level_cat$level[2]) 
level_3 <- as.character(level_cat$level[3]) 
level_4 <- as.character(level_cat$level[4]) 

# levels(df$level)[levels(df$level)== level_1] <- "officer"
# levels(df$level)[levels(df$level)== level_2] <- "supervisor"
# levels(df$level)[levels(df$level)== level_3] <- "senior"
# levels(df$level)[levels(df$level)== level_4] <- "project/tech"

df[df$level==level_1,"level"] <-"officer"
df[df$level==level_2,"level"] <-"supervisor"
df[df$level==level_3,"level"] <-"senior"
df[df$level==level_4,"level"] <-"project/tech"


## correcting level orders
class(df$level)
df$level <- factor(df$level, levels=c("officer","supervisor","project/tech","senior"))

##----------------------------------------------
## Converting degree to English characters- from character

degree_cat <- as.data.frame(unique(as.factor(df$degree)))
names(degree_cat)=c("degree")
degree_cat$degree <- sort(degree_cat$degree)

# degree
# 1	دکترا
# 2	دیپلم
# 3	کارشناسی
# 4	کارشناسی ارشد

degree_1 <- as.character(degree_cat$degree[1]) 
degree_2 <- as.character(degree_cat$degree[2]) 
degree_3 <- as.character(degree_cat$degree[3]) 
degree_4 <- as.character(degree_cat$degree[4]) 


df[df$degree== degree_1,"degree"] <- "phd"
df[df$degree== degree_2,"degree"] <- "highschool"
df[df$degree== degree_3,"degree"] <- "bachelor"
df[df$degree== degree_4,"degree"] <- "master"



## correcting level orders
unique(df$degree)
df$degree <- factor(df$degree, levels=c("highschool","bachelor","master","phd"))
##---------------------------------------------

## Converting gender to English characters- from character

gender_cat <- as.data.frame(unique(as.factor(df$gender)))
names(gender_cat)=c("gender")
gender_cat$gender <- sort(gender_cat$gender) 

gender_1 <- as.character(gender_cat$gender[1]) 
gender_2 <- as.character(gender_cat$gender[2]) 

# gender
# 1	زن
# 2	مرد

df[df$gender== gender_1,"gender"] <- "female"
df[df$gender== gender_2,"gender"] <- "male"


## correcting level orders
unique(df$gender)
df$gender <- factor(df$gender, levels=c("female","male"))
##-------------------------------------------------
## Converting age to English characters- from character

# age_cat <- as.data.frame(unique(as.factor(df$age)))
# names(age_cat)=c("age")
# age_cat$age <- sort(age_cat$age)


for(agerow in (  1:nrow(df)  )   )
{

if (length (grep(pattern="61",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "61 and more"

else if (length (grep(pattern="60",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "56-60"

else if (length (grep(pattern="55",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "51-55"

else if (length (grep(pattern="50",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "46-50"

else if (length (grep(pattern="45",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "41-45"

else if (length (grep(pattern="40",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "36-40"

else if (length (grep(pattern="35",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "31-35"

else if (length (grep(pattern="30",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "26-30"

else if (length (grep(pattern="25",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "21-25"

else if (length (grep(pattern="21",x=df[agerow,"age"]))  > 0) 
  df[agerow,"age"] <- "0-21"   }

# age_1 <- as.character(age_cat$age[1]) ##"35 - 31"
# age_2 <- as.character(age_cat$age[2]) ##"40 -36"
# age_3 <- as.character(age_cat$age[3]) ##"30 -26"
# age_4 <- as.character(age_cat$age[4]) ## "25 -21"
# 
# df[df$age== age_1,"age"] <- "31-35"
# df[df$age== age_2,"age"] <- "36-40"
# df[df$age== age_3,"age"] <- "26-30"
# df[df$age== age_4,"age"] <- "21-25"

df$age <- factor(df$age, levels=c("0-21","21-25","26-30", "31-35","36-40","41-45","46-50","51-55","56-60","61 and more"))

unique(df$age)
class(df$age)
##---------------------------------------------------
personnel_cat <- as.data.frame(unique(as.factor(df$personnel)))
names(personnel_cat)=c("personnel")
personnel_cat$personnel <- sort(personnel_cat$personnel)

# personnel  covering all six values
# 1	1 نفر
# 2	1000 نفر و بیشتر
# 3	101 تا 500 نفر
# 4	2 تا 25 نفر
# 5	26 تا 100 نفر
# 6	501 تا 1000 نفر

personnel_1 <- as.character(personnel_cat$personnel[1]) 
personnel_2 <- as.character(personnel_cat$personnel[2]) 
personnel_3 <- as.character(personnel_cat$personnel[3]) 
personnel_4 <- as.character(personnel_cat$personnel[4]) 
personnel_5 <- as.character(personnel_cat$personnel[5]) 
personnel_6 <- as.character(personnel_cat$personnel[6]) 

df[df$personnel==personnel_1 ,"personnel"] <- "1"
df[df$personnel==personnel_2 ,"personnel"] <- "1001 and more"
df[df$personnel==personnel_3 ,"personnel"] <- "101-500"
df[df$personnel==personnel_4 ,"personnel"] <- "2-25"
df[df$personnel==personnel_5 ,"personnel"] <- "26-100"
df[df$personnel==personnel_6 ,"personnel"] <- "501-1000"

unique(df$personnel)
df$personnel <- factor(df$personnel, levels=c("1","2-25","26-100","101-500","501-1000","1001 and more"))
##---------------------------------------------------------
establish_cat <- as.data.frame(unique(as.factor(df$establish)))
names(establish_cat)=c("establish")
establish_cat$establish <- sort(establish_cat$establish)

# establish covering all 5 values


# 1	11 تا 20 سال
# 2	2 تا 5 سال
# 3	6 تا 10 سال
# 4	بیشتر از 20 سال
# 5	کمتر از 2 سال

# establish_1 <- as.character(establish_cat$establish[1]) 
# establish_2 <- as.character(establish_cat$establish[2]) 
# establish_3 <- as.character(establish_cat$establish[3]) 
# establish_4 <- as.character(establish_cat$establish[4]) 
# establish_5 <- as.character(establish_cat$establish[5]) 
# 
# 
# df[df$establish==establish_1, "establish"] <- "10-20"
# df[df$establish==establish_2, "establish"] <- "2-5"
# df[df$establish==establish_3, "establish"] <- "5-10"
# df[df$establish==establish_4, "establish"] <- "20 and more"
# df[df$establish==establish_5, "establish"] <- "0-2"

for(estrow in (  1:nrow(df)  )   )
{
  
   if (length (grep(pattern="5",          x=df[estrow,"establish"]))  > 0) 
    df[estrow,"establish"] <- "2-5"
  
  else if (length (grep(pattern="11",     x=df[estrow,"establish"]))  > 0) 
    df[estrow,"establish"] <- "10-20"
  
  else if (length (grep(pattern="6",      x=df[estrow,"establish"]))  > 0) 
    df[estrow,"establish"] <- "5-10"
  
  else if (length (grep(pattern="20",     x=df[estrow,"establish"]))  > 0) 
    df[estrow,"establish"] <- "20 and more"
  
  else  df[estrow,"establish"] <- "0-2"}
  
  
unique(df$establish)
class(df$establish)
summary(as.factor(df$establish))

df$establish <- factor(df$establish,levels=c("0-2","2-5","5-10","10-20","20 and more"))

##-----------------------------------------------------------
ownership_cat <- as.data.frame(unique(as.factor(df$ownership)))
names(ownership_cat)=c("ownership")
ownership_cat$ownership <- sort(ownership_cat$ownership)

# ownership
# 1	دولتی
# 2	خصوصی
# 3	نیمه خصوصی

ownership_1 <- as.character(ownership_cat$ownership[1]) 
ownership_2 <- as.character(ownership_cat$ownership[2]) 
ownership_3 <- as.character(ownership_cat$ownership[3]) 

df[df$ownership==ownership_1, "ownership"] <- "governmental"
df[df$ownership==ownership_2, "ownership"] <- "private"
df[df$ownership==ownership_3, "ownership"] <- "semi-private"

unique(df$ownership)
df$ownership <- factor(df$ownership,levels=c("governmental","private","semi-private"))
##--------------------------------------------------------
nationality_cat <- as.data.frame(unique(df$nationality))
names(nationality_cat)<- c("nationality")
nationality_cat$nationality <- sort(nationality_cat$nationality)

# nationality
# 1	ایرانی
# 2	ایرانی, غیر ایرانی
# 3	غیر ایرانی

nationality_1 <- as.character(nationality_cat$nationality[1]) 
nationality_2 <- as.character(nationality_cat$nationality[2]) 
nationality_3 <- as.character(nationality_cat$nationality[3]) 


df[df$nationality==nationality_1,"nationality"] <- "iranian"
df[df$nationality==nationality_2,"nationality"] <- "both"
df[df$nationality==nationality_3,"nationality"] <- "non-iranian"


unique(df$nationality)
df$nationality <- factor(df$nationality,levels=c("iranian","non-iranian","both"))
##-----------------------------------------------------
class(df$activity)

activity_cat<- as.data.frame(unique(df$activity))
names(activity_cat)<- c("activity")
activity_cat$activity <- sort(activity_cat$activity)
  
# activity
# 1	به عنوان شرکت وابسته به هولدینگ/بانک/ بیمه /سازمان دولتی/وزارتخانه
# 2	به عنوان یک دپارتمان مستقل
# 3	به عنوان یک شرکت کاملا مستقل
# 4	کار مشاوره ای / کار پروژه ای
# 5	کار مشاوره ای / کار پروژه ای, به عنوان یک دپارتمان مستقل
# 6	کار مشاوره ای / کار پروژه ای, به عنوان یک دپارتمان مستقل, به عنوان یک شرکت کاملا مستقل
# 7	کار مشاوره ای / کار پروژه ای, به عنوان یک شرکت کاملا مستقلا مستقل


activity_1 <- as.character(activity_cat$activity)[1]
activity_2 <- as.character(activity_cat$activity)[2]
activity_3 <- as.character(activity_cat$activity)[3]
activity_4 <- as.character(activity_cat$activity)[4:7]

df[df$activity==activity_1,"activity"]     <- "corporation"
df[df$activity==activity_2,"activity"]     <- "department"
df[df$activity==activity_3,"activity"]     <- "company"
df[df$activity %in% activity_4,"activity"] <- "consultancy"


df$activity <- factor(df$activity)
##------------------------------------------------------
unique(df$major)
major_cat<- as.data.frame(unique(df$major))

# unique(df$major)
# 1	الکترونیک
# 2	مهندسی کامپیوتر- IT یا هوش مصنوعی, مجموعه مدیریت - مهندسی صنایع - MBA
# 3	مجموعه مدیریت - مهندسی صنایع - MBA
# 4	مهندسی کامپیوتر- IT یا هوش مصنوعی
# 5	آمار و سایر رشته های Computational
# 6	 سلامت
# 7	ریاضی  - فیزیک - اقتصاد- علوم زیستی و Bioinformatics
# 8	مجموعه مدیریت - مهندسی صنایع - MBA, آمار و سایر رشته های Computational
# 9	حسابداري


df$major_man            <- NA
df$major_science        <- NA
df$major_computer       <- NA
df$major_stat           <- NA

for(manrow in (1:nrow(df)))
{
if (length (grep(pattern="MBA"           ,x=df[manrow,"major"]))  > 0) 
df[manrow,"major_man"] <- "yes" else df[manrow,"major_man"] <- "no" 

if (length (grep(pattern="IT"            ,x=df[manrow,"major"]))  > 0) 
  df[manrow,"major_computer"] <- "yes" else df[manrow,"major_computer"] <- "no" 

if (length (grep(pattern="Bioinformatics",x=df[manrow,"major"]))  > 0) 
  df[manrow,"major_science"] <- "yes" else df[manrow,"major_science"] <- "no" 

if (length (grep(pattern="Computational" ,x=df[manrow,"major"]))  > 0) 
  df[manrow,"major_stat"] <- "yes" else df[manrow,"major_stat"] <- "no" 

}

df[,"major_man"]      <- factor(df[,"major_man"],     levels=c("yes","no"))
df[,"major_science"]  <- factor(df[,"major_science"], levels=c("yes","no"))
df[,"major_computer"] <- factor(df[,"major_computer"],levels=c("yes","no"))
df[,"major_stat"]     <- factor(df[,"major_stat"],    levels=c("yes","no"))









