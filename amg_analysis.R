library(dplyr)
library(plyr)
library(gridExtra)
library(questionr)
library(ggplot2)
library(caret)
library(knitr)
library(memisc)
library(foreign)
library(testit)

#loading data
df_08 = read.spss("~/Desktop/Amg_challenge/Political_landscape/2008_October/2008.10.22.release.sav", to.data.frame=TRUE)
df_12 = read.spss("~/Desktop/Amg_challenge/Political_landscape/2012_October/2012.10.24.release.sav", to.data.frame=TRUE)

# Dimensions of Data Frame for year 2008 and 2012
dim(df_08)
dim(df_12)
# names of the column
colnames(df_08)
colnames(df_12)
# Detail description of data frames
str(df_08)
str(df_12)

#Selected features for year 2008
col_names_08 <- c("q01","q02","q03","q05","q05a","q06","q06a","q06b","q07","q16","q24","q36","q37","q38","q39","q40","q45",
               "q46","q46a","q46b","d1","d1a","d2","d3", "d4", "d4a","d5","d5a","d6", "d7", "d8", "d9", "d9a",
               "d10", "d10a", "d11","county", "language","gender", "weight")

# Corresponding selected features for 2012
col_names_12 <- c("q4","q5","q6","q7","q7a","q8","q8a","q8b","q9","q11","q3","q36","q37","q40","q41","q42","q47",
                  "q48","q49","q50","d1","d1a","d2","d3", "d4", "d4a","d5","d5a","d7", "d6", "d8com", "d9", "d9a",
                  "d10", "d10a", "d11","county", "language","gender", "weight")

# Selecting the relevant features in the same order for 2008 and 2012
df_08 <- subset(df_08, select = col_names_08)
df_12 <- subset(df_12, select = col_names_12)

# # 2008 # # 2008 # # 2008 # 2008 # 2008 # 2008 # # 2008 # # 2008 # # 2008 # # 2008 # # 2008 # # 2008 # # 2008 # # 2008 

# Creating and standardizing the party registration variable for the year 2008
df_08$q05a <- ifelse(df_08$q05a == "registered, Democrat", "Democrat",df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 2, "Republican",df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 3, "Other",df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 4, "Independent",df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 5, NA,df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 6, NA,df_08$q05a)
df_08$q05a <- ifelse(df_08$q05a == 7, NA,df_08$q05a)

# Changing the varialbe named to Party_registered
names(df_08)[5] <- "party_registered"

# Checking the party distribution for year 2008
table(df_08$party_registered)

# Creating a Party Preference Variable from three quesions q06, q06a, q06b
df_08$pres_ticketpref <- 1:nrow(df_08)

# NA in factor variables cannot be assigned
df_08$q06 <- as.numeric(df_08$q06)
df_08$q06a <- as.numeric(df_08$q06a)
df_08$q06b <- as.numeric(df_08$q06b)

# NA cannot be compared and hence assigning values to it.
df_08$q06[is.na(df_08$q06)] <- 0
df_08$q06a[is.na(df_08$q06a)] <- 0
df_08$q06b[is.na(df_08$q06b)] <- 0

# Creating the presidential preference variable from questions: q06, q06a, q06b
df_08$pres_ticketpref <- ifelse(df_08$q06 == 1 | df_08$q06 == 2 ,"Democratic",df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06 == 3 ,NA,df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06a == 1 | df_08$q06a == 2,"Republican",df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06a == 3,NA,df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06b == 2,"Democratic",df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06b == 1,"Republican",df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06b == 3,"Other",df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse(df_08$q06b == 4,NA,df_08$pres_ticketpref)
df_08$pres_ticketpref <- ifelse((df_08$pres_ticketpref == "Democratic" | df_08$pres_ticketpref == "Republican" | df_08$pres_ticketpref == "Other"),df_08$pres_ticketpref,NA)

# Presidential Preference distribution for year 2008
table(df_08$pres_ticketpref)

######################################        standardizing for year 2012         ##############################################


# Creating and standardizing the party registration variable for the year 2012
df_12$q7a <- ifelse(df_12$q7a == "registered, Democrat", "Democrat",df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 2, "Republican",df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 3, "Other",df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 4, "Independent",df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 5, NA,df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 6, NA,df_12$q7a)
df_12$q7a <- ifelse(df_12$q7a == 7, NA,df_12$q7a)

# Chaning the the normalized variable to party_registered
names(df_12)[5] <- "party_registered"
# Distribution of Voters for the year 2012
table(df_12$party_registered)
# Creating a Party Preference Variable from three quesions q8, q8a, q8b
df_12$pres_ticketpref <- 1:nrow(df_12)

# NA in factor variables cannot be assigned
df_12$q8 <- as.numeric(df_12$q8)
df_12$q8a <- as.numeric(df_12$q8a)
df_12$q8b <- as.numeric(df_12$q8b)

# NA cannot be compared and hence assigning default values to it.
df_12$q8[is.na(df_12$q8)] <- 0
df_12$q8a[is.na(df_12$q8a)] <- 0
df_12$q8b[is.na(df_12$q8b)] <- 0

# Creating a Party Preference Variable from three quesions q8, q8a, q8b
df_12$pres_ticketpref <- ifelse(df_12$q8 == 1 | df_12$q8 == 2 ,"Democratic",df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8 == 3 ,NA,df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8a == 1 | df_12$q8a == 2,"Republican",df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8a == 3,NA,df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8b == 2,"Democratic",df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8b == 1,"Republican",df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8b == 3,"Other",df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse(df_12$q8b == 4,NA,df_12$pres_ticketpref)
df_12$pres_ticketpref <- ifelse((df_12$pres_ticketpref == "Democratic" | df_12$pres_ticketpref == "Republican" | df_12$pres_ticketpref == "Other"),df_12$pres_ticketpref,NA)

# Distributin of voters for the year 2012
table(df_12$pres_ticketpref)

# Standardizing the ethnicity for the two years 2008 and 2012
df_12$d8com <- as.character(df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "Asian, non-Hispanic", "Asian",df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "black, non-Hispanic","Black or African American" ,df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "Hispanic or Latino, any race","Hispanic or Latino" ,df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "white, non-Hispanic","Caucasian or White And Non-Hispanic" ,df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "other, non-Hispanic","[VOL] Other (SPECIFY)" ,df_12$d8com)
df_12$d8com <- ifelse(df_12$d8com == "multi-race, non-Hispanic","[VOL] Other (SPECIFY)" ,df_12$d8com)
df_12$d8com <- as.factor(df_12$d8com)

# Distribution of two ethnicities for the year 2008 and 2012
table(df_08$d8)
table(df_12$d8com)

# Creating a year variable for the two dataframes, to distinguish respective year entries
df_08$year <- 2008
df_12$year <-2012

# Copying the names, So that the two data frames have the same colnames.
new_names <- names(df_08)
names(df_12) <- new_names

#Combining the two dataframes into one
df <- rbind(df_08, df_12)

# The total number of rows: for validation
assert("Correct no of rows", nrow(df) == (nrow(df_08) + nrow(df_12)))

# Writing the final file in csv to disk
write.csv(df, file="~/Desktop/Amg_challenge/combined.csv")

# Final column names for the, the df questions have the same colnames as for the year 2008 and 2012 standarized.
colnames(df)


p1 <- ggplot(data =df_08, aes(x = pres_ticketpref)) + geom_histogram(width = 0.25, col ='red', fill='green') +
  xlab('Presidential Preference for 2008') + ylab('Number of Voters')
p2 <- ggplot(data = df_12, aes(x = pres_ticketpref)) + geom_histogram(col='green', fill = 'red',width = 0.25) +
  xlab('Presidential Preference for 2012') + ylab('Number of Voters')
grid.arrange(p1,p2,ncol = 2)

##################################                    Relevant Weights                       ####################################       

##weights of 2008
by(df_08$weight,df_08$pres_ticketpref,mean)                                                                      
##weights of 2012
by(df_12$weight,df_12$pres_ticketpref,mean) 

# Graphs showing important patterns and shifts in perceptions
q01  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q01~year,scales = "free") + scale_fill_discrete()
q02  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q02~year,scales = "free") + scale_fill_discrete()
q03  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q03~year,scales = "free") + scale_fill_discrete()
q24  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q24~year,scales = "free") + scale_fill_discrete()
q36  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q36~year,scales = "free") + scale_fill_discrete()
q38  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q38~year,scales = "free") + scale_fill_discrete()
q39  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q39~year,scales = "free") + scale_fill_discrete()
q45  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q45~year,scales = "free") + scale_fill_discrete()
q46a  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q46a~year,scales = "free") + scale_fill_discrete()
q46b  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q46b~year,scales = "free") + scale_fill_discrete()


# Relevant Graphs with some importance
q07  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q07~year,scales = "free") + scale_fill_discrete()
q16  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q16~year,scales = "free") + scale_fill_discrete()
q37  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q37~year,scales = "free") + scale_fill_discrete()
q40  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q40~year,scales = "free") + scale_fill_discrete()
q46  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( q46~year,scales = "free") + scale_fill_discrete()
gen_year  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( gender~year,scales = "free") + scale_fill_discrete()
lan_year  <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( language~year,scales = "free") + scale_fill_discrete()


##################################          Relevant Weights according to demographic             ###################################

# Graphs showing important demographic Relationships
d1a <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d1a~year,scales = "free") + scale_fill_discrete()
d4 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d4~year,scales = "free") + scale_fill_discrete()
d5 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d5~year,scales = "free") + scale_fill_discrete()
d6 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d6~year,scales = "free") + scale_fill_discrete()
d7 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d7~year,scales = "free") + scale_fill_discrete()
d8 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d8~year,scales = "free") + scale_fill_discrete()
d9 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d9~year,scales = "free") + scale_fill_discrete()
d9a <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d9a~year,scales = "free") + scale_fill_discrete()
d11 <- ggplot(data = df, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d11~year,scales = "free") + scale_fill_discrete()


#Graphs with finer granularity
g1 <- ggplot(data = df_08, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d8~q01,scales = "free") + scale_fill_discrete()
g2 <- ggplot(data = df_12, aes(x = pres_ticketpref)) + geom_histogram() + facet_grid( d8~q01,scales = "free") + scale_fill_discrete()
grid.arrange(g1,g2,ncol=2)



# Weights for the demographic
# Age bracket
by(df_08$weight,df_08$d1a,mean)
by(df_12$weight,df_12$d1a,mean)
#Parents or not
by(df_08$weight,df_08$d4,mean)
by(df_12$weight,df_12$d4,mean)
# Employment
by(df_08$weight,df_08$d5,mean)
by(df_12$weight,df_12$d5,mean)
#Relationship
by(df_08$weight,df_08$d6,mean)
by(df_12$weight,df_12$d6,mean)
#Education
by(df_08$weight,df_08$d7,mean)
by(df_12$weight,df_12$d7,mean)
# Ethnicity
by(df_08$weight,df_08$d8,mean)
by(df_12$weight,df_12$d8,mean)
# Born in U.s
by(df_08$weight,df_08$d9,mean)
by(df_12$weight,df_12$d9,mean)
# Citizen
by(df_08$weight,df_08$d9a,mean)
by(df_12$weight,df_12$d9a,mean)
# Salary
by(df_08$weight,df_08$d11,mean)
by(df_12$weight,df_12$d11,mean)














