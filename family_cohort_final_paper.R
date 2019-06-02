#====================================
# Final Research Project Analysis 
#====================================

#-----------------------------------
# Setting up workspace
#-----------------------------------

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "family-cohort") {
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
source(file.path(root, "data.R"))

# Loading the packages we want
library(tidyverse)
library(haven) # for reading stata data
library(lfe) # for fixed effect regression
library(stargazer) # for pretty regression tables
library(xtable)
library(Hmisc)
library(ggplot2)
#-----------------------------------
# Loading In the Data
#-----------------------------------

#df <- read_dta(file.path(ddir, "Joneses", "ipums.dta.gz"))

#save(small_df, file = file.path(ddir, "Joneses", "small_df.Rda"))

load(file.path(ddir, "df.Rda"))

#Use this to observe changes in your data 
small_section <- df[1:10,]
View(small_section)

set.seed(100)
sample_df = sample_n(df, 100000)

#-----------------------------------
# Cleaning Our Data
#-----------------------------------

#Here is where we will clean our data

#Trying to figure out #s of people in a given city
cities <- as.data.frame(table(df$PWMETRO))
names(cities) <- c("PWMETRO", "number")
cities <- cities[order(cities$number),]
cities1to45 <- cities[1:45,]
cities46to90 <- cities[46:90,]
cities91to135 <- cities[91:135,]
cities136to180 <- cities[136:180,]
cities181to225 <- cities[181:225,]
cities226to270 <- cities[226:270,]
cities271to300 <- cities[271:300,]
cities301to332 <- cities[301:332,]

print(xtable(cities1to45, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole1-45.tex")
print(xtable(cities46to90, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole46-90.tex")
print(xtable(cities91to135, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole91-135.tex")
print(xtable(cities136to180, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole136-180.tex")
print(xtable(cities181to225, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole181-225.tex")
print(xtable(cities226to270, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole226-270.tex")
print(xtable(cities271to300, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole271-300.tex")
print(xtable(cities301to332, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole301-332.tex")


#smaller datasets organized by city & year 
Milwaukee_1980 <- subset(df, PWMETRO == 5080) %>% subset(YEAR == 1980)
Milwaukee_1990 <- subset(df, PWMETRO == 5080) %>% subset(YEAR == 1990)
Milwaukee_2000 <- subset(df, PWMETRO == 5080) %>% subset(YEAR == 2000)
MilwaukeeAll <- subset(df, PWMETRO == 5080)


SanAntonio_1980 <- subset(df, PWMETRO == 7240) %>% subset(YEAR == 1980)
SanAntonio_1990 <- subset(df, PWMETRO == 7240) %>% subset(YEAR == 1990)
SanAntonio_2000 <- subset(df, PWMETRO == 7240) %>% subset(YEAR == 2000)
SanAntonioAll <- subset(df, PWMETRO == 7240)

Charlotte_1980 <- subset(df, PWMETRO == 1520) %>% subset(YEAR == 1980)
Charlotte_1990 <- subset(df, PWMETRO == 1520) %>% subset(YEAR == 1990)
Charlotte_2000 <- subset(df, PWMETRO == 1520) %>% subset(YEAR == 2000)
CharlotteAll <-subset(df, PWMETRO == 1520)

Chicago_1980 <- subset(df, PWMETRO == 1600) %>% subset(YEAR == 1980)
Chicago_1990 <- subset(df, PWMETRO == 1600) %>% subset(YEAR == 1990)
Chicago_2000 <- subset(df, PWMETRO == 1600) %>% subset(YEAR == 2000)
ChicagoAll <- subset(df, PWMETRO == 1600)

Houston_1980 <- subset(df, PWMETRO == 3360) %>% subset(YEAR == 1980)
Houston_1990 <- subset(df, PWMETRO == 3360) %>% subset(YEAR == 1990)
Houston_2000 <- subset(df, PWMETRO == 3360) %>% subset(YEAR == 2000)
HoustonAll <- subset(df, PWMETRO == 3360)

Atlanta_1980 <- subset(df, PWMETRO == 520) %>% subset(YEAR == 1980)
Atlanta_1990 <- subset(df, PWMETRO == 520) %>% subset(YEAR == 1990)
Atlanta_2000 <- subset(df, PWMETRO == 520) %>% subset(YEAR == 2000)
AtlantaAll <- subset(df, PWMETRO == 520)


#sex codebooks
sex_codebook <- tibble(SEX = c(1, 2),
                       sex = c("Male", "Female"))
sexsp_codebook <- tibble(SEX_SP = c(1,2),
                         sexsp = c("Male", "Female"))

#limited df to test stuff on
dflimited <- head(df, 15)

#race codebooks
race_codebook <- tibble(RACE = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        race = c("White", "Black", "Native",
                                 "Asian", "Asian", "Asian",
                                 "Other", "Multi", "Multi"))
racesp_codebook <- tibble(RACE_SP = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                          racesp = c("White", "Black", "Native",
                                 "Asian", "Asian", "Asian",
                                 "Other", "Multi", "Multi"))

#demonstration codebooks work on limited version
dflimitedclean <- dflimited %>% left_join(sex_codebook) %>% 
                                left_join(sexsp_codebook) %>%
                                left_join(race_codebook) %>%
                                left_join(racesp_codebook)
#Beginning to get rid of some of the columns for the df
filtereddf <- df %>% select(-GQ, -NFAMS, -NSUBFAM, -NCOUPLES,
                            -MULTGEN, -MULTGEND, -FAMUNIT, -SUBFAM, 
                            -SFTYPE, -SFRELATE, -CBSUBFAM, -CBSFTYPE, 
                            -CBSFRELATE, -NCHILD, -ELDCH, -YNGCH, -RELATE, -RELATED)
#Attempt to clean the larger df; fails b/c of size
filtereddfclean <- filtereddf %>% left_join(sex_codebook) %>% 
  left_join(sexsp_codebook) %>%
  left_join(race_codebook) %>%
  left_join(racesp_codebook)


# Sunday, April 28th
NYmetro_df <- df %>%
  filter(PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

NYinter_df <- NYmetro_df %>%
  filter(RACE != RACE_SP)

summary(NYmetro_df)
  
LAmetro_df <- df %>%
  filter(PWMETRO == 4480) 

#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 


metro <- sample_df$PWMETRO

##


#"Proof of Concept" - Cedric

tiny_df <- df[1:1000000, 1:105]

#the following show distribution of Speaks English of Spouse for each English proficiency level of the person who filled out the survey 

#Head of house: "Does not speak English"

clean_df1 <- tiny_df %>%
  filter(SPEAKENG == 1)

p1 <- hist(clean_df1$SPEAKENG_SP)

#Head of house: "Yes, speaks English..."
#this gives: "Error in hist.default(clean_df2$SPEAKENG_SP) : invalid number of 'breaks'"
#it seems that no one responded with "2" := "Yes, speaks English..." 
#so we can probably just disregard this category entirely to make our lives a little easier

clean_df2 <- tiny_df %>%
  filter(SPEAKENG == 2)

p2 <- hist(clean_df2$SPEAKENG_SP)

#Head of house: "Yes, speaks only English"

clean_df3 <- tiny_df %>%
  filter(SPEAKENG == 3)

p3 <- hist(clean_df3$SPEAKENG_SP)

#Head of house: "Yes, speaks very well"

clean_df4 <- tiny_df %>%
  filter(SPEAKENG == 4)

p4 <- hist(clean_df4$SPEAKENG_SP)

#Head of house: "Yes, speaks well" 

clean_df5 <- tiny_df %>%
  filter(SPEAKENG == 5)

p5 <- hist(clean_df5$SPEAKENG_SP)

#Head of house: "Yes, but not well"

clean_df6 <- tiny_df %>%
  filter(SPEAKENG == 6)

p6 <- hist(clean_df6$SPEAKENG_SP)

#The other responses, N/A (Blank), Unkown, and Illegible are irrelevant

plot(SPEAKENG_SP ~ SPEAKENG, data = tiny_df)

#This shows what seems obvious: people are married to people with similar language skills to them
#People who speak only english marry almost exclusively others who speak only english
#It is likely that those who do not have very good English skills and are married share a non-English language
#If we are looking for a cutoff to create a binary "Can English/Cannot English," between 4 and 5 is where a big contrast appears
#Interracial marriage occurs in all language breakdowns:

plot(RACE_SP ~ SPEAKENG, data = tiny_df)

#But, between people who speak only English, there are few interracial marriages and almost no hispanic spouses:

hist(clean_df3$RACE)

hist(clean_df3$RACE_SP)

hist(clean_df3$HISPAN_SP)

#With more language skills, however, both people who responded and their spouses are more racially diverse:

hist(clean_df4$RACE)

hist(clean_df4$RACE_SP)

#Thus, most of our interracial marriage will likely be occuring between those with mutliple shared languages
#I recommend, of SPEAKENG and SPEAKENG_SP we disregard responses 0, 1, 2, 7, and 8
#They are firstly a small portion of the population and are awkward in the context of the other responses
#We can take an angle with the paper of "Interracial marriage between people with at least some English language skills"

#How we could approach this, i.e., use this sample and work from here:

sample_df <- tiny_df %>%
  filter(SPEAKENG > 2,
         SPEAKENG < 7) %>%
  filter(SPEAKENG_SP > 2,
         SPEAKENG_SP <7)

#Recoding 

SPEAKENG_codebook <-
  tibble(SPEAKENG = 3:6,
         ENG = c("Only English", "Very Well", "Well", "Yes, But Not Well"))

SPEAKENGSP_codebook <-
  tibble(SPEAKENG_SP= 3:6,
         ENG_SP = c("Only English", "Very Well", "Well", "Yes, But Not Well"))

sample_df <- 
  sample_df %>%
  left_join(SPEAKENG_codebook) %>%
  left_join(SPEAKENGSP_codebook)

#racial composition within New York metro area

tinyNY_df <- tiny_df %>%
  filter(PWMETRO == 5600)

NY_df <- df %>%
  filter(PWMETRO == 5600)

NY_df12 <- df %>%
  filter(PWMETRO == 5600, MARST == 1 | MARST == 2)

NY_df2 <- NY_df %>%
  filter(MARST == 2)

#MARST == 1: spouse present


NY_df1 <- NY_df %>%
  filter(MARST == 1)

pwmetro <- tibble(PWMETRO = c(4480), pwmetro = "Los Angeles-Long Beach")








f1 <-
ggplot(data=, aes(x=race, y=percent)) +
    geom_bar(stat="identity", color="blue", fill="white")

# Sunday, May 5th

# We decide to work on interracial marriages in Los Angeles-Long Beach this time

# part 1: data cleaning

LAmetro_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

# clean the dataset for interracial marriages in LA in the 1980s

LAmetro_1980_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1980) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1980_df <- LAmetro_1980_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 1990s

LAmetro_1990_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1990) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1990_df <- LAmetro_1990_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 2000s

LAmetro_2000_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 2000) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_2000_df <- LAmetro_2000_df %>%
  filter(RACE != RACE_SP)

# part 2: creating a new dataset for the figure

LAtable <- LAmetro_df %>%
  group_by(YEAR) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

# part 3: creating a figure

ggplot(data = LAtable, aes(x = YEAR, y = prop_interracial)) +
  geom_point() +
  geom_line()

# part 4: incorporate new cities and create a similar figure

Citiesmetro_df <- df %>%
  filter(PWMETRO == 4480 | PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

Citiestable <- Citiesmetro_df %>%
  group_by(YEAR, PWMETRO) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

# for creating tables with the race percentages once the cityname-year dfs exist and
# for creating a line graph with the race percentages once the cityname-year dfs exist

#MILWAUKEE
#For 1980
Milwaukee_1980 <- Milwaukee_1980 %>% left_join(race_codebook)
observationsMilwaukee_1980 <- nrow(Milwaukee_1980)
raceMilwaukee_1980 <- as.data.frame(table(Milwaukee_1980$race))
names(raceMilwaukee_1980) <- c("Race", "Population")
raceMilwaukee_1980$Percent <- 0
raceMilwaukee_1980$Percent <- (raceMilwaukee_1980$Population / observationsMilwaukee_1980 * 100)
nonwhiteraceMilwaukee_1980 <- raceMilwaukee_1980 %>% filter(raceMilwaukee_1980$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1980, type "latex"), file = "insert file path")
#For 1990
Milwaukee_1990 <- Milwaukee_1990 %>% left_join(race_codebook)
observationsMilwaukee_1990 <- nrow(Milwaukee_1990)
raceMilwaukee_1990 <- as.data.frame(table(Milwaukee_1990$race))
names(raceMilwaukee_1990) <- c("Race", "Population")
raceMilwaukee_1990$Percent <- 0
raceMilwaukee_1990$Percent <- (raceMilwaukee_1990$Population / observationsMilwaukee_1990 * 100)
nonwhiteraceMilwaukee_1990 <- raceMilwaukee_1990 %>% filter(raceMilwaukee_1990$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1990, type "latex"), file = "insert file path")
#For 2000
Milwaukee_2000 <- Milwaukee_2000 %>% left_join(race_codebook)
observationsMilwaukee_2000 <- nrow(Milwaukee_2000)
raceMilwaukee_2000 <- as.data.frame(table(Milwaukee_2000$race))
names(raceMilwaukee_2000) <- c("Race", "Population")
raceMilwaukee_2000$Percent <- 0
raceMilwaukee_2000$Percent <- (raceMilwaukee_2000$Population / observationsMilwaukee_2000 * 100)
nonwhiteraceMilwaukee_2000 <- raceMilwaukee_2000 %>% filter(raceMilwaukee_2000$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_2000, type "latex"), file = "insert file path")
#turning them into one table
raceMilwaukee_1980$Year <- 1980
raceMilwaukee_1990$Year <- 1990
raceMilwaukee_2000$Year <- 2000
raceMilwaukeeall <- rbind(raceMilwaukee_2000, raceMilwaukee_1990, raceMilwaukee_1980)
#plotting this city's racial composition over the years in a line graph
ggplot(data = raceMilwaukeeall, aes(x=Year, y=Percent)) + 
  geom_point(aes(colour=Race)) +
  geom_line(aes(colour=Race)) +
              labs(title = "Percentage of Milwaukee Residents by Race")
#graph of non-white percentages
nonwhiteraceMilwaukee_1980$Year <- 1980
nonwhiteraceMilwaukee_1990$Year <- 1990
nonwhiteraceMilwaukee_2000$Year <- 2000
nonwhiteraceMilwaukeeall <- rbind(nonwhiteraceMilwaukee_2000, nonwhiteraceMilwaukee_1990, nonwhiteraceMilwaukee_1980)
#plotting this city's non-white racial composition over the years in a line graph
ggplot(data = nonwhiteraceMilwaukeeall, aes(x=Year, y=Percent)) + 
              geom_point(aes(colour=Race)) +
              geom_line(aes(colour=Race)) +
              labs(title = "Percentages of Non-white Residents in Milwaukee by Race")


#SAN ANTONIO
#For 1980
SanAntonio_1980 <- SanAntonio_1980 %>% left_join(race_codebook)
observationsSanAntonio_1980 <- nrow(SanAntonio_1980)
raceSanAntonio_1980 <- as.data.frame(table(SanAntonio_1980$race))
names(raceSanAntonio_1980) <- c("Race", "Population")
raceSanAntonio_1980$Percent <- 0
raceSanAntonio_1980$Percent <- (raceSanAntonio_1980$Population / observationsSanAntonio_1980 * 100)
nonwhiteraceSanAntonio_1980 <- raceSanAntonio_1980 %>% filter(raceSanAntonio_1980$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1980, type "latex"), file = "insert file path")
#For 1990
SanAntonio_1990 <- SanAntonio_1990 %>% left_join(race_codebook)
observationsSanAntonio_1990 <- nrow(SanAntonio_1990)
raceSanAntonio_1990 <- as.data.frame(table(SanAntonio_1990$race))
names(raceSanAntonio_1990) <- c("Race", "Population")
raceSanAntonio_1990$Percent <- 0
raceSanAntonio_1990$Percent <- (raceSanAntonio_1990$Population / observationsSanAntonio_1990 * 100)
nonwhiteraceSanAntonio_1990 <- raceSanAntonio_1990 %>% filter(raceSanAntonio_1990$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1990, type "latex"), file = "insert file path")
#For 2000
SanAntonio_2000 <- SanAntonio_2000 %>% left_join(race_codebook)
observationsSanAntonio_2000 <- nrow(SanAntonio_2000)
raceSanAntonio_2000 <- as.data.frame(table(SanAntonio_2000$race))
names(raceSanAntonio_2000) <- c("Race", "Population")
raceSanAntonio_2000$Percent <- 0
raceSanAntonio_2000$Percent <- (raceSanAntonio_2000$Population / observationsSanAntonio_2000 * 100)
nonwhiteraceSanAntonio_2000 <- raceSanAntonio_2000 %>% filter(raceSanAntonio_2000$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_2000, type "latex"), file = "insert file path")
#turning them into one table
raceSanAntonio_1980$Year <- 1980
raceSanAntonio_1990$Year <- 1990
raceSanAntonio_2000$Year <- 2000
raceSanAntonioall <- rbind(raceSanAntonio_2000, raceSanAntonio_1990, raceSanAntonio_1980)
#plotting this city's racial composition over the years in a line graph
ggplot(data = raceSanAntonioall, aes(x=Year, y=Percent)) + 
  geom_point(aes(colour=Race)) +
  geom_line(aes(colour=Race)) +
  labs(title = "Percentage of San Antonio Residents by Race")
#graph of non-white percentages
nonwhiteraceSanAntonio_1980$Year <- 1980
nonwhiteraceSanAntonio_1990$Year <- 1990
nonwhiteraceSanAntonio_2000$Year <- 2000
nonwhiteraceSanAntonioall <- rbind(nonwhiteraceSanAntonio_2000, nonwhiteraceSanAntonio_1990, nonwhiteraceSanAntonio_1980)
#plotting this city's non-white racial composition over the years in a line graph
ggplot(data = nonwhiteraceSanAntonioall, aes(x=Year, y=Percent)) + 
  geom_point(aes(colour=Race)) +
  geom_line(aes(colour=Race)) +
  labs(title = "Percentages of Non-white Residents in San Antonio by Race")



#CHARLOTTE
#For 1980
Charlotte_1980 <- Charlotte_1980 %>% left_join(race_codebook)
observationsCharlotte_1980 <- nrow(Charlotte_1980)
raceCharlotte_1980 <- as.data.frame(table(Charlotte_1980$race))
names(raceCharlotte_1980) <- c("Race", "Population")
raceCharlotte_1980$Percent <- 0
raceCharlotte_1980$Percent <- (raceCharlotte_1980$Population / observationsCharlotte_1980 * 100)
nonwhiteraceCharlotte_1980 <- raceCharlotte_1980 %>% filter(raceCharlotte_1980$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1980, type "latex"), file = "insert file path")
#For 1990
Charlotte_1990 <- Charlotte_1990 %>% left_join(race_codebook)
observationsCharlotte_1990 <- nrow(Charlotte_1990)
raceCharlotte_1990 <- as.data.frame(table(Charlotte_1990$race))
names(raceCharlotte_1990) <- c("Race", "Population")
raceCharlotte_1990$Percent <- 0
raceCharlotte_1990$Percent <- (raceCharlotte_1990$Population / observationsCharlotte_1990 * 100)
nonwhiteraceCharlotte_1990 <- raceCharlotte_1990 %>% filter(raceCharlotte_1990$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_1990, type "latex"), file = "insert file path")
#For 2000
Charlotte_2000 <- Charlotte_2000 %>% left_join(race_codebook)
observationsCharlotte_2000 <- nrow(Charlotte_2000)
raceCharlotte_2000 <- as.data.frame(table(Charlotte_2000$race))
names(raceCharlotte_2000) <- c("Race", "Population")
raceCharlotte_2000$Percent <- 0
raceCharlotte_2000$Percent <- (raceCharlotte_2000$Population / observationsCharlotte_2000 * 100)
nonwhiteraceCharlotte_2000 <- raceCharlotte_2000 %>% filter(raceCharlotte_2000$Race != "White")
#For if we want to print this to a table as well
#print(xtable(racecityname_2000, type "latex"), file = "insert file path")
#turning them into one table
raceCharlotte_1980$Year <- 1980
raceCharlotte_1990$Year <- 1990
raceCharlotte_2000$Year <- 2000
raceCharlotteall <- rbind(raceCharlotte_2000, raceCharlotte_1990, raceCharlotte_1980)
#plotting this city's racial composition over the years in a line graph
ggplot(data = raceCharlotteall, aes(x=Year, y=Percent)) + 
  geom_point(aes(colour=Race)) +
  geom_line(aes(colour=Race)) +
  labs(title = "Percentage of Charlotte Residents by Race")
#graph of non-white percentages
nonwhiteraceCharlotte_1980$Year <- 1980
nonwhiteraceCharlotte_1990$Year <- 1990
nonwhiteraceCharlotte_2000$Year <- 2000
nonwhiteraceCharlotteall <- rbind(nonwhiteraceCharlotte_2000, nonwhiteraceCharlotte_1990, nonwhiteraceCharlotte_1980)
#plotting this city's non-white racial composition over the years in a line graph
ggplot(data = nonwhiteraceCharlotteall, aes(x=Year, y=Percent)) + 
  geom_point(aes(colour=Race)) +
  geom_line(aes(colour=Race)) +
  labs(title = "Percentages of Non-white Residents in Charlotte by Race")

#New Race Graph = proportion non-white
#City Codebook
city_codebook <- tibble(PWMETRO = c(5080, 7240, 1520, 1600, 3360, 520),
                        City = c("Milwaukee", "San Antonio", "Charlotte", "Chicago", "Houston", "Atlanta"))
df_relevant <- df %>% 
  filter(PWMETRO == 1520 | PWMETRO == 7240 | PWMETRO == 5080 | PWMETRO == 1600 | PWMETRO == 3360 | PWMETRO == 520) %>%
  mutate(race_non_white = if_else(RACE != 1, 1, 0, missing = NULL)) 

prop_non_white = df_relevant %>% 
  group_by(PWMETRO, YEAR) %>%
  summarise(diversity_prop = mean(race_non_white))

prop_non_white <- prop_non_white %>% left_join(city_codebook)

#bigcitiesracegraph
bigcitiespropnonwhite <- prop_non_white %>% filter (City == "Chicago" | City == "Houston" | City == "Atlanta")
bigcitiespropnonwhite$Percent <- bigcitiespropnonwhite$diversity_prop * 100
ggplot(data = bigcitiespropnonwhite, aes(x=YEAR, y= Percent)) + 
  geom_point(aes(colour=City)) +
  geom_line(aes(color=City)) +
  labs(title = "Percentage of Non-White Residents in Chicago, Houston, and Atlanta", x = "Year")

#smallcitiesracegraph
smallcitiespropnonwhite <- prop_non_white %>% filter (City == "Milwaukee" | City == "San Antonio" | City == "Charlotte")
smallcitiespropnonwhite$Percent <- smallcitiespropnonwhite$diversity_prop * 100
ggplot(data = smallcitiespropnonwhite, aes(x=YEAR, y= Percent)) + 
  geom_point(aes(colour=City)) +
  geom_line(aes(color=City)) +
  labs(title = "Percentage of Non-White Residents in Charlotte, Milwaukee, and San Antonio", x = "Year")

NY_dfint <- NY_df1 %>%
  filter(RACE != RACE_SP) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 