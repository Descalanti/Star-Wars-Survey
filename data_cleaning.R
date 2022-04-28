'''
Code for cleaning and plotting the Star Wars Survey Data from Fivethirtyeight. 
Authors: Devyn Escalanti
University of Central Florida 
Email: dtescalanti@gmail.com
'''

df <- read.csv("StarWars.csv", sep = ",", header = FALSE) #uploading data 
names(df) <- df[1,] #making the first row into a header 
df <- df[-1,] #deleting unneeded first row 
df <- df[-1,] #deleting unneeded second row 

colSums(is.na(df) |df == "") #check which "columns" have NA values (porbably don't need) 

colnames(df)[4]<-"Seen_1" #renaming unnamed "seen" movie columns 
colnames(df)[5]<-"Seen_2"
colnames(df)[6]<-"Seen_3"
colnames(df)[7]<-"Seen_4"
colnames(df)[8]<-"Seen_5"
colnames(df)[9]<-"Seen_6"


# Star wars data change yes/no questions to true/false ------------------------------


df$"Have you seen any of the 6 films in the Star Wars franchise?"[df$"Have you seen any of the 6 films in the Star Wars franchise?" == "Yes"] <- "TRUE" #changing yes/no values to true/false

df$"Have you seen any of the 6 films in the Star Wars franchise?"[df$"Have you seen any of the 6 films in the Star Wars franchise?" == "No"] <- "FALSE"

table(df$"Have you seen any of the 6 films in the Star Wars franchise?") #checking values in a frequency table


df$"Do you consider yourself to be a fan of the Star Wars film franchise?"[df$"Do you consider yourself to be a fan of the Star Wars film franchise?" == "Yes"] <- "TRUE"

df$"Do you consider yourself to be a fan of the Star Wars film franchise?"[df$"Do you consider yourself to be a fan of the Star Wars film franchise?" == "No"] <- "FALSE"

table(df$"Do you consider yourself to be a fan of the Star Wars film franchise?") #checking values in a frequency table 


# Star wars data change "Seen" column values to true/false ---------------------------------------------------------


df$Seen_1[df$Seen_1 == "Star Wars: Episode I  The Phantom Menace"] <- "TRUE"
df$Seen_1[df$Seen_1 == ""] <- "FALSE"

df$Seen_2[df$Seen_2 == "Star Wars: Episode II  Attack of the Clones"] <- "TRUE"
df$Seen_2[df$Seen_2 == ""] <- "FALSE"

df$Seen_3[df$Seen_3 == "Star Wars: Episode III  Revenge of the Sith"] <- "TRUE"
df$Seen_3[df$Seen_3 == ""] <- "FALSE"

df$Seen_4[df$Seen_4 == "Star Wars: Episode IV  A New Hope"] <- "TRUE"
df$Seen_4[df$Seen_4 == ""] <- "FALSE"

df$Seen_5[df$Seen_5 == "Star Wars: Episode V The Empire Strikes Back"] <- "TRUE"
df$Seen_5[df$Seen_5 == ""] <- "FALSE"

df$Seen_6[df$Seen_6 == "Star Wars: Episode VI Return of the Jedi"] <- "TRUE"
df$Seen_6[df$Seen_6 == ""] <- "FALSE"


# Determine which participants watched all 6 Star War movies. ---------------


t<-subset(df, Seen_6 == "TRUE" & Seen_5 == "TRUE" & Seen_4 == "TRUE" &
            Seen_3 == "TRUE" & Seen_2 == "TRUE"& Seen_1 == "TRUE")


# Episode ranking most to least favorite --------
colnames(t)[10]<-"Ep I" #renaming episode columns  
colnames(t)[11]<-"Ep II"
colnames(t)[12]<-"Ep 3"
colnames(t)[13]<-"Ep IV"
colnames(t)[14]<-"Ep V"
colnames(t)[15]<-"Ep VI"

t$"Ep I" <- as.numeric(t$"Ep I") #convert character values to numeric 
class(t$"Ep I")
sum(t$"Ep I")
#output: 1996 

t$"Ep II" <- as.numeric(t$"Ep II") #convert character values to numeric 
class(t$"Ep II")
sum(t$"Ep II")
#output: 2038

colSums(is.na(t) |t == "") #check which "columns" have NA values (porbably don't need) 

t$"Ep 3" <- as.numeric(t$"Ep 3") #convert character values to numeric 
class(t$"Ep 3")
sum(t$"Ep 3", na.rm = TRUE)
#output: 1999

t$"Ep IV" <- as.numeric(t$"Ep IV") #convert character values to numeric 
class(t$"Ep IV")
sum(t$"Ep IV")
#output: 1350

t$"Ep V" <- as.numeric(t$"Ep V") #convert character values to numeric 
class(t$"Ep V")
sum(t$"Ep V")
#output: 1121

t$"Ep VI" <- as.numeric(t$"Ep VI") #convert character values to numeric 
class(t$"Ep VI")
sum(t$"Ep VI")
#output: 1381

t2 <- aggregate(t$"Ep I", by=list(t$Seen_6), FUN=mean) 
colnames(t2) <- c("var", "rank") 
t2$movie <- c("Ep I") 

t3 <- aggregate(t$"Ep II", by=list(t$Seen_6), FUN=mean) 
colnames(t3) <- c("var", "rank") 
t3$movie <- c("Ep II") 

t4 <- aggregate(t$"Ep 3", by=list(t$Seen_6), FUN=mean,  na.rm = TRUE) 
colnames(t4) <- c("var", "rank") 
t4$movie <- c("Ep III") 

t5 <- aggregate(t$"Ep IV", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t5) <- c("var", "rank") 
t5$movie <- c("Ep IV") 

t6 <- aggregate(t$"Ep V", by=list(t$Seen_6), FUN=mean) 
colnames(t6) <- c("var", "rank") 
t6$movie <- c("Ep V") 

t7 <- aggregate(t$"Ep VI", by=list(t$Seen_6), FUN=mean) 
colnames(t7) <- c("var", "rank") 
t7$movie <- c("Ep VI") 

df_new <- rbind(t2, t3, t4, t5, t6, t7) #merge data by rows into new data frame 

df_new <- df_new [order(df_new$rank), ]   #ordering data by rank 

df_new$rank <- factor(df_new$rank, levels = df_new$rank)  # to retain the order in plot.

df_new$movie <- factor(df_new$movie, levels = df_new$movie)  # to retain the order in plot.

#plotting a lollipop chart of moving rankings (most favored to least favored) 
ggplot(df_new, aes(x=movie, y= rank)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=movie, 
                   xend=movie, 
                   y=0, 
                   yend=rank)) + 
  scale_y_discrete(limits=rev)+
  labs(title="Star Wars Episode Rankings (most favored to least favored)", 
       caption="Source: Fivethirtyeight") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#How sure am I on this ranking? I'm not sure id I was supposed to perform a calculation here but i am highly confident of the episode ranking order. Given that the scale is 1 (most favorite) to 6 (least favorite), episodes with *lower* aggregated ranking averages across respondents have a higher favorability relative to aggregated rankings with higher values. The ranking from most favored to least: Ep V, IV, VI, I, III, II 


# Data Prep for visualization of character ranking -------------

#renaming character columns in the data frame labeled "t"
colnames(t)[16]<-"Hans Solo" #renaming character columns  
colnames(t)[17]<-"Luke Skywalker"
colnames(t)[18]<-"Princess Leia Organa"
colnames(t)[19]<-"Anakin Skywalker"
colnames(t)[20]<-"Obi Wan Kenobi"
colnames(t)[21]<-"Emperor Palpatine"
colnames(t)[22]<-"Darth Vader"  
colnames(t)[23]<-"Lando Calrissian"
colnames(t)[24]<-"Boba Fett"
colnames(t)[25]<-"C-3P0"
colnames(t)[26]<-"R2 D2"
colnames(t)[27]<-"Jar Jar Binks"
colnames(t)[28]<-"Padme Amidala"
colnames(t)[29]<-"Yoda"

#converting character ranking strings into numerical characters (then will covert to numerical class) 

t$"Hans Solo"[t$"Hans Solo" == "Unfamiliar (N/A)"] <- "0"
t$"Hans Solo"[t$"Hans Solo" == "Very unfavorably"] <- "1"
t$"Hans Solo"[t$"Hans Solo" == "Somewhat unfavorably"] <- "2"
t$"Hans Solo"[t$"Hans Solo" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Hans Solo"[t$"Hans Solo" == "Somewhat favorably"] <- "4"
t$"Hans Solo"[t$"Hans Solo"== "Very favorably"] <- "5"
t$"Hans Solo" <- as.numeric(t$"Hans Solo") 
class(t$"Hans Solo")
sum(t$"Hans Solo")

t$"Luke Skywalker"[t$"Luke Skywalker"== "Unfamiliar (N/A)"] <- "0"
t$"Luke Skywalker"[t$"Luke Skywalker" == "Very unfavorably"] <- "1"
t$"Luke Skywalker"[t$"Luke Skywalker" == "Somewhat unfavorably"] <- "2"
t$"Luke Skywalker"[t$"Luke Skywalker" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Luke Skywalker"[t$"Luke Skywalker" == "Somewhat favorably"] <- "4"
t$"Luke Skywalker"[t$"Luke Skywalker" == "Very favorably"] <- "5"
t$"Luke Skywalker" <- as.numeric(t$"Luke Skywalker") 

t$"Princess Leia Organa"[t$"Princess Leia Organa"== "Unfamiliar (N/A)"] <- "0"
t$"Princess Leia Organa"[t$"Princess Leia Organa" == "Very unfavorably"] <- "1"
t$"Princess Leia Organa"[t$"Princess Leia Organa" == "Somewhat unfavorably"] <- "2"
t$"Princess Leia Organa"[t$"Princess Leia Organa" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Princess Leia Organa"[t$"Princess Leia Organa" == "Somewhat favorably"] <- "4"
t$"Princess Leia Organa"[t$"Princess Leia Organa" == "Very favorably"] <- "5"
t$"Princess Leia Organa" <- as.numeric(t$"Princess Leia Organa") #transforming column into numerical values 

t$"Anakin Skywalker"[t$"Anakin Skywalker"== "Unfamiliar (N/A)"] <- "0"
t$"Anakin Skywalker"[t$"Anakin Skywalker" == "Very unfavorably"] <- "1"
t$"Anakin Skywalker"[t$"Anakin Skywalker" == "Somewhat unfavorably"] <- "2"
t$"Anakin Skywalker"[t$"Anakin Skywalker" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Anakin Skywalker"[t$"Anakin Skywalker" == "Somewhat favorably"] <- "4"
t$"Anakin Skywalker"[t$"Anakin Skywalker" == "Very favorably"] <- "5"
t$"Anakin Skywalker" <- as.numeric(t$"Anakin Skywalker") #transforming column into numerical values 

t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi"== "Unfamiliar (N/A)"] <- "0"
t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi" == "Very unfavorably"] <- "1"
t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi" == "Somewhat unfavorably"] <- "2"
t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi" == "Somewhat favorably"] <- "4"
t$"Obi Wan Kenobi"[t$"Obi Wan Kenobi" == "Very favorably"] <- "5"
t$"Obi Wan Kenobi" <- as.numeric(t$"Obi Wan Kenobi") #transforming column into numerical values 

t$"Emperor Palpatine"[t$"Emperor Palpatine"== "Unfamiliar (N/A)"] <- "0"
t$"Emperor Palpatine"[t$"Emperor Palpatine" == "Very unfavorably"] <- "1"
t$"Emperor Palpatine"[t$"Emperor Palpatine" == "Somewhat unfavorably"] <- "2"
t$"Emperor Palpatine"[t$"Emperor Palpatine" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Emperor Palpatine"[t$"Emperor Palpatine" == "Somewhat favorably"] <- "4"
t$"Emperor Palpatine"[t$"Emperor Palpatine" == "Very favorably"] <- "5"
t$"Emperor Palpatine" <- as.numeric(t$"Emperor Palpatine") #transforming column into numerical values 


t$"Darth Vader"[t$"Darth Vader"== "Unfamiliar (N/A)"] <- "0"
t$"Darth Vader"[t$"Darth Vader" == "Very unfavorably"] <- "1"
t$"Darth Vader"[t$"Darth Vader" == "Somewhat unfavorably"] <- "2"
t$"Darth Vader"[t$"Darth Vader" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Darth Vader"[t$"Darth Vader" == "Somewhat favorably"] <- "4"
t$"Darth Vader"[t$"Darth Vader" == "Very favorably"] <- "5"
t$"Darth Vader" <- as.numeric(t$"Darth Vader") #transforming column into numerical values 


t$"Lando Calrissian"[t$"Lando Calrissian"== "Unfamiliar (N/A)"] <- "0"
t$"Lando Calrissian"[t$"Lando Calrissian" == "Very unfavorably"] <- "1"
t$"Lando Calrissian"[t$"Lando Calrissian" == "Somewhat unfavorably"] <- "2"
t$"Lando Calrissian"[t$"Lando Calrissian" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Lando Calrissian"[t$"Lando Calrissian" == "Somewhat favorably"] <- "4"
t$"Lando Calrissian"[t$"Lando Calrissian" == "Very favorably"] <- "5"
t$"Lando Calrissian" <- as.numeric(t$"Lando Calrissian") #transforming column into numerical values 


t$"Boba Fett"[t$"Boba Fett"== "Unfamiliar (N/A)"] <- "0"
t$"Boba Fett"[t$"Boba Fett" == "Very unfavorably"] <- "1"
t$"Boba Fett"[t$"Boba Fett" == "Somewhat unfavorably"] <- "2"
t$"Boba Fett"[t$"Boba Fett" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Boba Fett"[t$"Boba Fett" == "Somewhat favorably"] <- "4"
t$"Boba Fett"[t$"Boba Fett" == "Very favorably"] <- "5"
t$"Boba Fett" <- as.numeric(t$"Boba Fett") #transforming column into numerical values 


t$"C-3P0"[t$"C-3P0"== "Unfamiliar (N/A)"] <- "0"
t$"C-3P0"[t$"C-3P0" == "Very unfavorably"] <- "1"
t$"C-3P0"[t$"C-3P0" == "Somewhat unfavorably"] <- "2"
t$"C-3P0"[t$"C-3P0" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"C-3P0"[t$"C-3P0" == "Somewhat favorably"] <- "4"
t$"C-3P0"[t$"C-3P0" == "Very favorably"] <- "5"
t$"C-3P0" <- as.numeric(t$"C-3P0") #transforming column into numerical values 


t$"R2 D2"[t$"R2 D2"== "Unfamiliar (N/A)"] <- "0"
t$"R2 D2"[t$"R2 D2" == "Very unfavorably"] <- "1"
t$"R2 D2"[t$"R2 D2" == "Somewhat unfavorably"] <- "2"
t$"R2 D2"[t$"R2 D2" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"R2 D2"[t$"R2 D2" == "Somewhat favorably"] <- "4"
t$"R2 D2"[t$"R2 D2" == "Very favorably"] <- "5"
t$"R2 D2" <- as.numeric(t$"R2 D2") #transforming column into numerical values 


t$"Jar Jar Binks"[t$"Jar Jar Binks"== "Unfamiliar (N/A)"] <- "0"
t$"Jar Jar Binks"[t$"Jar Jar Binks" == "Very unfavorably"] <- "1"
t$"Jar Jar Binks"[t$"Jar Jar Binks" == "Somewhat unfavorably"] <- "2"
t$"Jar Jar Binks"[t$"Jar Jar Binks" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Jar Jar Binks"[t$"Jar Jar Binks" == "Somewhat favorably"] <- "4"
t$"Jar Jar Binks"[t$"Jar Jar Binks" == "Very favorably"] <- "5"
t$"Jar Jar Binks" <- as.numeric(t$"Jar Jar Binks") #transforming column into numerical values 


t$"Padme Amidala"[t$"Padme Amidala"== "Unfamiliar (N/A)"] <- "0"
t$"Padme Amidala"[t$"Padme Amidala" == "Very unfavorably"] <- "1"
t$"Padme Amidala"[t$"Padme Amidala" == "Somewhat unfavorably"] <- "2"
t$"Padme Amidala"[t$"Padme Amidala" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Padme Amidala"[t$"Padme Amidala" == "Somewhat favorably"] <- "4"
t$"Padme Amidala"[t$"Padme Amidala" == "Very favorably"] <- "5"
t$"Padme Amidala" <- as.numeric(t$"Padme Amidala") #transforming column into numerical values 


t$"Yoda"[t$"Yoda"== "Unfamiliar (N/A)"] <- "0"
t$"Yoda"[t$"Yoda" == "Very unfavorably"] <- "1"
t$"Yoda"[t$"Yoda" == "Somewhat unfavorably"] <- "2"
t$"Yoda"[t$"Yoda" == "Neither favorably nor unfavorably (neutral)"] <- "3"
t$"Yoda"[t$"Yoda" == "Somewhat favorably"] <- "4"
t$"Yoda"[t$"Yoda" == "Very favorably"] <- "5"
t$"Yoda" <- as.numeric(t$"Yoda") #transforming column into numerical values 

#aggregating values for average score across respondents 
t1 <- aggregate(t$"Hans Solo", by=list(t$Seen_6), FUN=mean) 
colnames(t1) <- c("var", "rank") 
t1$character <- c("Hans Solo") 

t2 <- aggregate(t$"Luke Skywalker", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t2) <- c("var", "rank") 
t2$character <- c("Luke Skywalker") 

t3 <- aggregate(t$"Princess Leia Organa", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t3) <- c("var", "rank") 
t3$character <- c("Princess Leia Organa") 

t4 <- aggregate(t$"Anakin Skywalker", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t4) <- c("var", "rank") 
t4$character <- c("Anakin Skywalker") 

t5 <- aggregate(t$"Obi Wan Kenobi", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t5) <- c("var", "rank") 
t5$character <- c("Obi Wan Kenobi") 

t6 <- aggregate(t$"Emperor Palpatine", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t6) <- c("var", "rank") 
t6$character <- c("Emperor Palpatine") 

t7 <- aggregate(t$"Darth Vader", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t7) <- c("var", "rank") 
t7$character <- c("Darth Vader") 

t8 <- aggregate(t$"Lando Calrissian", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t8) <- c("var", "rank") 
t8$character <- c("Lando Calrissian") 

t9 <- aggregate(t$"Boba Fett", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t9) <- c("var", "rank") 
t9$character <- c("Boba Fett") 

t10 <- aggregate(t$"C-3P0", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t10) <- c("var", "rank") 
t10$character <- c("C-3P0") 

t11 <- aggregate(t$"R2 D2", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t11) <- c("var", "rank") 
t11$character <- c("R2 D2") 

t12 <- aggregate(t$"Jar Jar Binks", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t12) <- c("var", "rank") 
t12$character <- c("Jar Jar Binks") 

t13 <- aggregate(t$"Padme Amidala", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t13) <- c("var", "rank") 
t13$character <- c("Padme Amidala") 

t14 <- aggregate(t$"Yoda", by=list(t$Seen_6), FUN=mean, na.rm = TRUE) 
colnames(t14) <- c("var", "rank") 
t14$character <- c("Yoda") 

df2 <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) #merge data by rows into new data frame 

df2 <- df2 [order(df2$rank), ]   #ordering data by rank 

df2$rank <- factor(df2$rank, levels = df2$rank)  # to retain the order in plot.

df2$character <- factor(df2$character, levels = df2$character)  # to retain the order in plot.



# Character Ranking least to most favorite  ------------------------------


#plotting a lollipop chart of moving rankings (least to most favored) 
ggplot(df2, aes(x=character, y= rank)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=character, 
                   xend=character, 
                   y=0, 
                   yend=rank)) + 
  labs(title="Star Wars Character Rankings (from least to most favored)", 
       caption="Source: Fivethirtyeight") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


