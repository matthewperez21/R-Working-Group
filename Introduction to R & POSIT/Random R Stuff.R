# Example code for dplyr

df <- data.frame('Name' = c("Jon", "Jon", "Bobby", "Sarah", "Fred"),
                 'Race' = c("Black", "White", "Asian", "Asian", "Black"))
df1 <- df %>%
  group_by(Name) %>%
  summarise(Race = ifelse(n() > 1, "multi-racial", Race))

# load data example

client_data <- read.csv("C:/Users/perezm27/Documents/R Working Group/Example Dataset.csv")

# Cleaning and preparing the dataset

str(client_data) #checking structure of our dataset

library(Amelia)
missmap(client_data, col=c("black", "grey")) #plot a missingness map to see where our missing values are

client_data = na.omit(client_data) #use to deal with NA values in Age columns

client_data$ABSTINENCE = factor(client_data$ABSTINENCE)

client_data$SES = factor(client_data$SES, order = TRUE, levels = c(3, 2, 1)) #This is an ordinal variable so we set order = TRUE 
                                                          #and includes the levels argument in ascending order (Class 3< Class2 <Class 1)  

#Correlation plot

library(GGally)
ggcorr(client_data, 
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")


#Abstinence count
ggplot(client_data, aes(x = ABSTINENCE)) + #aes() specifies the x and y axes variables we want to use
  geom_bar(width = 0.5, fill = "coral") + #used for bar graphs
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) + #used to label the bars
  theme_classic() #built-in color schemes

#Abstinence count by Sex
ggplot(client_data, aes(x = ABSTINENCE, fill=SEX)) +
  geom_bar(position = position_dodge()) + #position_dodge() is to show the bars side by side
  geom_text(stat='count',
            aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust = -0.5) +
  theme_classic()

#Abstinence by SES
ggplot(client_data, aes(x = ABSTINENCE, fill = SES)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat = 'count', 
            aes(label = stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()

#Age Density
ggplot(client_data, aes(x = AGE)) +
  geom_density(fill = 'coral')

#Abstinence by Age
client_data$Discretized.age = cut(client_data$AGE, #Discretize age to plot abstinence with cut() to specify the cuts in our vector
                                  c(0, 10, 20, 30, 40, 50, 60, 70, 80, 100)) 

ggplot(client_data, aes(x = Discretized.age, fill = ABSTINENCE)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat = 'count', aes(label = stat(count)), 
            position = position_dodge(width = 1), vjust = -0.5) +
  theme_classic()

client_data$Discretized.age = NULL

#Notes for R workshop - enter into R introduction course RMD 

test <- c("a", "b", "c")

test <- as.factor(test)

test

summary(titanic$MONTHLY_INCOME)

getwd()
