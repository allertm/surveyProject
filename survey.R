## survey project in R

# install and load library
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#download file
download.file ("http://files.figshare.com/2236372/combined.csv","Data/portal_data_joined.csv")
# load file
surveys <- read.csv('Data/portal_data_joined.csv')

## data anlaysis 

# species_id vs year vs weight 

# data parsing
data1 <- surveys %>%
  group_by(species_id, year, sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  filter(!is.na(mean_weight)) %>%
  filter(sex == "M")
  
# graph (distribution)
ggplot(data = data1, aes(x = year, y = mean_weight, group = species_id, color = species_id)) + geom_line() + annotate("text", x = 1990, y = 225, label = "Mean weight of all male species from 1977 to 2002") + theme_bw()
ggsave("Rproject1.pdf")

# species_id vs year vs hindfoot_length (only include females)
data2 <- surveys %>%
  group_by(species_id, hindfoot_length, year, sex) %>%
  summarize(mean_hindfoot_length = mean(hindfoot_length, na.rm = TRUE)) %>%
  filter(sex == "F", species_id == "BA")
## statisical test 
# ANOVA
fml <- aov(year~hindfoot_length, data=data2)
anova(fml)
# p- value= 0.714

## graph (comparison)
# load data  
f <- data.frame(Year = c("1990", "1991", "1992"), mean_hindfoot_length = c (14, 12.1667, 13.5)) 
# basic layers and graph options
m <- ggplot(f, aes(y = mean_hindfoot_length))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Exectuting the graph
m + geom_bar(aes(x = Year, color = Year), stat = "identity") + theme_bw() + annotate("text", x = 2, y = 18, label = "Mean Hindfoot Length of Female Baiomys taylori")+ annotate ("text", x = 1, y = 15, label = "p = 0.714")
ggsave("RProject2.pdf")


# weight vs speceis vs hindfoot length, male only 
# data parsing
data3 <- surveys %>%
  group_by(weight, species_id, hindfoot_length, sex) %>%
  filter( sex == "M") 
# graph (relationship)
ggplot(data = data3, aes(x = hindfoot_length, y = weight, color = species_id)) + geom_point() + annotate("text", x = 30, y = 325, label = "Comparing hindfoot length and weight for males from 1977 to 2002") + theme_bw() 
ggsave("RProject3.pdf")



