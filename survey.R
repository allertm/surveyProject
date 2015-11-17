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

## data anlaysis ideas 

## species_id vs year vs weight 

# Mutate weight to kg
surveys %>%
  mutate(weight_kg = weight / 1000) %>% # convert to kg and add in column
  filter(!is.na(weight)) %>%
  head

# data parsing
data1 <- surveys %>%
  group_by(species_id, year) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  filter(!is.na(mean_weight)) 
  
#filtered_d1.2 <- data1 %>%
  #mutate(mean_weight_kg = mean_weight / 1000) %>% # convert to kg and add in column
  #filter(!is.na(mean_weight)) %>%
  #head

# build figure
ggplot(data = data1, aes(x = year, y = mean_weight, group = species_id, color = species_id)) + geom_line()
ggsave("Rproject1.pdf")

# species_id vs month vs weight (only include males, females could be pregnant and bias the data)

data2 <- surveys %>%
  group_by(species_id, month, sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  filter(!is.na(mean_weight), sex == "M") %>%
  head
?"dplyr-package"

data3 <- surveys %>%
  group_by(species_id, month, sex, weight) %>%
  filter(sex == "M", !is.na(weight)) %>%
  head





# weight vs speceis vs hindfoot length (comparison)



# sex vs hindfoot length vs species(distribution)
# speceis vs year vs sex (distribution)


