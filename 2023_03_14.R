# SEP
# 2023-03-14
# joins and shape

# mutating join adds data
# filtering reduces size based off what exists in the second data frame

library(tidyverse)

data1 = data.frame(ID = c(1,2), 
                   X1 = c("a1", "a2"))
data1

data2 = data.frame(ID = c(2,3), 
                   X1 = c("b1", "b2"))
data2

# left_join
data12 = left_join(data1, data2, by = "ID")

data12

# may want to join by more than one column, joining by date and site

data12 = data1 %>%
  left_join(data2, by = "ID")
data12
# only joining where there is a link between the two columns 

data12 = data1 %>%
  right_join(data2, by = "ID")
data12
# preserves the right side of the data (second data set)
# pretend right join does not exist at all 

data12 = data1 %>% 
  inner_join(data2, by = "ID")
data12
# the only the ID that is preserved is the only column they have in common

data12 = data1 %>%
  full_join(data2, by= "ID")
data12
# retains everything whether or not they have it in common
# any place where they can't fill in information from the other data frame, it inserts an NA

#semi_join
data12 = data1%>%
  semi_join(data2, by= "ID")
data12
# keep only the rows that have corresponding values for each ID

data12 = data1 %>%
  anti_join(data2, by = "ID")
data12
# what you dont have in the second data set to match the first data set

# Exercise 1.1

# left join by common name

# wider vs. longer

survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                     barnacle_n = c(2, 11, 8, 27),
                     chiton_n = c(1, 0, 0, 2),
                     mussel_n = c(0, 1, 1, 4))
survey
# this is the wide format of the data
# each species gets its own column

long = survey %>%
  pivot_longer(cols= c("barnacle_n", "chiton_n", "mussel_n"), 
               names_to = "taxon", 
               values_to = "counts")
long


wide = long %>%
  pivot_wider(names_from = taxon, 
              values_from = counts)
wide


# Exercise 1.2

ggplot(data = wide) +
  geom_point(data = wide, aes(x= quadrat_id, y = barnacle_n), color = "blue") +
  geom_point(data = wide, aes(x= quadrat_id, y = chiton_n), color = "red") +
  geom_point(data = wide, aes(x= quadrat_id, y = mussel_n), color = "green")
  
ggplot(data = wide) +
  geom_point(data = wide, aes(x= quadrat_id, y = barnacle_n, color = "barnacle")) +
  geom_point(data = wide, aes(x= quadrat_id, y = chiton_n, color = "chiton")) +
  geom_point(data = wide, aes(x= quadrat_id, y = mussel_n, color = "mussel"))

ggplot(data = long) + 
  geom_point(data = long, aes(x= quadrat_id, y = counts, color = taxon)) 








































