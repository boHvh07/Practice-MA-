library(readr)
library(dplyr)

boston_test <- read_csv("~/Downloads/boston_test.csv")

Btest <- boston_test[,-1]

Btest <- transform(Btest, ID = as.numeric(factor("Restaurant.Name")))


### Fix count and this is unnecesary
Btest <- Btest %>%                        
  group_by(Restaurant.Name) %>%
  dplyr::mutate(ID = cur_group_id())

### count function for MOD
#fOR DUPLICATE COUNT?
sotu_whole %>% 
  mutate(n_citizen = str_count(text, "citizen")) ???

Btest$Count <- Btest %>% count()

nrow() #Use Nrow