# load relevant packages/ libraries

library(tidyverse)
library(readxl)
library(skimr)


# read in dataset
# get it here, https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-02

tuition <- read_excel("datasets/us_avg_tuition.xlsx")

# check data has been read in correctly

head(tuition)
tail(tuition)


# cursory EDA

skim(tuition)



# measure change in tuition by state from 2004-05 to 2015-16 by subtracting the former from the latter

# create new 'change' variable and change 'State' from chr to fct

tuition_change <- tuition %>%  # create new data.frame 
  mutate(change = (`2015-16`-`2004-05`), State = factor(State)) %>%  # create new 'change' variable 
  select(State, change) %>%                                          # and change 'State' from chr to fct   
  arrange(change)  # order by 'change'


# check new data.frame

tuition_change



# create bar chart of changing tuition by state, arranged in descendinr order of change

tuition_change %>% ggplot(aes(x = fct_reorder(State, change), y = change)) +
  geom_col(aes(fill = ifelse(change > 0, "increase", "decrease"), col = "shade")) +
  coord_flip() + 
  labs(y = "Total Change in dollars $", x = NULL) +
  ggtitle("Change in tuition fees 2005-2015 in US states") +
  scale_fill_manual(values = c("increase" = "royalblue3", "decrease" = "orange1"), name = NULL) +
  scale_color_manual(values = c("shade" = "grey35"), name = NULL,  # choose fill and colour, omit legend title
                     labels = NULL, breaks = NULL) +               # and labels
  theme_classic() +
  theme(plot.title = element_text(size = rel(2)),  # adjust plot title text size
        legend.text = element_text(size = rel(1.6)))  # adjust legend text size



# visualise tuition growth from 2004-05 to 2015-16

# new data.frame

tuition_growth <- tuition %>%
  pivot_longer(-1, names_to = "year", values_to = "tuition") %>%  # change to long data
  rename(state = State) %>%
  group_by(state) %>%
  filter(year %in% c("2004-05", "2015-16"))  # filter on 2004-05 and 2015-16 only

# check data.frame

tuition_growth

# create a Cleveland plot by combining geom_point with geom_line
# order states by min(tuition)

tuition_growth %>%
  ggplot(mapping = aes(x = tuition, y = fct_reorder(state, tuition, .fun="min"), col = year, group = state)) +
  geom_line(col = "black") +
  geom_point(size = rel(2.5)) +
  labs(y = "State", x = "Tuition $") +
  theme_minimal() +
  ggtitle("Growth in US Tuition 2005-2015") +
  scale_color_manual(values = c("deeppink1", "darkviolet"), name = NULL) +
  theme(plot.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(2)))
        
  
  
  
