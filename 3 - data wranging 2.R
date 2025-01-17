library(tidyverse)

# The mpg dataset is another one of those 'hidden' datasets that is always loaded:
df <- mpg


# Let's do some warmup exercises:
# 
# Create a summary that includes only cars manufactured by "toyota" or "honda", 
# with displ less than 2.0. Show only the manufacturer, model, year, and displ 
# columns in the result, sorted by displ in ascending order.
df %>% 
  count(manufacturer)

df %>% 
  filter(manufacturer %in% c('toyota','honda')) %>% 
  filter(displ < 2) %>% 
  select(manufacturer,model,year,displ) %>% 
  arrange(displ)

# Add a new column called efficiency that calculates the average fuel efficiency 
# as (cty + hwy) / 2. Filter the dataset to show only rows where efficiency is 
# greater than 25 and the class is "compact". Display the manufacturer, model, 
# efficiency, and class columns, sorted by efficiency in descending order.





# Group the dataset by class and calculate:
#    - The total number of cars in each class (total_cars).
#    - The average city mileage (avg_cty).
#    - The average highway mileage (avg_hwy).
# Display the results, sorted by total_cars in descending order.

df %>% 
  group_by(class) %>% 
  mutate(max_hwy_mpg= max(hwy))

df %>% 
  group_by(class) %>% 
  summarise(avg_hwy_mpg = mean(hwy),
            total_cars = n(),
            distinct_count_manufacturer = n_distinct(manufacturer))
df %>% 
  group_by(manufacturer) %>% 
  slice_min(displ, with_ties = F)

df %>% 
  # select(manufacturer,year) %>% 
  group_by(manufacturer) %>% 
  arrange(desc(year),desc(hwy)) %>% 
  slice_head(n=2)
# Keeps the top 2 listings for every manufacturer. Need to keep it in group_by() in order for it to break up into each group
