library(tidyverse)

df1 <- read_csv('https://www.dropbox.com/s/df2w04r0c09kw3f/df_1.csv?dl=1')
df2 <- read_csv('https://www.dropbox.com/s/xfp1qzxvo19ym0x/df_2.csv?dl=1')
df3 <- read_csv('https://www.dropbox.com/s/uzusr9723ffn546/df_3.csv?dl=1')
df4 <- read_csv('https://www.dropbox.com/s/js8tehtsk7btpeq/df_4.csv?dl=1')

# A few sample calculations:
df1 %>% summarize(across(everything(), list(mean = ~mean(.x), 
                                            sd = ~sd(.x))))
df_binded <- bind_rows(df1,df2,df3,df4)
# correlation matrix:
df1 %>% cor()

# simple regression analysis...but here's an explanation:
# 
# lm() --> function for producing a linear regression model.
# y ~ x --> uses R's formula expression syntax to say "y regressed on x"
# %>% coefficients() --> this just extracts the coefficients from the resulting model
lm(y ~ x, data = df1) %>% coefficients()

