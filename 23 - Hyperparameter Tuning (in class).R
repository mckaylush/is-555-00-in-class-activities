library(tidyverse)
library(tidymodels)


# Setup w/ Credit Data --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1') %>% 
  mutate(status = as.factor(status))

cr_data %>% glimpse()

# Model setup:
set.seed(42)
cr_split <- initial_split(cr_data, strata = status)
cr_training <- cr_split %>% training()
cr_testing <- cr_split %>% testing()

# Let's create a recipe that:
#    - imputes missing numeric values
#    - log transforms assets, debt, income, price, expenses
#    - normalizes all numeric predictors
#    - dummy codes all categories
#    - downsamples the bad/good status counts
cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = 1)

cr_training %>% count(status)
cr_rec %>% prep() %>% juice() %>% count(status)

# Now let's setup a model spec (rpart decision tree), workflow, and do a cross validation.
model_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_wkfl <- workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(cr_rec)

cr_folds <- vfold_cv(data = cr_training, strata = status)




# Next, a tunable model specification, recipe, and workflow:

model_spec_tunable <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_rec_tunable <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = tune())

cr_wkfl_tunable <- workflow() %>% 
  add_model(model_spec_tunable) %>% 
  add_recipe(cr_rec_tunable)

# Extract parameters, create a grid to search, then search:

params <- extract_parameter_set_dials(cr_wkfl_tunable)

tuning_grid <- grid_random(params,n=5)

tuning_results <- cr_wkfl_tunable %>% 
  tune_grid(grid = tuning_grid,
            resamples = cr_folds)

# Look at the results, selecting the best one and using those to finalize.






