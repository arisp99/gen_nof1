# N-Of-1 (Single Subject Design)

This repo is a based of another repo by MikeJSeo, which introduces a nof1 package that can be used to analyze nof1 studies. The goal in this repo is to take the package that was created and generalize it to work for any nof1 study.

# To Install and Load the original nof1 package

```{r}
install.packages("devtools")
devtools::install_github("arisp99/nof1gen")
library(nof1gen)
```

# Format of the data file

The file must contain two parts, the data section and the metadata section. The data section will contain all the observations taken during the trial. For each observation, we will have a list of the treatment the patient was on at the time followed by a list of all the data points collected. These two lists should be the same length. The metadata section will then contain the  user_id, the trigger, the design of the trial, whether or not a washout  period will be used (if not in metadata, default is TRUE), the alpha value for the confidence interval, the start and end  date (YYYY-MM-DD), followed by the response type of each of the observations. Either binomial, poisson, or normal. The response types must be in the same order as the observations in the data section and must be the last pieces of information in the metadata section. Refer to formated data.json and afib_formated.json for two examples.

# Running the simulations
The package itself contains six example data sets. Below is how we would run the simulations on each of the six datasets.
```{r}
# Use the function gen_wrap to run the model
result_afib <-gen_wrap(afib_form)
result_afib_var <-gen_wrap(afib_form_no_var)
result_diet <-gen_wrap(diet_form)
result_diet_small <-gen_wrap(diet_small)
result_no_mscd <-gen_wrap(no_mscd)
result_no_scd <-gen_wrap(no_scd)

# Get the results in json format
output_afib <-
  toJSON(result_afib, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_afib_var <-
  toJSON(result_afib_var, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet <-
  toJSON(result_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_mscd <-
  toJSON(result_no_mscd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_scd <-
  toJSON(result_no_scd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)

# Graphs can also be used to analyze the data. To do this, we need to run the nof1.summary.R file
# In order to run the graphs, we need to feed in a object created by nof1.data
# ex: (N.B. this will not work as variables not yet defined)
nof1 <- nof1.data(Y, Treat, response = response_type)
frequency_plot(nof1)
stacked_percent_barplot(nof1)
raw_table(nof1)
```
