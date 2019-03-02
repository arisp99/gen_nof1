# N-Of-1 (Single Subject Design)

This repo is a based of another repo by MikeJSeo, which introduces a nof1 package that can be used to analyze nof1 studies. The goal in this repo is to take the package that was created and generalize it to work for any nof1 study.

# To Install and load the package

```{r}
install.packages("devtools")
devtools::install_github("arisp99/nof1gen")
library(nof1gen)
```

# Format of the data file

The file must contain two parts, the data section and the metadata section. The data section should contain all the data from the outcomes considered during the trial. For each outcome, we will have a list (time series) of the treatment the patient was on at the time followed by a list (time series) of all the data points collected. These two lists should be the same length although different outcomes can have different lengths. The metadata section will then contain the  user_id, the trigger, the design of the trial, the start and end  date (YYYY-MM-DD), followed by the response type of each of the outcomes, either normal, binomial, poisson, or ordinal. The response types must be in the same order as the outcomes in the data section and must be the last pieces of information in the metadata section. Refer to afib_form.json and diet_form.json for two examples in the data-raw folder.

# Running the simulations

The package itself contains six example data sets. Below is how we would run the simulations on each of the six datasets. If we wanted to run a new dataset we would simply need to extract the data from the file (ex. can use fromJSON() if it a .json file) and then we can use either the wrap_single function or the wrap_all function to run the analysis on the new data set. wrap_single will run the analysis on a single specified outcome whereas wrap_all will run the analysis on all the outcomes.

```{r}
# Use the function wrap_single to run the model on a single outcome
result_afib       <- wrap_single(afib_form, "afib_episode_yn", "binomial", FALSE)
result_afib_var   <- wrap_single(afib_form_no_var, "afib_episode_yn", "binomial", FALSE)
result_diet       <- wrap_single(diet_form, "daily_stool_consistency", "binomial", FALSE)
result_diet_small <- wrap_single(diet_form_small, "daily_stool_frequency", "poisson", FALSE)
result_no_mscd    <- wrap_single(no_mscd_form, "promis_pain_interference", "normal", FALSE)
result_no_scd     <- wrap_single(no_scd_form, "promis_gi_symptoms", "normal", FALSE)

# Use the function wrap_all to run the model for each outcome
all_result_afib       <- wrap_all(afib_form, list("binomial"), FALSE)
all_result_afib_var   <- wrap_all(afib_form_no_var, list("binomial"), FALSE)
all_result_diet       <- wrap_all(diet_form, list("binomial", "poisson", "normal", "normal"), FALSE)
all_result_diet_small <- wrap_all(diet_form_small, list("binomial", "poisson", "normal", "normal"), FALSE)
all_result_no_mscd    <- wrap_all(no_mscd_form, list("binomial", "poisson", "normal", "normal"), FALSE)
all_result_no_scd     <- wrap_all(no_scd_form, list("binomial", "poisson", "normal", "normal"), FALSE)

# We can also implement a washout period as such
result_diet_small <- wrap_single(diet_form_small, "daily_stool_frequency", "poisson", set_to_null = "5")
all_result_diet_small <- wrap_all(diet_form_small, list("binomial", "poisson", "normal", "normal"), set_to_null = list("5", "5", "2", "2"))
```

# Summarizing the simulations

Once we run the simulations, we can then summarize them to get some basic information.

```{r}
# Use the summarize_nof1 function to summarize the simulations
output_afib       <- summarize_nof1(result_afib$model_results, result_afib$system_info$treatments)
output_afib_var   <- summarize_nof1(result_afib_var$model_results, result_afib_var$system_info$treatments)
output_diet       <- summarize_nof1(result_diet$model_results, result_diet$system_info$treatments)
output_diet_small <- summarize_nof1(result_diet_small$model_results, result_diet_small$system_info$treatments)
output_no_mscd    <- summarize_nof1(result_no_mscd$model_results, result_no_mscd$system_info$treatments)
output_no_scd     <- summarize_nof1(result_no_scd$model_results, result_no_scd$system_info$treatments)

# We can then output the above to a .json format
output_afib <-
  toJSON(output_afib, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_afib_var <-
  toJSON(output_afib_var, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet <-
  toJSON(output_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet_small <-
  toJSON(output_diet_small, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_mscd <-
  toJSON(output_no_mscd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_scd <-
  toJSON(output_no_scd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)

# Use the summarize_all_nof1 function to summarize the the cases where we ran a model for each outcome
all_output_afib       <- summarize_all_nof1(all_result_afib, all_result_afib$system_info$treatments)
all_output_afib_var   <- summarize_all_nof1(all_result_afib_var, all_result_afib_var$system_info$treatments)
all_output_diet       <- summarize_all_nof1(all_result_diet, all_result_diet$system_info$treatments)
all_output_diet_small <- summarize_all_nof1(all_result_diet_small, all_result_diet_small$system_info$treatments)
all_output_no_mscd    <- summarize_all_nof1(all_result_no_mscd, all_result_no_mscd$system_info$treatments)
all_output_no_scd     <- summarize_all_nof1(all_result_no_scd, all_result_no_scd$system_info$treatments)

# We can then output the above to a .json format
all_output_afib <-
  toJSON(all_output_afib, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
all_output_afib_var <-
  toJSON(all_output_afib_var, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
all_output_diet <-
  toJSON(all_output_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
all_output_diet_small <-
  toJSON(all_output_diet_small, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
all_output_no_mscd <-
  toJSON(all_output_no_mscd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
all_output_no_scd <-
  toJSON(all_output_no_scd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
```

# Creating graphs and tables to visualze data

Graphs can also be used to analyze the data. There are several graphs that can be created both for the raw data and for the result data, which is created by our simulataions. Below is an example for each graph:

```{r}
# Use the function raw_graphs to create the graphs
raw_graphs("time_series_plot", all_result_diet_small, TRUE, "daily_stool_frequency", date_start = as.Date('2015-01-01'), date_end = as.Date('2016-01-31'))
raw_graphs("frequency_plot", result_afib)
raw_graphs("stacked_percent_barplot", all_result_diet_small, TRUE, "daily_stool_consistency")
raw_graphs("raw_table", all_result_afib, TRUE, "afib_episode_yn")

# Use the function result_graphs to create the graphs
result_graphs("kernel_plot", all_result_diet_small, outcome_name = "daily_stool_frequency")
result_graphs("kernel_plot", result_diet_small, multiple = FALSE, title = "daily_stool_frequency")
result_graphs("odds_ratio_plot", all_result_diet_small)
result_graphs("odds_ratio_plot", all_result_afib)
result_graphs("probability_barplot", all_result_diet_small)
result_graphs("probability_barplot", all_result_afib)
```

