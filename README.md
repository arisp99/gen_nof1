# N-Of-1 (Single Subject Design)

This repo is a based of another repo by MikeJSeo, which introduces a nof1 package that can be used to analyze nof1 studies. The goal in this repo is to take the package that was created and generalize it to work for any nof1 study.

# To Install and Load the original nof1 package

```{r}
install.packages("devtools")
devtools::install_github("arisp99/nof1gen")
library(nof1gen)
```

# Format of the data file

The file must contain two parts, the data section and the metadata section. The data section will contain all the observations taken during the trial. For each observation, we will have a list of the treatment the patient was on at the time followed by a list of all the data points collected. These two lists should be the same length. The metadata section will then contain the  user_id, the trigger, the design of the trial, the start and end  date (YYYY-MM-DD), followed by the response type of each of the observations. Either normal, binomial, poisson, or ordinal. The response types must be in the same order as the observations in the data section and must be the last pieces of information in the metadata section. Refer to afib_form.json and diet_form.json for two examples.

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
```

# Summarizing the simulations

Once we run the simulations, we can then summarize them to get some basic information.

```{r}
# Use the summarize_nof1 function to summarize the simulations
output_afib <- summarize_nof1(result_afib, 2)
output_afib_var <- summarize_nof1(result_afib_var, 2)
output_diet <- summarize_nof1(result_diet, 3)
output_diet_small <- summarize_nof1(result_diet_small, 3)
output_no_mscd <- summarize_nof1(result_no_mscd, 2)
output_no_scd <- summarize_nof1(result_no_scd, 2)

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
all_output_afib <- summarize_all_nof1(all_result_afib, 2)
all_output_afib_var <- summarize_all_nof1(all_result_afib_var, 2)
all_output_diet <- summarize_all_nof1(all_result_diet, 3)
all_output_diet_small <- summarize_all_nof1(all_result_diet_small, 3)
all_output_no_mscd <- summarize_all_nof1(all_result_no_mscd, 2)
all_output_no_scd <- summarize_all_nof1(all_result_no_scd, 2)

# We can then output the above to a .json format
output_afib <-
  toJSON(all_output_afib, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_afib_var <-
  toJSON(all_output_afib_var, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet <-
  toJSON(all_output_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet_small <-
  toJSON(all_output_diet_small, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_mscd <-
  toJSON(all_output_no_mscd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_scd <-
  toJSON(all_output_no_scd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
```

# Creating graphs and tables to visualze data

Graphs can also be used to analyze the data. There are several graphs that can be created both for the raw data and for the result data, which is created by our simulataions. At the time of this update, it is only possible to create the raw data graphs and tables. There are four graphs that can be constructed. Below is an example for each one:

```{r}
# Use the function make_raw_graphs to creat the graphs
raw_graphs("time_series_plot", diet_form, 1) #N.B. time_series_plot only works for ordinal data as of now. It may also be able to eventaully work for binomial data.
raw_graphs("frequency_plot", diet_form, 2)
raw_graphs("stacked_percent_barplot", diet_form, 1)
raw_graphs("raw_table", diet_form, 2)
```

