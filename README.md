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

The package itself contains six example data sets. Below is how we would run the simulations on each of the six datasets. If we wanted to run a new dataset we would simply need to extract the data from the file (ex. can use fromJSON() if it a .json file) and then we can use the gen_wrap function to run the analysis on the new data set.

```{r}
# Use the function gen_wrap to run the model
result_afib <-gen_wrap(afib_form)
result_afib_var <-gen_wrap(afib_form_no_var)
result_diet <-gen_wrap(diet_form)
result_diet_small <-gen_wrap(diet_form_small)
result_no_mscd <-gen_wrap(no_mscd_form)
result_no_scd <-gen_wrap(no_scd_form)
```

# Summarizing the simulations

Once we run the simulations, we can then summarize them to get some basic information like so.

```{r}
# Use the summarize_nof1 function to summarize the simulations
output_afib <- summarize_nof1(result_afib[[2]], result_afib[[3]], 2)
output_afib_var <- summarize_nof1(result_afib_var[[2]], result_afib_var[[3]], 2)
output_diet <- summarize_nof1(result_diet[[2]], result_diet[[3]], 3)
output_diet_small <- summarize_nof1(result_diet_small[[2]], result_diet_small[[3]], 3)
output_no_mscd <- summarize_nof1(result_no_mscd[[2]], result_no_mscd[[3]], 2)
output_no_scd <- summarize_nof1(result_no_scd[[2]], result_no_scd[[3]], 2)

# We can then output the above to a .json format
output_afib <-
  toJSON(result_afib, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_afib_var <-
  toJSON(result_afib_var, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet <-
  toJSON(result_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_diet_small <-
  toJSON(result_diet, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_mscd <-
  toJSON(result_no_mscd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output_no_scd <-
  toJSON(result_no_scd, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
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

