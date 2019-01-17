#' time series plot across different interventions
#'
#' @param nof1 nof1 object created using nof1.data
#' @param timestamp time of the nof1 event occurring
#' @param timestamp.format format of the timestamp
#' @param Outcomes.name used to label y-axis outcome variable
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' timestamp <- seq(as.Date('2015-01-01'),as.Date('2016-01-31'), length.out = length(Y))
#' time_series_plot(nof1, timestamp = timestamp, timestamp.format = '%m-%d-%Y', Outcome.name = 'Stress')

# changed '#ffb380' to '#ff8080' and added '#80ff8c'. Issue was that there were
# not enough color options. Added na.rm=TRUE for geom_hline. number of categories
# is only used for ordinal data. so maybe time_series_plot only works for
# ordinal.
time_series_plot <- function(nof1, date_start = NULL, date_end = NULL, date.format = "%m/%d/%Y %H:%M",
  Outcome.name = "") {
  if (nof1$response %in% c("normal", "poisson")){
    period <- seq(date_start, date_end, length.out = length(nof1$Y))
    date <- as.Date(period, date.format)
    data <- data.frame(Y = nof1$Y, date = date, Treatment = factor(nof1$Treat, levels = c("baseline", "A", "B")))
    ggplot(data, aes(x = date, Y, color = Treatment)) + geom_point(na.rm = TRUE) +
      facet_wrap(.~ Treatment) + theme_bw() + scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38")) +
      labs(x = "Date", y = "Outcomes")
      # geom_smooth(method ="lm", na.rm = TRUE)
  }
  else{
    date <- as.Date(timestamp, timestamp.format)

    data <- data.frame(Y = as.numeric(nof1$Y), Treatment = gsub("\\_", " ", nof1$Treat),
      date = date)
    data2 <- aggregate(nof1$Y, list(Treatment = gsub("\\_", " ", nof1$Treat)), mean)

    ggplot(data,
      aes(x = date, Y, fill = Treatment)) +
      geom_bar(stat = "identity", na.rm = TRUE) + # use y-values for the height of the bar
      facet_grid(. ~ Treatment) + # form panels for each treatment
      theme_bw() + # makes theme black and white
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      labs(x = "Date", y = Outcome.name) +
      geom_hline(data = data2, aes(yintercept = x, linetype = "Mean"), color = "black") + # adds horizontal line in the data
      scale_y_continuous(breaks = 0:nof1$ncat, oob = rescale_none, label = c("Low", rep("", length = nof1$ncat - 1), "High")) +
      # above adds in the lines on the y-axis and the low and high notation
      scale_fill_manual(values = c("#adc2eb", "#ffb380")) +
      scale_linetype_manual(name = "", values = 1, guide = guide_legend(override.aes = list(color = c("black"))))
  }
}

#' Frequency plot for raw data
#'
#' @param nof1 nof1 object created using nof1.data
#' @param xlab x axis label
#' @param title The title of the figure
#' @param bins Used for continuous data. Specifies the number of bins. The
#' default value is 10.
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' frequency_plot(nof1)
frequency_plot <- function(nof1, xlab = "Outcomes", title = NULL, bins = 10) {

  if (nof1$response %in% c("binomial")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data = data, aes(x = factor(Y, levels = 0:1), y = x, fill = Treat)) + geom_bar(stat = "identity",
      position = "dodge", width = 0.8, na.rm = TRUE, alpha = 0.9) + labs(title = title,
      x = xlab, y = "Count", fill = "Treatment") + scale_x_discrete(labels = c("Low", "High")) +
      theme_bw()
  } else if (nof1$response %in% c("ordinal")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data = data, aes(x = Y, y = x, fill = Treat)) + geom_bar(stat = "identity",
      position = "dodge", na.rm = TRUE) + labs(title = title, x = xlab, y = "Count",
      fill = "Outcomes") + xlim(0.5, nof1$ncat + 0.5) + theme_bw()
  } else if (nof1$response %in% c("normal", "poisson")) {
    data <- data.frame(Y = nof1$Y, Treatment = nof1$Treat)
    ggplot(data, aes(x = Y, fill = Treatment, color = Treatment)) +
      # position dodge puts them next to each other, na.rm gets rid of NA data,
      # bins is, alpha is how see through the colors are
      geom_histogram(position = "dodge", na.rm = TRUE, bins = bins, alpha = 0.9) +
      labs(title = title, x = xlab, y = "Count") +
      theme_bw()
  }
}

#' Stacked_percent_barplot for raw data (for ordinal or binomial data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @param title The title of the figure
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' stacked_percent_barplot(nof1)
stacked_percent_barplot <- function(nof1, title = NULL) {

  if (nof1$response %in% c("binomial")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data, aes(fill = factor(Y, levels = 0:1, labels = c("Low", "High")), y = x, x = Treat)) + geom_bar(stat = "identity",
      position = "fill", na.rm = TRUE) + scale_y_continuous(labels = percent_format()) +
      theme_bw() + labs(title = title, x = "Treatment", y = "Percentage", fill = "Outcomes") +
      scale_fill_manual(values = c("light blue", "dark blue"))
  } else if (nof1$response %in% c("ordinal")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data, aes(fill = factor(Y, levels = 1:nof1$ncat), y = x, x = Treat)) +
      geom_bar(stat = "identity", position = "fill") + scale_y_continuous(labels = percent_format()) +
      theme_bw() + labs(title = title, x = "Treatment", y = "Percentage", fill = "Outcomes") +
      scale_fill_manual(values = 4:(3 + nof1$ncat), labels = 1:nof1$ncat, drop = FALSE)
  } else {
    stop("only works for binomial and ordinal data")
  }
}

#' Summary data table
#'
#' Provides a summary data table for the particular outcome in a particular dataset.
#'
#' @param nof1 nof1 object created using nof1.data
#' @return Gives a comprhensive table with several statistical values.
#' Each column indicates the value given. For a normal or poisson
#' response type the following are given:
#' \item{Treat}{The treatment recieved}
#' \item{mean}{The average value of the outcome}
#' \item{sd}{The standard deviation for the outcome}
#' \item{2.5\%}{2.5\% of the data are equal to or less than this value}
#' \item{50\%}{50\% of the data are equal to or less than this value}
#' \item{97.5\%}{97.5\% of the data are equal to or less than this value}
#' For a binomial or ordinal response type, the following are given:
#' \item{A}{Number of data points taken while in Treatment A phase
#' that have a value of 0 and 1, in the first and second row, respectivly}
#' \item{B}{Number of data points taken while in Treatment B phase
#' that have a value of 0 and 1, in the first and second row, respectivly}
#' \item{baseline}{Number of data points taken while in baseline phase
#' that have a value of 0 and 1, in the first and second row, respectivly}
raw_table <- function(nof1) {

  if (nof1$response %in% c("binomial", "ordinal")) {
    table(nof1$Y, nof1$Treat, useNA = "ifany")
  } else if (nof1$response %in% c("normal", "poisson")) {
    raw_table <- aggregate(nof1$Y, list(Treat = nof1$Treat), mean, na.rm = TRUE)
    colnames(raw_table)[2] <- "mean"
    cbind(raw_table, sd = aggregate(nof1$Y, list(Treat = nof1$Treat), sd, na.rm = TRUE)[,
      -1], aggregate(nof1$Y, list(Treat = nof1$Treat), quantile, na.rm = TRUE,
      c(0.025, 0.5, 0.975))[, -1])
  }
}

#' Kernel density estimation plot
#'
#' Creates a kernel density estimation plot for a specific outcome
#'
#' @param result An object with information about the simulation. The object is
#' derived from the output of nof1.run
#' @param bins The number of bins the histogram will contain. Default is 30.
#' @param x_max The upper limit of the x-axis. Default is to set the upper limit
#' to the maximum value of the data inputed
#' @param title The title of the graph
kernel_plot <- function(result, bins = 30, x_max = NULL, title = NULL) {
  samples <- do.call(rbind, result$samples)
  beta_variable <- exp(samples[, grep("beta", colnames(samples))])
  data <- as.data.frame(beta_variable)
  data <- melt(data, id.vars = NULL, variable.name = "beta", value.name = "odds_ratio")

  if (is.null(x_max)){
    x_max <- max(data$odds_ratio)
  }

  ggplot(data, aes(x = odds_ratio, color = beta)) + geom_density(na.rm = TRUE, size = 2) +
    geom_histogram(aes(y = ..density..), bins = bins, col = "gray", alpha = 0.7, na.rm = TRUE) + theme_bw() +
    facet_wrap(. ~ beta, scales = "free") + xlim(0, x_max) +
    labs(title = title, x = "Odds Ratio", y = "Density")
}


odds_ratio_plot <- function(result.list, result.name = NULL, level = 0.95, title = NULL) {

  odds_ratio <- matrix(NA, nrow = length(result.list), ncol = 3)

  for (i in 1:length(result.list)) {
    result <- result.list[[i]]
    samples <- do.call(rbind, result$samples)
    odds_ratio[i, ] <- exp(quantile(samples[, grep("beta", colnames(samples))],
      c((1 - level)/2, 0.5, 1 - (1 - level)/2)))
  }

  odds <- as.data.frame(odds_ratio)
  names(odds) <- c("lower", "OR", "upper")

  if (is.null(result.name)) {
    odds$vars <- row.names(odds)
  } else {
    if (length(result.name) != length(result.list)) {
      stop("result.name should have same length as result.list")
    }
    odds$vars <- result.name
  }
  ticks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)
  ggplot(odds, aes(y = OR, x = factor(vars))) + geom_point() + geom_errorbar(aes(ymin = lower,
    ymax = upper), width = 0.2) + scale_y_log10(breaks = ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype = 2) + coord_flip() + labs(x = "Variables",
    y = "Odds Ratio", title = title) + theme_bw()
}


probability_barplot <- function(result.list, result.name = NULL) {

  probability <- rep(NA, length(result.list) * 2)

  for (i in 1:length(result.list)) {
    result <- result.list[[i]]
    # print(str(result$samples))
    samples <- do.call(rbind, result$samples)
    # print(str(samples))
    probability[(i - 1) * 2 + 1] <- mean(exp(samples[, grep("beta", colnames(samples))]) >
      1)
    probability[i * 2] <- 1 - probability[(i - 1) * 2 + 1]
  }

  if (is.null(result.name)) {
    result.name <- rep(1:length(result.list), each = 2)
  } else {
    if (length(result.name) != length(result.list)) {
      stop("result.name should have same length as result.list")
    }
    result.name <- rep(result.name, each = 2)
  }
  # print(probability)
  # print(result.name)
  print((result.list[[1]]$nof1$Treat)[2])
  # print(rep(c(levels(result.list$result$nof1$Treat)[2],
              # levels(result.list$result$nof1$Treat)[1]), length(result.list)))

  data <- data.frame(probability = probability, result.name = result.name,
    Treat = rep(c(levels(result.list$result$nof1$Treat)[2],
    levels(result.list$result$nof1$Treat)[1]), length(result.list)))

  ggplot(data, aes(fill = factor(Treat), y = probability, x = result.name)) + geom_bar(stat = "identity",
    position = "fill") + scale_y_continuous(labels = percent_format()) + labs(x = "Variables",
    y = "Percentages", fill = "Treatment") + coord_flip() + theme_bw()
}

#' Raw Graphs and Tables
#'
#' \code{raw_graphs} allows the user to create a specific table or graph
#' for an outcome. Possible graphs include: time_series_plot, frequency_plot,
#' stacked_percent_barplot, and raw_table.
#'
#' @param graph A string which specifies the specific table or graph to construct.
#'  Options include: time_series_plot, frequency_plot, stacked_percent_barplot, and raw_table.
#' @param model_result The analysis we want to make the graphs for. It is
#' the output of \code{wrap_single}
#' @param multiple A boolean that indicates if the \code{model_result} input has multiple
#'  possible outcome. In other words, was \code{model_result} input created \code{\link{from wrap_all}}
#'  or \code{\link{wrap_single}}? The default is FALSE meaning we only have one outcome in the
#'  \code{model_result} input.
#' @param outcome_name A string that indicates the name of the outcome we wish to graph
#' @param ... Specific paramaters depending on table or graph to constuct
#' @examples
#' raw_graphs("raw_table", all_result_afib, TRUE, "afib_episode_yn")
#' raw_graphs("frequency_plot", result_afib)
#' @export
raw_graphs <- function(graph, model_result, multiple = FALSE, outcome_name = NULL,
                       date_start = NULL, date_end = NULL, date.format = "%m-%d-%Y",
                       xlab = "Outcomes", title = NULL, bins = 10) {

  if (!multiple) {
    nof1_out <- model_result[[2]][[1]]
  }
  else {
    if (is.null(outcome_name)) {
      return("Need to specify outcome name when have model_result input with multiple possible outcome")
    }
    else {
      nof1_out <- model_result[[2]][[as.name(outcome_name)]][[1]]
      title = outcome_name
    }
  }

  if (graph == "time_series_plot") {
    time_series_plot(nof1_out, date_start = date_start, date_end = date_end, date.format = date.format)
  } else if (graph == "frequency_plot") {
    frequency_plot(nof1_out, xlab, title, bins)
  } else if (graph == "stacked_percent_barplot") {
    stacked_percent_barplot(nof1_out, title)
  } else if (graph == "raw_table") {
    raw_table(nof1_out)
  } else {"Not a viable graph or table"}
}

result_graphs <- function(graph, model_result, multiple = TRUE, outcome_name = NULL,
                          result.name = NULL, title = NULL, bins = 30, x_max = NULL) {
  if (!multiple && graph == "kernel_plot"){
    kernel_plot(model_result[[2]][[2]], bins = bins, x_max = x_max, title = title)
  }
  else if (!multiple){
    "Can only create a kernel plot for one outcome"
  }
  else if (multiple && is.null(outcome_name)) {
    "Need to input an outcome name if have multiple results"
  }
  else{
    kern_out <- model_result[[2]][[as.name(outcome_name)]][[2]]
    if (is.null(outcome_name) && graph == "kernel_plot"){
      "Need to input the specific outcome for a kernel plot with multiple outcomes"
    }
    else if (graph == "kernel_plot"){
      kernel_plot(kern_out, bins = bins, x_max = x_max, title = outcome_name)
    }
    else if (graph == "odds_ratio_plot"){
      odds_ratio_plot(result.list, result.name = result.name, level = level, title = title)
    }
    else if (graph == "probability_barplot"){
      probability_barplot(result.list, result.name = result.name)
    } else {"Not a viable graph"}
  }
}

# data = dataset$data
# metadata = dataset$metadata
#
# # getting our data read and formated
# read_data <- tryCatch({
#   read_dummy <- formated_read_input(data, metadata)
#   if (metadata$washout == "FALSE" || is.null(metadata$washout)) {
#     read_dummy
#   } else {
#     read_dummy <- washout(read_dummy, metadata)
#     read_dummy
#   }
# }, error = function(error) {
#   return(paste("input read error: ", error))
# })
#
# # Define some variables to be used in constructing nof1 object
# names <- names(data)
# nof_responses <- length(data)
# read_len <- length(read_data)
# nof_treat <- length(unique(unlist(data[, 1]$treat)))
#
# if (graph == "kernel_plot") {
#   response_type = metadata[length(metadata) - nof_responses + kern_index]
#   data_out <- list(Treat = read_data[, kern_index]$treatment[[1]], Y = read_data[, kern_index]$result[[1]])
#   nof1_out <- with(data_out, {nof1.data(Y, Treat, response = response_type)})
#   result_out <- nof1.run(nof1_out)
#   # print(str(result_out))
#   kernel_plot(result_out, title = title)
# }
# else {
#   returns = list()
#   for (i in 1:nof_responses) {
#     returns[[i]] <- result_graphs_helper(read_data[, i], names[i], metadata[length(metadata) -
#                                                                               nof_responses + i], nof_treat)
#   }
#   names(returns) <- as.list(names)
#   # print(str(returns))
#   if (graph == "odds_ratio_plot") {
#     odds_ratio_plot(returns, result.name = result.name, title = title)
#   } else if (graph == "probability_barplot") {
#     probability_barplot(returns, result.name = result.name)
#   } else {"Not a viable graph or table"
#   }
# }
