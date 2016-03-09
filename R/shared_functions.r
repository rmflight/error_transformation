#' plot differences in range
#'
#' @param org_data the original data
#' @param log_mean should plot with log-transformed mean on x-axis?
#'
#' @export
#'
#' @return plot
#' @import dplyr
#' @import ggplot2
plot_diff <- function(org_data, log_mean = log1p){

  # calculate the range between lowest and highest value of each feature
  diff_values <- apply(org_data, 1, function(x){max(x) - min(x)})
  diff_frame <- data.frame(diff = diff_values, mean = rowMeans(org_data))

  # how to label the x-axis
  use_mean <- "mean"
  x_title <- use_mean

  # if using log-mean, then transform
  if (is.function(log_mean)) {
    use_mean <- "log_mean"
    x_title <- "log(mean)"
    diff_frame <- mutate(diff_frame, log_mean = log1p(mean))
  }

  # generate and return the plot
  diff_plot <- ggplot(diff_frame, aes_string(x = use_mean, y = "diff")) +
    geom_point() + ggtitle("Difference vs Mean") + xlab(x_title)
  return(diff_plot)
}

#' sd and rsd plot
#'
#' @param org_data the original data as matrix
#' @param log_mean should the mean on x-axis be log-transformed
#' @param sd_type plot the SD or RSD?
#'
#' @export
#' @return plot
#' @importFrom visualizationQualityControl summarize_data
plot_sd <- function(org_data, log_mean = log1p, sd_type = "sd"){
  # summarize the data
  summ_data <- summarize_data(t(org_data), log_transform = log_mean)

  # choose whether to transform the mean or not
  use_mean <- "mean"
  x_title <- use_mean

  if (is.function(log_mean)) {
    use_mean <- "log_mean"
    x_title <- "log(mean)"
  }

  # filter based on type of SD and plot
  sd_plot <- filter(summ_data, type == sd_type) %>%
    ggplot(., aes_string(x = use_mean, y = "var")) + geom_point() +
    ggtitle("Mean vs SD") + xlab(x_title) + ylab(sd_type)
  return(sd_plot)
}

#' generate histogram
#'
#' @param in_data original data
#' @param bins how many bins to use
#'
#' @export
#' @return plot
plot_hist <- function(in_data, bins = 100){
  plot_frame <- data.frame(abundance = as.vector(in_data))
  ggplot(plot_frame, aes(x = abundance)) + geom_histogram(bins = bins)
}

#' root transform the data
#'
#' @param in_data data to transform
#' @param root what root to use in the transformation
#'
#' @export
#' @return matrix
root_transform <- function(in_data, root = 2){
  root_data <- in_data ^ (1/root)
  root_data
}

#' filter data
#'
#' @param in_data original input data
#' @param min_value what minimum value to filter out
#'
#' @export
#' @return data.frame
filter_features <- function(in_data, min_value){
  n_rep <- ncol(in_data)
  pass_filter <- apply(in_data, 1, function(x){
    sum(x >= min_value) == n_rep
  })
  out_data <- in_data[pass_filter, ]
  out_data
}

#' apply auto scaling
#'
#' auto scaling scales by subtracting the mean and dividing by the standard
#' deviation
#'
#' @param in_data the matrix of original data
#' @param sub_mean should mean subtraction be performed
#'
#' @export
#' @return matrix
auto_scale <- function(in_data, sub_mean = FALSE){
  if (sub_mean) {
    out_val <- t(apply(in_data, 1, function(x){x - mean(x)}))
  } else {
    out_val <- in_data
  }

  out_val <- t(apply(out_val, 1, function(x){x / sd(x)}))
  out_val
}

#' apply pareto scaling
#'
#' auto scaling scales by subtracting the mean and dividing by the standard
#' deviation
#'
#' @param in_data the matrix of original data
#' @param sub_mean should mean subtraction be performed
#'
#' @export
#' @return matrix
pareto_scale <- function(in_data, sub_mean = FALSE){

  if (sub_mean) {
    out_val <- t(apply(in_data, 1, function(x){x - mean(x)}))
  } else {
    out_val <- in_data
  }

  out_val <- t(apply(out_val, 1, function(x){x / sqrt(sd(x))}))
  out_val
}

#' get sd or rsd from two datasets
#'
#' @param data1 the first dataset
#' @param data2 the second dataset
#'
#' @export
#' @return data.frame
summarize_two <- function(data1, data2, sd_type = "sd", name1 = "data1",
                          name2 = "data2"){
  data1_summ <- summarize_data(t(data1)) %>% filter_(., ~type == sd_type) %>%
    mutate(., data = name1)
  data2_summ <- summarize_data(t(data2)) %>% filter_(., ~type == sd_type) %>%
    mutate(., data = name2)
  out_data <- rbind(data1_summ, data2_summ)
  out_data
}
