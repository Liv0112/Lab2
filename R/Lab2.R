library(devtools)
library(roxygen2)
library(readr)
library(tidyverse)
library(ggplot2)
DRG <- read.csv("/Users/wyw/Desktop/Cornell/Data_Science_1/Assignments/Homework_3/DRG_data.csv")
DRG <- DRG %>%
  mutate(DRG.code = substr(DRG.Definition,1,3)) %>%
  select(DRG.code, everything())

get_DRG <- function(){
  DRG
}

# Payments = 1 or 2
# 1 for Average Medicare Payments (default)
# 2 for Average Total Payments
#' Title
#'
#' @param pmt
#'
#' @return plot
#' @export
#'
#' @examples
plot_DRG <- function(pmt = 1){
  g <- DRG %>%
    ggplot() +
    scale_y_continuous(trans = "log10") +
    xlab("DRG Code") +
    theme(axis.text.x = element_text(size = 6, angle = 90),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12))
  if(pmt == 1){
    g + geom_boxplot(aes(x = DRG.code, y = log(Average.Medicare.Payments)), outlier.shape = NA) +
      ggtitle("Average Medicare Payments by DRG codes") + ylab("Average Medicare Payments ($)")
  } else if(pmt == 2){
    g + geom_boxplot(aes(x = DRG.code, y = log(Average.Total.Payments)), outlier.shape = NA) +
      ggtitle("Average Total Payments by DRG codes") + ylab("Average Total Payments ($)")
  }
}
plot_DRG(1)

calculate_DRG <- function(type) {
  DRG_spread <- DRG %>%
    select(DRG.code, Provider.Id, Provider.State, Average.Medicare.Payments) %>%
    spread(DRG.code, Average.Medicare.Payments)
  switch(type,
         mean = apply(DRG_spread[,3:102], MARGIN = 2, mean, na.rm = T),
         median = apply(DRG_spread[,3:102], MARGIN = 2, median, na.rm = T),
         sd = apply(DRG_spread[,3:102], MARGIN = 2, sd, na.rm = T))
}
calculate_DRG("mean")



