#' ggplot2 QQ
#'
#' This function provides a normal Q-Q plot with ggplot2.
#' 
#' The code is originially from here:
#' http://stackoverflow.com/a/4357932/6260885
#' @param samples Data points to be qq plotted.
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and durations.
#' @examples 
#' ggplot2_QQ(lm)
#' paths <- get_participants_path_length_all(db, session.ids = c(1:8), scene.ids = c(0:3))
#' 
ggplotQQ <- function (samples) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(samples[!is.na(samples)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = samples)
  p <- ggplot(data=d, aes(sample=samples)) +
    stat_qq(shape=1, size=3) +           # open circles
    labs(title="Normal Q-Q Plot",             # plot title
         x="Theoretical Quantiles",      # x-axis label
         y="Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype="dashed") +
    theme_bw()# dashed reference line
  return(p)
}