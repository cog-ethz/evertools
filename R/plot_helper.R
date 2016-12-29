#' Normal Plots
#'
#' This function provides a normal Q-Q  and P-P plot with 
#' ggplot2 with labeling
#' 
#' @param samples Data points to be tested for normality.
#' @param variable.name Name of the variable which is ploted
#' @param scene.name Name of the scene that is ploted
#' @keywords EVE, evaluation, normality, normal plots, PP-plot, QQ-plot
#' @export
#' @return ggplot2 graph 
#' @examples 
#' paths <- get_participants_path_length_all(db,session.ids = session.ids, scene.ids = scene.ids)
#' normal_plots(paths$path_length[paths$scene_id==17],"Path Length","Training")
#' 
normal_plots <- function(samples, variable.name = "", scene.name = ""){
  
  #Additional data computation
  n <- length(samples)
  type_pp <- rep_len("PP-Plot",n)
  type_qq <- rep_len("QQ-Plot",n)
  sorted_sample <- sort(samples)
  sample_mean <- mean(samples)
  sample_sd <- sd(samples)
  
  #Data assembly into data frame
  QQ=data.frame(type=type_qq,
                empirical=sorted_sample,
                theorical=qnorm((1:n)/n,sample_mean,sample_sd))
  
  PP=data.frame(type = type_pp,
                empirical=pnorm(sorted_sample,sample_mean,sample_sd),
                theorical=(1:n)/n)
  
  probability_plots_data <- merge(QQ,PP, all = TRUE)
  
  #Plotting
  p <- ggplot(data=probability_plots_data,
         aes(y=empirical,x=theorical)) +
    geom_abline(intercept=0,
                slope=1,
                colour="red") +
    geom_point(alpha=0.3,
               shape=21,
               fill="blue",
               colour="black",
               size=1) +
    geom_smooth(method="lm",
                se=TRUE,
                alpha=0.3,
                formula=y ~ poly(x, 3, raw=TRUE))+
    facet_wrap(~type, scales = "free") +
    labs(title="Normal plots for" %++% variable.name %++% "in" %++% scene.name,
         x="Theoretical Expectation",
         y="Empirical Observation") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

#' Box Plot Variable
#'
#' This function provides a box plot of a variable.
#' 
#' It can be separated by scene and a controling factor
#' 
#' @param samples Data points to be qq plotted.
#' @param variable.name Name of the variable which is ploted
#' @param scene.name Name of the scene that is ploted
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and durations.
#' @examples
#' #Box plot without variable subgroupings
#' box_plot_variable(test, 
#'                   y.var = "path_length", 
#'                   x.var = "scene_name", 
#'                   grouping = "scene_id", 
#'                   y.name = "Path Length", 
#'                   x.name = "Scene", 
#'                   group.label = c("Training","City"))
#' #Box plot with variable subgroupings
#' box_plot_variable(test,
#'                   y.var = "path_length",
#'                   x.var = "scene_name",
#'                   grouping = "scene_id + sex",
#'                   coloring = "sex",
#'                   y.name = "Path Length",
#'                   x.name = "Scene",
#'                   group.label = c("Training","City"),
#'                   legend.name = "Sex",
#'                   legend.label = c("TRUE"="Female", "FALSE"="Male"))
#' 
box_plot_variable <- function(data = NULL, y.var = NULL, x.var = NULL, grouping = NULL, coloring = NULL,
                              y.name = "", x.name = "",group.label=NULL,legend.name= "", legend.label=NULL) {
  p <- ggplot(data = data) + 
    geom_boxplot( aes_string(y=y.var, x=x.var, group = grouping,color=coloring),na.rm = TRUE) +
    labs(title="Boxplot of" %++% y.name,       # plot title
         x=x.name,      # x-axis label
         y=y.name) +   # y-axis label
    scale_colour_discrete(name  =legend.name,
                          labels=legend.label) +
    scale_x_discrete(limits = group.label)+
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}