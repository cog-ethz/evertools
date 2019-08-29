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
#' It can be separated by scenes and controling factors.
#'
#' @param data Data points to be plotted.
#' @param y.var The variable to be plotted along the y-axis.
#' @param x.var The variable to be plotted along the x-axis.
#' @param coloring The colors for subgroups.
#' @param y.name Name of the y variable in the plot.
#' @param x.name Name of the x variable in the plot.
#' @param legend.name Title of the legend.
#' @param legend.label New labels for the legend (optional).
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and durations.
#' @examples
#' #Box plot without variable subgroupings
#' box_plot_variable(test,
#'                   y.var = "path_length",
#'                   x.var = "scene_name",
#'                   y.name = "Path Length",
#'                   x.name = "Scene",
#'                   group.label = c("Training","City"))
#' #Box plot with variable subgroupings
#' box_plot_variable(test,
#'                   y.var = "path_length",
#'                   x.var = "scene_name",
#'                   coloring = "sex",
#'                   y.name = "Path Length",
#'                   x.name = "Scene",
#'                   group.label = c("Training","City"),
#'                   legend.name = "Sex",
#'                   legend.label = c("TRUE"="Female", "FALSE"="Male"))
#'
box_plot_variable <- function(data = NULL, y.var = NULL, x.var = NULL, coloring = NULL,
                              y.name = "", x.name = "", group.label=NULL, legend.name= "", legend.label=NULL) {
  p <- ggplot(data = data) +
    geom_boxplot( aes_string(y=y.var, x=x.var, color=coloring),na.rm = TRUE) +
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

#' Box Plot Variable
#'
#' This function provides a box plot of a variable.
#'
#' It can be separated by scene and a controling factor
#'
#' @param data Data points to be qq plotted.
#' @param bin.width Used to compute density.
#' @param alpha How transparent the curve is.
#' @param x.var Variable to be histogrammed.
#' @param x.name Variable name to be shown.
#' @param grouping How to group distributions.
#' @param legend.name Title of the legend.
#' @param legend.label New labels for the legend (optional).
#' @param condition.name Title of the graph addition.
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and durations.
#' @examples
#' #Box plot without variable subgroupings
#' histogram_density_plot_variable(test,
#'                   bin.width = 50,
#'                   alpha = 0.2,
#'                   x.var = "path_length",
#'                   x.name = "Path Length")
#' #Box plot with variable subgroupings
#' histogram_density_plot_variable(test,
#'                   bin.width = 50,
#'                   alpha = 0.2,
#'                   x.var = "path_length",
#'                   x.name = "Path Length",
#'                   grouping = "sex",
#'                   legend.name = "Sex
#'                   legend.labels = c("TRUE"="Female", "FALSE"="Male"))
#'
histogram_density_plot_variable <- function(data = NULL, bin.width = 50, alpha = 0.2, x.var = NULL, x.name = "",
                                            grouping = NULL, legend.name= "", legend.labels=NULL,
                                            condition.name = "") {
  p <- ggplot(data=data) +
    geom_histogram(binwidth = bin.width,
                   position = "identity",
                   alpha = alpha,
                   aes_string(x = x.var,
                              y = "..density..",
                              color = grouping,
                              fill = grouping)) +
    geom_density(aes_string(x = x.var,
                            color = grouping))+
    geom_density(aes_string(x=x.var))+
    labs(title = x.name %++% " Histogram and Density " %++% condition.name,
         x = x.name,
         y = "Density") +
    scale_colour_discrete(name  =legend.name,
                          labels = legend.labels)+
    scale_fill_discrete(name = legend.name,
                        labels = legend.labels)+
    scale_alpha_continuous(guide=FALSE)+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}


#' Plot All Participants By Map
#'
#' This function plots all participants path for a single
#' scene.
#'
#' @param db dbyplr database handle
#' @param session.ids Sessions to be loaded
#' @param scene.name Name of the scene that is ploted
#' @keywords EVE, evaluation, top-down mal, paths, visualisation
#' @export
#' @return ggplot2 graph of paths in scene
#' @examples
#' plot_all_participants_by_map(db, session.ids, "The_Viking_Village", alpha = 0.5, dataPath = unityDataPath)
#'
plot_all_participants_by_map <- function(db, session.ids = c(1), scene.name = "The_Viking_Village", alpha = 1, dataPath ="", width=2048,height=2048){

  projection <- matrix(as.numeric(as.vector(xmlToDataFrame(paste0(unityDataPath,scene.name,"_projectionMatrix.xml"))$text)),nrow = 4)
  worldToCamera <- matrix(as.numeric(as.vector(xmlToDataFrame(paste0(unityDataPath,scene.name,"_worldToCameraMatrix.xml"))$text)),nrow = 4)

  basemap <- png::readPNG(paste0(unityDataPath,scene.name,"_",width,"x",height,".png"))
  if (alpha < 1){
  basemap <- matrix(rgb(basemap[,,1],basemap[,,2],basemap[,,3], 0.5), nrow=dim(basemap)[1])
  }

  position_data <- get_participants_positions_XYZ(db = db, session.ids = session.ids, scene.name = scene.name)
  position_data <- transform_3d_positions_to_image(position_data = position_data, projection, worldToCamera)

  col <- as.vector(viridis::plasma(length(session.ids)))

  p <- ggplot(data=position_data, aes(x,y, colour=factor(position_data$session_id)))+
      annotation_custom(rasterGrob(basemap, width = unit(1,"npc"), height = unit(1,"npc")),
                        -1, 1, -1, 1)+
      geom_path(size = 1, lineend = "round")+
      scale_colour_manual(values = col, name="Sessions") + xlim(-0.9,0.9 ) + ylim(-0.9,0.9) + coord_fixed()

  return (p)
}
