# plot_bar <- function(variables) {
#   
#   
# 
#   
#   
#   library(ggplot2)
#   
#   create_bar_plot <- function(data, y_col, x_col, facet_col, fill_col, palette_name, legend_title) {
#     
#     ggplot(data, aes(x = {{x_col}}, y = {{y_col}}, fill = {{fill_col}})) +
#       geom_bar(stat = "identity") +
#       labs(x = deparse(substitute(x_col)),
#            y = deparse(substitute(y_col)),
#            fill = legend_title) +
#       scale_fill_brewer(palette = palette_name, name = legend_title) +
#       facet_wrap(~ eval(facet_col), ncol = 1)
#     
#   }
#   
#   # Create example data
#   df <- data.frame(x = c("A", "B", "C", "D"),
#                    y = c(10, 20, 30, 40),
#                    fill_col = c("red", "green", "blue", "orange"),
#                    facet_col = rep(c("group1", "group2"), 2))
#   
#   # Create the plot
#   create_bar_plot(
#     data = df,
#     y_col = y,
#     x_col = x,
#     facet_col = facet_col,
#     fill_col = fill_col,
#     palette_name = "Set1",
#     legend_title = "Color Category"
#   )
#   
#   
#   
# }


# 
# path_to_har <- gtaptools::templates("oranig_example.har")
# 
# input_data <- HARr::read_har(path_to_har, toLowerCase = F)
# 
# input_data <- as.data.frame.table(input_data$`1MAR`)
# 
# input_data <- input_data |> 
#   dplyr::filter(COM %in% c("Agriculture", "Livestock", "Extractive"))
# 
# 
# 
# library(ggplot2)
# 
# create_stacked_bar_plot <- function(data, y_col, x_col, facet_col, stack_col, palette_name, legend_title) {
#   
#   ggplot(data, aes(x = {{x_col}}, y = {{y_col}}, fill = {{stack_col}})) +
#     geom_bar(stat = "identity", position = "stack") +
#     labs(x = deparse(substitute(x_col)),
#          y = deparse(substitute(y_col)),
#          fill = legend_title) +
#     scale_fill_brewer(palette = palette_name, name = legend_title) +
#     facet_wrap(vars({{facet_col}}))
#   
# }
# 
# 
# 
# # Create example data
# df <- data.frame(x = c("A", "B", "B", "A"),
#                  y = c(10, 20, 30, 40),
#                  stack_col = rep(c("group1", "group2"), 2),
#                  facet_col = rep(c("category1", "category2"), 2))
# 
# # Create the plot
# create_stacked_bar_plot(df, y_col = y, x_col = x, facet_col = facet_col, stack_col = stack_col, 
#                         palette_name = "Set1", legend_title = "Stack Category")
# 
# 
# create_stacked_bar_plot(input_data, y = Freq, x = MAR,  facet_col = SRC, stack_col = COM,
#                         palette_name = "Set1", legend_title = "KK")

