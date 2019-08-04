library(plotly)

step <- 1
x <- seq(0,10, length.out = 1000)
y <- sin(step*x)

# create data
# aval <- list()
# for(step in 1:11){
#     aval[[step]] <-list(visible = FALSE,
#                         name = paste0('v = ', step),
#                         x=x,
#                         y=sin(step*x))
# }
# aval[3][[1]]$visible = TRUE

# create steps and plot all traces
#steps <- list()
p <- plot_ly() %>%
    add_lines(x=x,
              y=y,
              name = 'test',
              type = 'scatter',
              mode = 'lines',
              hoverinfo = 'name',
              line=list(color='00CED1'),
              showlegend = FALSE)

    #step <- list(args = list('visible', rep(FALSE, length(aval))),
     #            method = 'restyle')
    #step$args[[2]][i] = TRUE
    #steps[[i]] = step
#}

# add slider control to plot
# p <- p %>%
#     layout(sliders = list(list(currentvalue = list(prefix = "Frequency: "),
#                                steps = x)))
#
# p
