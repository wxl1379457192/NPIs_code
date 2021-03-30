
library(reshape2)
library(ggplot2)
library(patchwork)


gather <- read.csv("Gather_dataset.csv")
gather$New_cases <- gather$New_cases/gather$Pop*1000000
gather$New_cases_smoothed <- gather$New_cases_smoothed/gather$Pop*1000000

variables <- c("day", "Y1", "Y2")

cases_plot <- lapply(split(gather, gather$Country), function(m) {
  waves <- max(m$wave)
  position <- lapply(split(m, m$wave), function(v) {
    c(v$day[1], v$day[nrow(v)])
  })
  names <- m$Country[1]
  ifelse(
    waves == 1,
    m <- ggplot(
      melt(as.data.frame(m[, variables]), id = "day"),
      aes(x = day, y = value, color = variable)
    )  +
      labs(
        x = NULL,
        y = NULL,
        title = names
      ) +
      geom_rect(
        aes(
          xmin = position$'1'[1],
          xmax = position$'1'[2],
          ymin = -Inf,
          ymax = Inf
        ),
        fill = '#ffcccc',
        color = '#ffcccc',
        alpha = 0.01
      ) +
      geom_line() + 
      scale_color_manual(values=c('#808080','#0058db')) +
      theme_classic() + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.2, size = 8)),
    ifelse(
      waves == 2,
      m <- ggplot(
        melt(as.data.frame(m[,variables]), id = "day"),
        aes(x = day, y = value, color = variable)
      )  +
        labs(
          x = NULL,
          y = NULL,
          title = names
        ) +
        geom_rect(
          aes(
            xmin = position$'1'[1],
            xmax = position$'1'[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = '#ffcccc',
          color = '#ffcccc',
          alpha = 0.01
        ) +
        geom_rect(
          aes(
            xmin = position$'2'[1],
            xmax = position$'2'[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = '#ffcccc',
          color = '#ffcccc',
          alpha = 0.01
        ) +
        geom_line() + 
        scale_color_manual(values=c('#808080','#0058db')) +
        theme_classic() + 
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.2, size = 8)),
      m <- ggplot(
        melt(as.data.frame(m[, variables]), id = "day"),
        aes(x = day, y = value, color = variable)
      )  +
        labs(
          x = NULL,
          y = NULL,
          title = names
        ) +
        geom_rect(
          aes(
            xmin = position$'1'[1],
            xmax = position$'1'[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = '#ffcccc',
          color = '#ffcccc',
          alpha = 0.01
        ) + 
        geom_rect(
          aes(
            xmin = position$'2'[1],
            xmax = position$'2'[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = '#ffcccc',
          color = '#ffcccc',
          alpha = 0.01
        ) +
        geom_rect(
          aes(
            xmin = position$'3'[1],
            xmax = position$'3'[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = '#ffcccc',
          color = '#ffcccc',
          alpha = 0.01
        ) + 
        geom_line() + 
        scale_color_manual(values=c('#808080','#0058db')) +
        theme_classic() + 
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.2, size = 8))
    )
  )
  return(m)
})

for (variable in 0:7) {
  cases_plot_temp <- cases_plot[variable*20+1:20]
  ggsave(paste0("smooth/",variable,".jpg"),wrap_plots(cases_plot_temp[which(!sapply(cases_plot_temp, is.null))]))
}
