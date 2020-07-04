ggadd <- function(plot, ...)  {
  plot <- plot + ...
  plot
}

ggadd_aes <- function(plot, ...)  {
  plot <- plot + aes(...)
  plot
}

ggadd_geom <- function(plot, geom, ...)  {
  if (geom == "point") {plot + geom_point(...) -> plot}
  if (geom == "jitter") {plot + geom_jitter(...) -> plot}
  if (geom == "line") {plot + geom_line(...) -> plot}
  if (geom == "area") {plot + geom_area(...) -> plot}
  if (geom == "hist") {plot + geom_hist(...) -> plot}
  if (geom == "density") {plot + geom_density(...) -> plot}
  if (geom == "bar") {plot + geom_density(...) -> plot}
  if (geom == "col") {plot + geom_col(...) -> plot}
  if (geom == "boxplot") {plot + geom_boxplot(...) -> plot}
  if (geom == "smooth") {plot + geom_smooth(...) -> plot}  
  plot
}

ggadd_title <- function(plot, title, center = FALSE, color, size, bold = FALSE)  {
  
  plot + ggtitle(title) -> plot
  
  if (!missing(color)) {
    plot + theme(plot.title = element_text(colour = color)) -> plot
  }
  
  if (!missing(size)) {
    plot + theme(plot.title = element_text(size = size)) -> plot
  }
  
  if (center)  {
    plot + theme(plot.title = element_text(hjust = 0.5)) -> plot
  }
  
  if (bold)  {
    plot + theme(plot.title = element_text(face = "bold")) -> plot
  }
  
  plot
}

ggadd_subtitle <- function(plot, subtitle, center = FALSE, color, size, bold = FALSE)  {

    plot + labs(subtitle = subtitle) -> plot
  
  if (!missing(color)) {
    plot + theme(plot.subtitle = element_text(colour = color)) -> plot
  }
  
  if (!missing(size)) {
    plot + theme(plot.subtitle = element_text(size = size)) -> plot
  }
  
  if (center)  {
    plot + theme(plot.subtitle = element_text(hjust = 0.5)) -> plot
  }
  
  if (bold)  {
    plot + theme(plot.subtitle = element_text(face = "bold")) -> plot
  }
  
  plot
}

ggadd_caption <- function(plot, caption, center = FALSE, color, size, bold = FALSE)  {
  
  plot + labs(caption = caption) -> plot
  
  if (!missing(color)) {
    plot + theme(plot.caption = element_text(colour = color)) -> plot
  }
  
  if (!missing(size)) {
    plot + theme(plot.caption = element_text(size = size)) -> plot
  }
  
  if (center)  {
    plot + theme(plot.caption = element_text(hjust = 0.5)) -> plot
  }
  
  if (bold)  {
    plot + theme(plot.caption = element_text(face = "bold")) -> plot
  }
  
  plot
}

ggadd_x <- function(plot, lab, color, lim)  {
  if (!missing(lab)) {plot + xlab(lab) -> plot}
  if (!missing(lim)) {plot + xlim(lim) -> plot}
  if (!missing(color)) {plot + theme(axis.title.x = element_text(colour = color)) -> plot}
  plot
}

ggadd_y <- function(plot, lab, color, lim)  {
  if (!missing(lab)) {plot + ylab(lab) -> plot}
  if (!missing(lim)) {plot + ylim(lim) -> plot}
  if (!missing(color)) {plot + theme(axis.title.y = element_text(colour = color)) -> plot}
  plot
}

ggadd_legend <- function(plot, show = TRUE, position = "right", title_color, title_fill, title_size, title_shape)  {
  if (!show) {position = "none"}
  plot + theme(legend.position = position) -> plot
  if (!missing(title_color)) {plot + labs(color = title_color) -> plot}
  if (!missing(title_fill)) {plot + labs(fill = title_fill) -> plot}
  if (!missing(title_size)) {plot + labs(size = title_size) -> plot}
  if (!missing(title_shape)) {plot + labs(shape = title_shape) -> plot}
  plot
}

ggadd_theme <- function(plot, theme = "light")  {
  if (theme == "void") {plot + theme_void() -> plot}
  if (theme == "minimal") {plot + theme_minimal() -> plot}
  if (theme == "classic") {plot + theme_classic() -> plot}
  if (theme == "bw") {plot + theme_bw() -> plot}
  if (theme == "grey") {plot + theme_grey() -> plot}
  if (theme == "light") {plot + theme_light() -> plot}
  plot
}

ggadd_flip <- function(plot, flip = TRUE)  {
  if (flip) {plot + coord_flip() -> plot}
  plot
}

ggadd_line <- function(plot, xintercept, yintercept, color = "black", dashed = FALSE, size = 1, alpha = 0.75)  {
  
  if (dashed) {
    linetype = "dashed"
  } else {
    linetype = "solid"
  }
  
  if (!missing(yintercept)) {
    plot <- plot + geom_hline(yintercept = yintercept, color = color, alpha = alpha, linetype = linetype, size = size)
  }
  if (!missing(xintercept)) {
    plot <- plot + geom_vline(xintercept = xintercept, color = color, alpha = alpha, linetype = linetype, size = size)
  }
  plot
}

ggadd_text <- function(plot, x, y, text, color = "black", size = 4, alpha = 0.75) {
  plot + annotate("text", x = x, y = y, label = text, color = color, size = size, alpha = alpha) -> plot
  plot
}

ggadd_facet <- function(plot, ...)  {
  plot + facet_wrap(...) -> plot
  plot
}

iris %>% 
  ggplot() %>% 
  ggadd_theme() %>% 
  ggadd_aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Width) %>% 
  ggadd_geom("point", alpha = 0.50) %>% 
  ggadd_title("Iris", color = "grey", size = 20, center = TRUE, bold = TRUE) %>% 
  ggadd_subtitle("Blumen Daten", color = "grey", size = 20, center = TRUE, bold = TRUE) %>% 
  ggadd_caption("Figure 1.10", bold = TRUE) %>% 
  ggadd_x(lab = "Sepal Length", color = "blue", lim = c(4,5)) %>% 
  ggadd_y(lab = "Sepal Width", color = "pink", lim = c(0,5)) %>% 
  ggadd_legend(show = TRUE, position = "right", title_color = "Blumenart", title_size = "Huhu") %>% 
  ggadd_line(yintercept = 4.5, dashed = FALSE, color = "red", size = 1) %>% 
  ggadd_text(4.5, 4.5, "nicht weiter nach oben", color = "black", size = 4) %>% 
  ggadd_facet(~Species) %>% 
  ggadd_flip()
   


