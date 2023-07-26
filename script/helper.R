theme_jhchae <- function(font = NULL, fontsize = 15) {
  
  if (is.null(font)) {
    theme_bw(base_size = fontsize) +
      theme(legend.position = "top", panel.grid.minor = element_blank()) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = .3, colour = 'grey90'),
            plot.title = element_text(face = "bold"),
            axis.ticks = element_line(linewidth = .3, colour = "black"),
            axis.text = element_text(colour = 'black', size = rel(.8)),
            axis.title = element_text(colour = 'black', size = rel(.8)),
            strip.text = element_text(colour = 'black', size = rel(.8)),
            strip.background = element_rect(fill = "grey90"),
            plot.caption = element_text(hjust = 0))
  } else {
    theme_bw(base_size = fontsize, base_family = font) +
      theme(legend.position = "top", panel.grid.minor = element_blank()) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = .3, colour = 'grey90'),
            plot.title = element_text(face = "bold"),
            axis.ticks = element_line(linewidth = .3, colour = "black"),
            axis.text = element_text(family = font, colour = 'black', size = rel(.8)),
            axis.title = element_text(family = font, colour = 'black', size = rel(.8)),
            strip.text = element_text(family = font, colour = 'black', size = rel(.8)),
            strip.background = element_rect(fill = "grey90"),
            plot.caption = element_text(hjust = 0))
  }
  
}

# From: https://github.com/stefano-meschiari/latex2exp/issues/11
set.seed(50389082) # least solution to Archimedes' cattle problem. good to know.
latex_lines <- function(each_line) { # spread each entry in each_line onto a different line using overset, wrapping each line in normalsize
  if ("character" != class(each_line)) {
    stop("latex_lines expects a character vector")
  }
  ret <- paste0("\\normalsize{", each_line[1], "}")
  while(0 != length(each_line <- tail(each_line, n=-1))) {
    ret <- paste0("\\overset{", ret, "}{\\normalsize{", each_line[1],"}}")
  }
  return(ret)
}