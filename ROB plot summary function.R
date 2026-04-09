
Rob_summary3 <- function(data, tool = "ROB2", overall = FALSE, weighted = TRUE, colour = "cochrane", quiet = FALSE) {
  judgement <- NULL
  Weights <- NULL
  domain <- NULL
  
  if (tool == "ROB2") {
    if (length(colour) > 1) {
      low_colour <- colour[1]
      concerns_colour <- colour[2]
      high_colour <- colour[3]
    } else {
      if (colour == "colourblind") {
        low_colour <- "#fee8c8"
        concerns_colour <- "#fdbb84"
        high_colour <- "#e34a33"
      }
      if (colour == "cochrane") {
        low_colour <- "#02C100"
        concerns_colour <- "#E2DF07"
        high_colour <- "#BF0000"
      }
    }
    for (i in 2:9) {
      data[[i]] <- trimws(tolower(data[[i]]))
      data[[i]] <- dplyr::recode(data[[i]],
                                 "y" = "l",   # Y = Low risk
                                 "m" = "s",   # M = Some concerns
                                 "n" = "h")   # N = High risk
    }
    if (!weighted) {
      data[, 10] <- rep(1, nrow(data))
    } else {
      if (NCOL(data) < 10) {
        stop("Column missing (number of columns < 10). Likely that a column detailing weights for each study is missing.")
      }
    }
    
    data.tmp <- data
    names(data.tmp)[2] <- "Was the study’s target population a close representation of the national population in relation to relevant variables?"
    names(data.tmp)[3] <- "Was some form of random selection used to select the sample, or was a census undertaken?"
    names(data.tmp)[4] <- "Was the likelihood of nonresponse bias minimal?"
    names(data.tmp)[5] <- "Were data collected directly from the subjects (as opposed to a proxy)?"
    names(data.tmp)[6] <- "Was an acceptable case definition used in the study?"
    names(data.tmp)[7] <- "Was the study instrument that measured the parameter of interest shown to have validity and reliability?"
    names(data.tmp)[8] <- "Was the same mode of data collection used for all subjects?"
    names(data.tmp)[9] <- "Were the numerator(s) and denominator(s) for the parameter of interest appropriate?"
    names(data.tmp)[10] <- "Weights"
    

    if (!overall) {
      data.tmp <- data.tmp[, c(2:9, 10)]
    } else {
      data.tmp <- data.tmp[, 2:10]
    }
    
    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp, domain, judgement, -Weights))
    rob.tidy$domain <- factor(rob.tidy$domain, levels = rev(unique(rob.tidy$domain)))
    rob.tidy$domain <- stringr::str_wrap(rob.tidy$domain, width = 50)
    
    rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l"))
    
    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
                        width = 0.85, position = "fill", color = "black") +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual("Risk of Bias",
                                 values = c(l = low_colour, s = concerns_colour, h = high_colour),
                                 labels = c(h = "  High risk of bias  ",
                                            s = "  Some concerns      ",
                                            l = "  Low risk of bias   ")) +
      ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
      ggplot2::labs(y = "Percentage of Studies")+
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 8, lineheight = 0.9, color = "black"),
        axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
        legend.position = c(0.5, -0.2),
        legend.justification = c("center", "bottom"),
        legend.direction = "horizontal",
        legend.background = ggplot2::element_rect(linetype = "solid", colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.5, "cm"),
        legend.text = ggplot2::element_text(size = 8),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(10, 70, 70, 10)
      )
    
    if (!quiet) print(plot)
    return(plot)
  }
}

