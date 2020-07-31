TradePlotMERA<-function (MSEobj, ..., Lims = c(0.2, 0.2, 0.8, 0.8), Title = NULL, 
          Labels = NULL, Satisficed = FALSE, Show = "both", point.size = 2, 
          lab.size = 4, axis.title.size = 12, axis.text.size = 10, 
          legend = TRUE, legend.title.size = 12, position = c("right", 
                                                              "bottom"), cols = NULL, fill = "gray80", 
          alpha = 0.4, PMlist = NULL, Refs = NULL, Yrs = NULL) 
{
  if (class(MSEobj) != "MSE") 
    stop("Object must be class `MSE`", call. = FALSE)
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package \"ggrepel\" needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  if (is.null(PMlist)) {
    PMlist <- unlist(list(...))
  }
  else {
    PMlist <- unlist(PMlist)
  }
  position <- match.arg(position)
  if (length(PMlist) == 0) 
    PMlist <- c("STY", "LTY", "P10", "AAVY")
  if (class(PMlist) != "character") 
    stop("Must provide names of PM methods")
  if (length(PMlist) < 2) 
    stop("Must provided more than 1 PM method")
  if (is.null(cols)) {
    cols <- c("#1b9e77", "#d95f02", "#7570b3", 
              "#e7298a")
  }
  if (length(cols) == MSEobj@nMPs) {
    Col <- "MP"
  } else {
    Col <- "Class"
  }
  nPMs <- length(PMlist)
  if (nPMs%%2 != 0) {
    message("Odd number of PMs. Recycling first PM")
    PMlist <- c(PMlist, PMlist[1])
    nPMs <- length(PMlist)
  }
  if (length(Lims) < nPMs) {
    message("Recycling limits")
    Lims <- rep(Lims, 10)[1:nPMs]
  }
  if (length(Lims) > nPMs) {
    Lims <- Lims[1:nPMs]
  }
  runPM <- vector("list", length(PMlist))
 
    #ref <- Refs[[PMlist[X]]]
    #yrs <- Yrs[[PMlist[X]]]
    
  nplots <- nPMs/2
  n.col <- ceiling(sqrt(nplots))
  n.row <- ceiling(nplots/n.col)
  m <- matrix(1:(n.col * n.row), ncol = n.col, nrow = n.row, 
              byrow = FALSE)
  xmin <- xmax <- ymin <- ymax <- x <- y <- Class <- label <- fontface <- NULL
  plots <- listout <- list()
  xInd <- seq(1, by = 2, length.out = nplots)
  yInd <- xInd + 1
  if (!(is.null(Title))) 
    Title <- rep(Title, nplots)[1:nplots]
  for (pp in 1:nplots) {
    yPM <- PMlist[yInd[pp]]
    yvals <- runPM[[match(yPM, PMlist)]]@Mean
    ycap <- runPM[[match(yPM, PMlist)]]@Caption
    yname <- runPM[[match(yPM, PMlist)]]@Name
    yline <- Lims[match(yPM, PMlist)]
    xPM <- PMlist[xInd[pp]]
    xvals <- runPM[[match(xPM, PMlist)]]@Mean
    xcap <- runPM[[match(xPM, PMlist)]]@Caption
    xname <- runPM[[match(xPM, PMlist)]]@Name
    xline <- Lims[match(xPM, PMlist)]
    xlim <- c(0, max(max(xvals, 1)))
    ylim <- c(0, max(max(yvals, 1)))
    xrect <- data.frame(xmin = 0, xmax = xline, ymin = 0, 
                        ymax = max(ylim))
    yrect <- data.frame(xmin = 0, xmax = max(xlim), ymin = 0, 
                        ymax = yline)
    MPType <- MPtype(MSEobj@MPs)
    Class <- MPType[match(MSEobj@MPs, MPType[, 1]), 2]
    labels <- MSEobj@MPs
    if (class(Labels) == "list") {
      repnames <- names(Labels)
      invalid <- repnames[!repnames %in% labels]
      if (length(invalid > 0)) {
        warning("Labels: ", paste(invalid, collapse = ", "), 
                " are not MPs in MSE")
        Labels[invalid] <- NULL
        repnames <- names(Labels)
      }
      labels[labels %in% repnames] <- Labels %>% unlist()
    }
    df <- data.frame(x = xvals, y = yvals, label = labels, 
                     Class = Class, pass = xvals > xline & yvals > yline, 
                     fontface = "plain", xPM = xPM, yPM = yPM)
    df$fontface <- as.character(df$fontface)
    df$fontface[!df$pass] <- "italic"
    df$fontface <- factor(df$fontface)
    listout[[pp]] <- df
    if (Satisficed) {
      xlim <- c(xline, 1)
      ylim <- c(yline, 1)
      plots[[pp]] <- ggplot2::ggplot()
    }
    else {
      plots[[pp]] <- ggplot2::ggplot() + ggplot2::geom_rect(data = xrect, 
                                                            ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                                                                         ymax = ymax), fill = fill, alpha = alpha) + 
        ggplot2::geom_rect(data = yrect, ggplot2::aes(xmin = xmin, 
                                                      xmax = xmax, ymin = ymin, ymax = ymax), fill = fill, 
                           alpha = alpha)
    }
    if (Col == "Class") {
      plots[[pp]] <- plots[[pp]] + ggplot2::geom_point(data = df, 
                                                       ggplot2::aes(x, y, shape = Class, color = Class), 
                                                       size = point.size, na.rm = TRUE)
      if (!is.null(lab.size)) 
        plots[[pp]] <- plots[[pp]] + ggrepel::geom_text_repel(data = df, 
                                                              ggplot2::aes(x, y, color = Class, label = label, 
                                                                           fontface = fontface), show.legend = FALSE, 
                                                              size = lab.size, na.rm = TRUE)
    }
    else if (Col == "MP") {
      plots[[pp]] <- plots[[pp]] + ggplot2::geom_point(data = df, 
                                                       ggplot2::aes(x, y, shape = Class, color = label), 
                                                       size = point.size, na.rm = TRUE)
      if (!is.null(lab.size)) 
        plots[[pp]] <- plots[[pp]] + ggrepel::geom_text_repel(data = df, 
                                                              ggplot2::aes(x, y, color = label, label = label, 
                                                                           fontface = fontface), show.legend = FALSE, 
                                                              size = lab.size, na.rm = TRUE)
    }
    plots[[pp]] <- plots[[pp]] + ggplot2::xlab(xcap) + ggplot2::ylab(ycap) + 
      ggplot2::xlim(xlim) + ggplot2::ylim(ylim) + ggplot2::theme_classic() + 
      ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size), 
                     axis.text = ggplot2::element_text(size = axis.text.size), 
                     legend.text = ggplot2::element_text(size = legend.title.size), 
                     legend.title = ggplot2::element_text(size = legend.title.size)) + 
      ggplot2::labs(shape = "MP Type", color = "MP Type")
    if (Col == "Class") {
      plots[[pp]] <- plots[[pp]] + ggplot2::scale_colour_manual(values = cols)
    }
    else if (Col == "MP") {
      plots[[pp]] <- plots[[pp]] + ggplot2::scale_colour_manual(values = cols) + 
        ggplot2::guides(color = FALSE)
    }
    if (!is.null(Title)) 
      plots[[pp]] <- plots[[pp]] + ggplot2::labs(title = Title[pp])
    if (legend == FALSE) 
      plots[[pp]] <- plots[[pp]] + ggplot2::theme(legend.position = "none")
  }
  out <- do.call("rbind", listout)
  tab <- table(out$label, out$pass)
  passall <- rownames(tab)[tab[, ncol(tab)] == nplots]
  Results <- summary(MSEobj, PMlist, silent = TRUE, Refs = Refs)
  Results$Satisificed <- FALSE
  Results$Satisificed[match(passall, Results$MP)] <- TRUE
  Results <- Results[, unique(colnames(Results))]
  if (Show == "plots") {
    join_plots(plots, n.col, n.row, position = position, 
               legend = legend)
  }
  else if (Show == "table") {
    print(Results)
  }
  else {
    join_plots(plots, n.col, n.row, position = position, 
               legend = legend)
    print(Results)
  }
  out <- list(Results = Results, Plots = plots)
  invisible(out)
}