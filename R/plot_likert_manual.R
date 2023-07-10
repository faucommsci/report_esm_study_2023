# Edit plot_likert() function from sjPlot
#  -> Display "t=[i]" instead of "n=[i]"


# load helper functions
# > `is.brewer.pal()` from `color_utils.R` 

# check whether a color value is indicating
# a color brewer palette
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  bp <- c(bp.seq, bp.div, bp.qul)
  pal %in% bp
}


# > `.is_false()` from `helpfunctions.R` 

.is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
 

# > `sj.setGeomColors()` from `sjPlotSetTheme.R` 
sj.setGeomColors <- function(plot,
                             geom.colors,
                             pal.len,
                             show.legend = TRUE,
                             labels = NULL,
                             reverse.colors = FALSE) {
  # ---------------------------------------------------------
  # check for themr options
  # ---------------------------------------------------------
  if (!is.null(geom.colors) && geom.colors[1] == "themr") {
    return(plot)
  }
  
  labels <- factor(unname(labels), levels = labels)
  
  # ---------------------------------------------------------
  # dummy function for setting legend labels and geom-colors
  # ---------------------------------------------------------
  usenormalscale <- function(plot, geom.colors, labels, bw.figure, ltypes) {
    if (!show.legend) {
      plot <- plot +
        scale_fill_manual(values = geom.colors, guide = FALSE) +
        scale_colour_manual(values = geom.colors, guide = FALSE) +
        scale_linetype_manual(values = ltypes, guide = FALSE) +
        guides(fill = "none", colour = "none", text = "none", linetype = "none")
    } else {
      plot <- plot +
        scale_fill_manual(values = geom.colors, labels = labels) +
        scale_colour_manual(values = geom.colors, labels = labels) +
        scale_linetype_manual(values = ltypes)
      # for b/w figures, add linetype scale
      if (bw.figure) {
        plot <- plot + guides(text = "none", colour = "none")
      } else {
        plot <- plot + guides(text = "none", linetype = "none")
      }
    }
    
    plot
  }
  # ---------------------------------------------------------
  # dummy function for only setting legend labels, but no
  # geom-colors
  # ---------------------------------------------------------
  uselegendscale <- function(plot, labels, bw.figure, ltypes) {
    if (!show.legend) {
      plot <- plot +
        scale_fill_discrete(guide = FALSE) +
        scale_colour_discrete(guide = FALSE) +
        scale_linetype_manual(values = ltypes, guide = FALSE) +
        guides(fill = "none", colour = "none", text = "none", linetype = "none")
    } else {
      plot <- plot +
        scale_fill_discrete(labels = labels) +
        scale_colour_discrete(labels = labels) +
        scale_linetype_manual(values = ltypes)
      # for b/w figures, add linetype scale
      if (bw.figure) {
        plot <- plot + guides(text = "none", colour = "none")
      } else {
        plot <- plot + guides(text = "none", linetype = "none")
      }
    }
    
    plot
  }
  
  # check if we have coloured plot or b/w figure with different linetypes
  bw.figure <- !is.null(geom.colors) && geom.colors[1] == "bw"
  
  if (bw.figure)
    ltypes <- seq_len(pal.len)
  else
    ltypes <- rep(1, times = pal.len)
  
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  if (!is.null(geom.colors)) {
    # brewer palette?
    if (is.brewer.pal(geom.colors[1])) {
      if (length(geom.colors) > 1) {
        neutral.color <- geom.colors[2]
        pal.len <- pal.len - 1
      } else {
        neutral.color <- NULL
      }
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(pal.len)
      if (reverse.colors) geom.colors <- rev(geom.colors)
      if (!is.null(neutral.color)) geom.colors <- c(geom.colors, neutral.color)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(pal.len)
      if (reverse.colors) geom.colors <- rev(geom.colors)
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = pal.len)
    } else if (length(geom.colors) > pal.len) {
      warning("More colors provided than needed. Shortening color palette.")
      geom.colors <- geom.colors[1:pal.len]
      if (reverse.colors) geom.colors <- rev(geom.colors)
    }
    
    if (length(geom.colors) < pal.len) {
      warning("Too less colors provided for plot. Using default color palette.")
      plot <- uselegendscale(plot, labels, bw.figure, ltypes)
    } else {
      plot <- usenormalscale(plot, geom.colors, labels, bw.figure, ltypes)
    }
  } else {
    plot <- uselegendscale(plot, labels, bw.figure, ltypes)
  }
  
  plot
}

# Main functions

plot_likert_manual <- function(items,
                        groups = NULL,
                        groups.titles = "auto",
                        title = NULL,
                        legend.title = NULL,  # Options to be passed directly to .plot_likert()
                        legend.labels = NULL,
                        axis.titles = NULL,
                        axis.labels = NULL,
                        catcount = NULL,
                        cat.neutral = NULL,
                        sort.frq = NULL,
                        weight.by = NULL,
                        title.wtd.suffix = NULL,
                        wrap.title = 50,
                        wrap.labels = 30,
                        wrap.legend.title = 30,
                        wrap.legend.labels = 28,
                        geom.size = .6,
                        geom.colors = "BrBG",
                        cat.neutral.color = "grey70",
                        intercept.line.color = "grey50",
                        reverse.colors = FALSE,
                        values = "show",
                        show.n = TRUE,
                        show.legend = TRUE,
                        show.prc.sign = FALSE,
                        grid.range = 1,
                        grid.breaks = 0.2,
                        expand.grid = TRUE,
                        digits = 1,
                        reverse.scale = FALSE,
                        coord.flip = TRUE,
                        sort.groups = TRUE, # Group Options
                        legend.pos = "bottom",
                        rel_heights = 1,
                        group.legend.options = list(nrow = NULL, byrow = TRUE), # Add rowwise order of levels and option to force a single rowed legend for 6 or more categories
                        cowplot.options = list(label_x = 0.01, hjust = 0, align = "v") # Fix for label position depending on label length bug in cowplot
) {
  
  # Select options to be passed to .plot_likert()
  .likert_options <- as.list(environment())[5:32]
  
  ## If now no groups are supplied only 1 group will be assumed. Check for cowplot is only performed if there are groups supplied.
  if (is.null(groups)) {
    groups <- rep(1, length.out = ncol(items))
  } else {
    if (!requireNamespace("cowplot", quietly = T))
      stop("Package 'cowplot' required for this function wor work. Please install it.", call. = F)
  }
  
  if (ncol(items) != length(groups))
    stop("Length of groups has to equal the number of items: ncol(items) != length(groups).", call. = F)
  
  # retrieve unique factor / group index values
  findex <- unique(groups)
  
  if (sort.groups) findex <- sort(findex)
  
  # Add empty title to plots, to create space for the group.labels
  if (is.null(title) && length(findex) != 1) title <- rep("", length(findex))
  
  .plot_list <- list()
  
  # iterate all sub-plots (groups)
  for (i in seq_along(findex)) {
    index <- which(groups == findex[i])
    
    .pl <- do.call(".plot_likert", args = c(list(items[, index], title = title[i]), .likert_options))
    
    # If there are 2 or more groups, the legend will be plotted according to legend.pos.
    if (length(findex) != 1) {
      if (legend.pos %in% c("top", "both") && i == 1)
        .pl <- .pl + theme(legend.position = "top") + guides(fill = do.call(guide_legend, group.legend.options))
      else if (legend.pos %in% c("bottom", "both") && i == length(findex))
        .pl <- .pl + theme(legend.position = "bottom") + guides(fill = do.call(guide_legend, group.legend.options))
      else if (legend.pos != "all")
        .pl <- .pl + theme(legend.position = "none")
    }
    
    .plot_list[i] <-  list(.pl)
  }
  
  # Options to turn off or overwrite cowplot group.labels.
  if (.is_false(groups.titles)) {
    groups.titles <- rep("", length(findex))
  } else if (!is.null(groups.titles) && (groups.titles[1] == "auto" || length(groups.titles) != length(findex)) && (is.numeric(groups))) {
    groups.titles <- sprintf("Component %i", seq_along(findex)) # For tab_itemscale compatibility
  } else if (length(groups.titles) != length(findex)) {
    groups.titles <- findex
  }
  
  # If groups were supplied, combine the subplots with cowplot::plot_grid()
  if (length(findex) == 1) {
    .out <- .plot_list[[1]]
  } else {
    .out <- do.call(get("plot_grid", asNamespace("cowplot")),
                    args = c(
                      list(
                        "plotlist" = .plot_list,
                        "labels" = groups.titles,
                        "rel_heights" = rel_heights,
                        "ncol" = 1
                      ),
                      cowplot.options
                    ))
  }
  
  .out
}

.plot_likert <- function(items,
                         title = NULL,
                         legend.title = NULL,
                         legend.labels = NULL,
                         axis.titles = NULL,
                         axis.labels = NULL,
                         catcount = NULL,
                         cat.neutral = NULL,
                         sort.frq = NULL,
                         weight.by = NULL,
                         title.wtd.suffix = NULL,
                         wrap.title = 50,
                         wrap.labels = 30,
                         wrap.legend.title = 30,
                         wrap.legend.labels = 28,
                         geom.size = .6,
                         geom.colors = "BrBG",
                         cat.neutral.color = "grey70",
                         intercept.line.color = "grey50",
                         reverse.colors = FALSE,
                         values = "show",
                         show.n = TRUE,
                         show.legend = TRUE,
                         show.prc.sign = FALSE,
                         grid.range = 1,
                         grid.breaks = 0.2,
                         expand.grid = TRUE,
                         digits = 1,
                         reverse.scale = FALSE,
                         coord.flip = TRUE) {
  
  # check param. if we have a single vector instead of
  # a data frame with several items, convert vector to data frame
  
  if (!is.data.frame(items) && !is.matrix(items)) items <- as.data.frame(items)
  
  # if grid.range is supplied as 1 value, it is duplicated for symmetric results. This is for compatibillity with older versions.
  if (length(grid.range) == 1) grid.range <- c(grid.range, grid.range)
  
  # copy titles
  
  if (is.null(axis.titles)) {
    axisTitle.x <- NULL
    axisTitle.y <- NULL
  } else {
    axisTitle.x <- axis.titles[1]
    if (length(axis.titles) > 1)
      axisTitle.y <- axis.titles[2]
    else
      axisTitle.y <- NULL
  }
  
  
  # check sorting
  
  if (!is.null(sort.frq)) {
    if (sort.frq == "pos.asc") {
      sort.frq  <- "pos"
      reverseOrder <- FALSE
    }
    if (sort.frq == "pos.desc") {
      sort.frq  <- "pos"
      reverseOrder <- TRUE
    }
    if (sort.frq == "neg.asc") {
      sort.frq  <- "neg"
      reverseOrder <- FALSE
    }
    if (sort.frq == "neg.desc") {
      sort.frq  <- "neg"
      reverseOrder <- TRUE
    }
  } else {
    reverseOrder <- FALSE
  }
  
  
  # try to automatically set labels is not passed as argument
  
  if (is.null(legend.labels)) {
    legend.labels <- sjlabelled::get_labels(
      items[[1]],
      attr.only = F,
      values = NULL,
      non.labelled = T
    )
  }
  
  if (is.null(axis.labels)) {
    # retrieve variable name attribute
    axis.labels <- unname(sjlabelled::get_label(items, def.value = colnames(items)))
  }
  
  
  # unname labels, if necessary, so we have a simple character vector
  if (!is.null(names(axis.labels))) axis.labels <- as.vector(axis.labels)
  
  if (!is.null(legend.labels)) {
    if (!is.null(names(legend.labels))) legend.labels <- as.vector(legend.labels)
  }
  
  
  # determine catcount
  
  adding <- ifelse(is.null(cat.neutral), 0, 1)
  
  if (is.null(catcount)) {
    # add new unique item values to catcount, so catcount
    # finally contains all unique values of items
    
    catcount <- items %>%
      purrr::map(~ stats::na.omit(unique(.x))) %>%
      purrr::flatten_dbl() %>%
      unique() %>%
      sort()
    
    neutral.between <- FALSE
    
    # remove neutral category
    if (!is.null(cat.neutral)) {
      # find neutral cat value in catcount
      ncv_pos <- which(catcount == cat.neutral)
      # if not empty, remove
      if (!sjmisc::is_empty(ncv_pos)) {
        catcount <- catcount[-ncv_pos]
        neutral.between <- dplyr::between(cat.neutral, min(catcount), max(catcount))
      }
    }
    
    # detect range of valid categories, which
    # then equals catcount
    catcount <- max(catcount) - min(catcount) + 1
    
    # check if category count matches category label count
    if (!is.null(legend.labels)) {
      # how many labels do we have?
      # substract 1, if we have neutral category
      lll <- length(legend.labels) - adding
      # catcount and legend label count equal?
      if (catcount < lll) {
        # warn user that detected amount of categories and supplied legend labels
        # are different.
        warning("Length of labels for item categories `legend.labels` differs from detected amount of categories. Use `catcount` argument to define amount of item categories, if plotting does not work.", call. = F)
        # adjust catcount to length of legend labels, because
        # we assume that labels represent the valid range of
        # item categories
        catcount <- lll
      }
    }
    
    # is catcount odd or even? make catcount even
    if (sjmisc::is_odd(catcount)) {
      # warn user about uneven category count, but only if
      # neutral category is not inside valid categories
      if (!neutral.between)
        warning("Detected uneven category count in items. Dropping last category.", call. = F)
      
      catcount <- catcount - 1
    }
  }
  
  
  # set legend labels, if we have none yet
  
  if (is.null(legend.labels)) legend.labels <- seq_len(catcount + adding)
  
  
  # prepare data frames
  
  mydat.pos <- data.frame()
  mydat.neg <- data.frame()
  mydat.dk <- data.frame()
  freq.df <- data.frame()
  
  # If we have neutral category in between and not as last
  # category, recode neutral category to last category
  if (!is.null(cat.neutral) && cat.neutral <= catcount) {
    # first, each other category has to be moved down one position
    # therefore, we create a pattern with values from neutral
    # category to category count
    downvote <- seq(cat.neutral, catcount + 1, by = 1)
    
    # now we "shift" this value pattern and make a
    # string out of it
    recode.pattern <- paste0(
      paste0(sprintf("%i=%i", c(downvote[-1], downvote[1]), downvote),
             collapse = ";"), ";else=copy"
    )
    
    # all factors with char labels need to be numeric,
    # else, recode won't work
    
    items <- purrr::modify_if(
      items,
      is_labelled_factor,
      sjlabelled::as_numeric,
      keep.labels = FALSE
    )
    
    # finally, recode data
    items <- sjmisc::rec(items, rec = recode.pattern, append = FALSE)
    
    # re-order legend labels as well
    ll.order <- c(seq_len(catcount + adding)[-cat.neutral], cat.neutral)
    legend.labels <- legend.labels[ll.order]
  }
  
  # loop through all likert-items
  for (i in seq_len(ncol(items))) {
    
    # convert to numeric values
    if (!is.numeric(items[[i]])) {
      items[[i]] <- sjlabelled::as_numeric(items[[i]], keep.labels = F)
    }
    
    # If we don't plot neutral category, but item still contains
    # that category, replace it with NA
    
    if (is.null(cat.neutral) && max(items[[i]], na.rm = T) > catcount)
      items[[i]] <- sjmisc::set_na(items[[i]], na = catcount + 1, as.tag = F)
    
    
    # create proportional frequency table
    
    if (is.null(weight.by)) {
      tab <- round(prop.table(table(items[[i]])), digits + 3)
    } else {
      tab <- round(prop.table(stats::xtabs(weight.by ~ items[[i]])), digits + 3)
    }
    
    
    # retrieve category number and related frequencies
    
    counts <- as.numeric(tab)
    valid <- as.numeric(names(tab))
    
    
    # create frequency vector, so zero-categories are cared for
    
    freq <- rep(0, catcount + adding)
    freq[valid] <- counts
    
    # append to data frame
    
    if (ncol(freq.df) == 0)
      freq.df <- as.data.frame(freq)
    else {
      # check for valid rows. if we hav missing categories
      # in all items, argument "catcount" must be set, because
      # automatic detection of amount of categories does not
      # work then.
      if (length(freq) != nrow(freq.df))
        stop("Could not determine amount of item categories. Please use argument `catcount`.", call. = F)
      else
        freq.df <- as.data.frame(cbind(freq.df, freq))
    }
  }
  
  
  # Check whether N of each item should be included into axis labels
  
  if (show.n) {
    for (i in seq_len(length(axis.labels))) {
      axis.labels[i] <- paste(
        axis.labels[i],
        sprintf(" (T=%i)", length(stats::na.omit(items[[i]]))),
        sep = ""
      )
    }
  }
  
  # determine split between pos and neg values
  
  # lower.half <- rev(seq(catcount / 2))
  lower.half <- rev(seq(ceiling(catcount / 2)))
  # upper.half <- 1 + catcount - lower.half
  upper.half <- setdiff(seq.int(catcount), lower.half)
  
  # sum up values to total, so we can sort items
  
  sums.lower <- unname(apply(freq.df[lower.half, , drop = FALSE], 2, sum))
  sums.upper <- unname(apply(freq.df[upper.half, , drop = FALSE], 2, sum))
  
  # sort items
  
  if (is.null(sort.frq))
    sort.freq <- seq_len(ncol(freq.df))
  else if (sort.frq == "pos")
    sort.freq <- order(sums.lower)
  else if (sort.frq == "neg")
    sort.freq <- order(sums.upper)
  else
    sort.freq <- seq_len(ncol(freq.df))
  
  # reverse item order?
  
  if (!reverseOrder) sort.freq <- rev(sort.freq)
  
  # save summed up y-values, for label positioning and annotation
  
  ypos.sum.pos <- c()
  ypos.sum.neg <- c()
  ypos.sum.dk <- c()
  
  # iterate all frequencies of the items. we have the simple
  # data rows in this data frame and now need to "split"
  # positive and negative values
  
  for (i in seq_len(ncol(freq.df))) {
    # sort
    fr <- freq.df[, sort.freq[i]]
    
    # positive values. we need an x-position for each item,
    # a group indicator, the frequencies (as percent value),
    # and the y position for labels.
    
    mydat.pos <- as.data.frame(
      rbind(mydat.pos,
            cbind(x = i,
                  grp = lower.half,
                  frq = fr[lower.half],
                  ypos = cumsum(fr[lower.half]) - 0.5 * (fr[lower.half]),
                  ypos2 = sum(fr[lower.half])
            )))
    
    
    # summed y-position for plotting the summed up frequency labels
    
    ypos.sum.pos <- c(ypos.sum.pos, sum(fr[lower.half]))
    
    # same as above for negative values
    
    mydat.neg <- as.data.frame(
      rbind(mydat.neg,
            cbind(x = i,
                  grp = upper.half,
                  frq = -fr[upper.half],
                  ypos = -1 * (cumsum(fr[upper.half]) - 0.5 * (fr[upper.half])),
                  ypos2 = -1 * sum(fr[upper.half])
            )))
    
    # summed up (cumulative) percs
    ypos.sum.neg <- c(ypos.sum.neg, -1 * sum(fr[upper.half]))
    
    # same as above for neutral category, if we have any
    
    if (!is.null(cat.neutral)) {
      mydat.dk <- as.data.frame(
        rbind(mydat.dk,
              cbind(x = i,
                    grp = catcount + adding,
                    frq = -1 + fr[catcount + adding],
                    ypos = -1 + (fr[catcount + adding] / 2),
                    ypos2 = -1 + fr[catcount + adding],
                    offset = -1 * grid.range[1])
        ))
      
      # cumulative neutral cat
      ypos.sum.dk <- c(ypos.sum.dk, -1 + fr[catcount + adding])
    }
  }
  
  
  # x-positions for cumulative percentages
  
  xpos.sum.dk <- xpos.sum.neg <- xpos.sum.pos <- seq_len(length(ypos.sum.pos))
  
  # grp as factor
  
  mydat.pos$grp <- as.factor(mydat.pos$grp)
  mydat.neg$grp <- as.factor(mydat.neg$grp)
  
  # same for neutral
  if (!is.null(cat.neutral)) {
    mydat.dk$grp <- as.factor("neutral")
    mydat.dk$geom.size <- geom.size
    mydat.dk$digits <- digits
  }
  
  # label digits needed
  
  mydat.neg$digits <- digits
  mydat.pos$digits <- digits
  
  # Prepare and trim legend labels to appropriate size
  
  legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  
  if (!is.null(legend.title)) {
    legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  }
  
  if (!is.null(title)) {
    if (!is.null(title.wtd.suffix)) {
      title <- paste(title, title.wtd.suffix, sep = "")
    }
    title <- sjmisc::word_wrap(title, wrap.title)
  }
  
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  axis.labels <- axis.labels[sort.freq]
  
  # set diagram margins
  
  if (expand.grid) {
    expgrid <- waiver()
  } else {
    expgrid <- c(0, 0)
  }
  
  # Set up grid breaks. Calculate grid breaks starting at the center (0). Negative sequence is reversed. Positive sequence is skipping the 0 to avoid doubeling.
  
  gridbreaks <- round(c(rev(seq(0, -grid.range[1], by = -grid.breaks)), seq(grid.breaks, grid.range[2], by = grid.breaks)), 2)
  gridlabs <- ifelse(abs(gridbreaks) > 1, "", paste0(abs(round(100 * gridbreaks)), "%"))
  
  # start plot here
  
  gp <- ggplot() +
    # positive value bars
    geom_col(
      data = mydat.pos,
      aes_string(x = "x", y = "frq", fill = "grp"),
      width = geom.size
    ) +
    # negative value bars
    geom_col(
      data = mydat.neg,
      aes_string(x = "x", y = "frq", fill = "grp"),
      width = geom.size,
      position = position_stack(reverse = T)
    )
  
  # print bar for neutral category. this is a "fake" bar created
  # with geom_rect. to make this work, we need to set the x-axis
  # to a continuous scale...
  
  if (!is.null(cat.neutral)) {
    gp <- gp +
      geom_rect(
        data = mydat.dk,
        aes(
          xmin = .data$x - (geom.size / 2),
          xmax = .data$x + (geom.size / 2),
          ymin = .data$offset,
          ymax = .data$frq + (.data$offset + 1),
          fill = "neutral")
      )
  }
  
  # if we have neutral colors, we need to add the geom-color
  # to the color values.
  
  if (!is.null(cat.neutral)) geom.colors <- c(geom.colors, cat.neutral.color)
  
  # should percentage value labels be printed?
  
  percsign <- mydat.pos$percsign <- mydat.neg$percsign <- ifelse(isTRUE(show.prc.sign), "%", "")
  if (nrow(mydat.dk) > 0) mydat.dk$percsign <- percsign
  
  # creating value labels for cumulative percentages, so
  # zero-percentages are not printed
  
  ypos.sum.pos.lab  <- ifelse(ypos.sum.pos > 0, sprintf("%.*f%s", digits, 100 * ypos.sum.pos, percsign), "")
  ypos.sum.neg.lab  <- ifelse(ypos.sum.neg < 0, sprintf("%.*f%s", digits, 100 * abs(ypos.sum.neg), percsign), "")
  ypos.sum.dk.lab  <- ifelse(ypos.sum.dk > -1, sprintf("%.*f%s", digits, 100 * (1 + ypos.sum.dk), percsign), "")
  
  if (values == "show") {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      stop("Package `ggrepel` needed to plot labels. Please install it.", call. = FALSE)
    }
    # show them in middle of bar
    gp <- gp +
      ggrepel::geom_text_repel(
        data = dplyr::filter(mydat.pos, .data$frq > 0),
        aes(
          x = .data$x,
          y = .data$frq,
          label = sprintf("%.*f%s", digits, 100 * .data$frq, percsign)
        ),
        direction = "y",
        position = position_stack(vjust = 0.5, reverse = TRUE),
        force = .5,
        point.padding = NA
      ) +
      ggrepel::geom_text_repel(
        data = dplyr::filter(mydat.neg, .data$frq < 0),
        aes(
          x = .data$x,
          y = .data$frq,
          label = sprintf("%.*f%s", digits, 100 * abs(.data$frq), percsign)
        ),
        direction = "y",
        position = position_stack(vjust = 0.5, reverse = TRUE),
        force = .5,
        point.padding = NA
      )
    
    if (!is.null(cat.neutral)) {
      gp <- gp +
        geom_text(
          data = dplyr::filter(mydat.dk, .data$frq > -1),
          aes(
            x = .data$x,
            y = .data$ypos + .data$offset + 1,
            label = sprintf("%.*f%s", digits, 100 * (1 + .data$frq), percsign)
          )
        )
    }
  } else if (values == "sum.inside" || values == "sum.outside") {
    # choose label offsets for summed proportions
    move_pos_labels_left = dplyr::case_when(
      values == "sum.outside" & !reverse.scale ~ T,
      values == "sum.inside" & !reverse.scale ~ F,
      values == "sum.outside" & reverse.scale ~ F,
      values == "sum.inside" & reverse.scale ~ T
    )
    # show cumulative outside bar
    if (move_pos_labels_left) {
      hort.pos <- -0.15
      hort.neg <- 1.15
      hort.dk <- -0.15
      # show cumulative inside bar
    } else {
      hort.pos <- 1.15
      hort.neg <- -0.15
      hort.dk <- 1.15
    }
    
    gp <- gp +
      annotate("text", x = xpos.sum.pos, y = ypos.sum.pos, hjust = hort.pos, label = ypos.sum.pos.lab) +
      annotate("text", x = xpos.sum.neg, y = ypos.sum.neg, hjust = hort.neg, label = ypos.sum.neg.lab)
    
    if (!is.null(cat.neutral)) {
      gp <- gp +
        annotate("text", x = xpos.sum.dk, y = ypos.sum.dk + 1 - grid.range[1], hjust = hort.dk, label = ypos.sum.dk.lab)
    }
  }
  
  # continues with plot
  
  gp <- gp +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legend.title) +
    
    # scale x is continuous to make plotting the bar annotation
    # for neutral category work...
    
    scale_x_continuous(breaks = seq_len(ncol(freq.df)), labels = axis.labels) +
    geom_hline(yintercept = 0, color = intercept.line.color)
  
  # check wether percentage scale (y-axis) should be reversed
  
  if (!reverse.scale) {
    gp <- gp + scale_y_continuous(breaks = gridbreaks, limits = c(-grid.range[1], grid.range[2]), expand = expgrid, labels = gridlabs)
  } else {
    gp <- gp + scale_y_reverse(breaks = gridbreaks, limits = c(grid.range[2], -grid.range[1]), expand = expgrid, labels = gridlabs)
  }
  
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  
  if (coord.flip) gp <- gp + coord_flip()
  
  # set geom colors
  
  sj.setGeomColors(
    gp,
    geom.colors,
    (catcount + adding),
    show.legend,
    legend.labels,
    reverse.colors
  )
}

is_labelled_factor <- function(x) is.factor(x) && !sjmisc::is_num_fac(x)


