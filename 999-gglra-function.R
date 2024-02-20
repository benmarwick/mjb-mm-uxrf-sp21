

gglra <- 
function (obj, 
          map = "symmetric", 
          rescale = 1, 
          dim = c(1, 2), 
          axes.inv = c(1, 1), 
          main = "", 
          cols = c("blue", "red"), 
          colarrows = "pink", 
          cexs = c(0.8, 0.8), 
          fonts = c(2, 4)) {
  
  library(ggplot2)
  library(ggrepel)
  library(ggalt)
  library(shadowtext)
  
  obj.rpc <- obj$rowcoord[, dim] %*% diag(obj$sv[dim] * axes.inv)
  obj.csc <- obj$colcoord[, dim] %*% diag(axes.inv)
  obj.cpc <- obj.csc %*% diag(obj$sv[dim])
  obj.ccc <- obj.csc * sqrt(obj$colmass)
  if (map == "symmetric") 
    obj.crd <- obj.cpc
  if (map == "asymmetric") 
    obj.crd <- obj.csc
  if (map == "contribution") 
    obj.crd <- obj.ccc
  if ((map != "symmetric") & (map != "asymmetric") & (map != 
                                                      "contribution")) 
    stop("map option is not a valid choice of present options: symmetric, asymmetric, contribution")
  perc.hor <- 100 * obj$sv[dim[1]]^2/sum(obj$sv^2)
  perc.ver <- 100 * obj$sv[dim[2]]^2/sum(obj$sv^2)
  
  obj.rpc.df <-  data.frame(obj.rpc)
  obj.rpc.df$groups_for_hulls <- str_remove(obj$rownames, "-pt.*")
  
  
  # get some centre points to place the labels
  centroid_for_labels <- 
    data.frame(obj.rpc, 
               label = obj$rownames) %>% 
    mutate(sample_group = str_remove(label, "-pt.*")) %>% 
    group_by(sample_group) %>% 
    summarise(mean_x = mean(X1),
              mean_y = mean(X2))
  
  point_labels <- 
    data.frame(obj.rpc, 
               label = obj$rownames) %>% 
    mutate(sample_group = str_remove(label, "-pt.*")) %>% 
    mutate(label =  str_extract(label, "pt-.{3}")) 

    
  
  legend_labels <- c("Termite mound", "Archaeological deposit")
  legend_title <- "Legend"
  
  ggplot() +
    theme_minimal() +
    labs(x =  paste("LRA dimension ", dim[1], " (", 
                    round(perc.hor, 1), "%)", sep = ""),
         y = paste("LRA dimension ", 
                   dim[2], " (", round(perc.ver, 1), "%)", sep = "")) +
    geom_vline(xintercept = 0, col = "gray", lty = 2) +
    geom_hline(yintercept = 0, col = "gray", lty = 2)  +
    geom_point(data = data.frame(obj.rpc),
               aes(X1, 
                   X2,
                   colour = str_detect(obj$rownames, "mm"),
                   shape = str_detect(obj$rownames, "mm")),
               size = 3) +
    geom_encircle(data = obj.rpc.df, 
                  aes(X1, 
                      X2,
                      group = groups_for_hulls), 
                  fill = "grey80",
                  s_shape = 1, 
                  expand = 0,
                  alpha = 0.2, 
                  color = "black", 
                  show.legend = FALSE) + 
    geom_text(data = data.frame(obj.crd,
                                label = obj$colnames),
              aes(X1, 
                  X2, 
                  label = label),
              colour = "red",
              size = 3) + 
    geom_shadowtext(data = centroid_for_labels,
              aes(mean_x, 
                  mean_y, 
                  label = sample_group),
              size = 2,
              colour = "black",
              bg.colour = "white") +
    # this shows the individual point lables
    # set size = 0 to hide these labels
    geom_shadowtext(data = point_labels,
                    aes( X1 , 
                         X2, 
                        label = label,
                        colour = str_detect(obj$rownames, "mm")),
                    size = 0,  # set size = 0 to hide these labels
                    bg.colour = "white") +
    scale_color_manual(values = scales::hue_pal()(2),
                       labels = legend_labels) +
    scale_shape_manual(values = scales:::shape_pal()(2),
                       labels = legend_labels) +
    guides(colour = guide_legend(title = legend_title),
           shape = guide_legend(title = legend_title)) +
    theme(legend.position = c(.3,.15))
  
}

## plot by phases -------------------------------------
gglra_phases <- 
  function (obj, 
            map = "symmetric", 
            rescale = 1, 
            dim = c(1, 2), 
            axes.inv = c(1, 1), 
            main = "", 
            cols = c("blue", "red"), 
            colarrows = "pink", 
            cexs = c(0.8, 0.8), 
            fonts = c(2, 4)) {
    
    library(ggplot2)
    library(ggrepel)
    library(ggalt)
    library(shadowtext)
    
    obj.rpc <- obj$rowcoord[, dim] %*% diag(obj$sv[dim] * axes.inv)
    obj.csc <- obj$colcoord[, dim] %*% diag(axes.inv)
    obj.cpc <- obj.csc %*% diag(obj$sv[dim])
    obj.ccc <- obj.csc * sqrt(obj$colmass)
    if (map == "symmetric") 
      obj.crd <- obj.cpc
    if (map == "asymmetric") 
      obj.crd <- obj.csc
    if (map == "contribution") 
      obj.crd <- obj.ccc
    if ((map != "symmetric") & (map != "asymmetric") & (map != 
                                                        "contribution")) 
      stop("map option is not a valid choice of present options: symmetric, asymmetric, contribution")
    perc.hor <- 100 * obj$sv[dim[1]]^2/sum(obj$sv^2)
    perc.ver <- 100 * obj$sv[dim[2]]^2/sum(obj$sv^2)
    
    obj.rpc.df <-  data.frame(obj.rpc)
    obj.rpc.df$groups_for_hulls <- str_extract(obj$rownames, "phase\\d")
    
    
    # get some centre points to place the labels
    centroid_for_labels <- 
      data.frame(obj.rpc, 
                 label = obj$rownames) %>% 
      mutate(phase = str_extract(label, "phase\\d")) %>% 
      group_by(phase) %>% 
      summarise(mean_x = mean(X1),
                mean_y = mean(X2))
    
    ggplot() +
      theme_minimal() +
      labs(x =  paste("LRA dimension ", dim[1], " (", 
                      round(perc.hor, 1), "%)", sep = ""),
           y = paste("LRA dimension ", 
                     dim[2], " (", round(perc.ver, 1), "%)", sep = "")) +
      geom_vline(xintercept = 0, col = "gray", lty = 2) +
      geom_hline(yintercept = 0, col = "gray", lty = 2)  +
      geom_point(data = data.frame(obj.rpc),
                 aes(X1, 
                     X2,
                     colour = str_extract(obj$rownames, "phase\\d")),
                 size = 3) +
      geom_encircle(data = obj.rpc.df, 
                    aes(X1, 
                        X2,
                        group = groups_for_hulls), 
                    fill = "grey80",
                    s_shape = 1, 
                    expand = 0,
                    alpha = 0.2, 
                    color = "black", 
                    show.legend = FALSE) + 
      geom_text(data = data.frame(obj.crd,
                                  label = obj$colnames),
                aes(X1, 
                    X2, 
                    label = label),
                colour = "red",
                size = 5) + 
      geom_shadowtext(data = centroid_for_labels,
                aes(mean_x, 
                    mean_y, 
                    label = phase),
                size = 3,
                colour = "black",
                bg.colour = "white") +
      scale_color_viridis_d() +
      guides(colour = guide_legend("Phase"),
             shape = 'none')
    
  }


## plot by rows -------------------------------------
gglra_rows <- 
  function (obj, 
            map = "symmetric", 
            rescale = 1, 
            dim = c(1, 2), 
            axes.inv = c(1, 1), 
            main = "", 
            cols = c("blue", "red"), 
            colarrows = "pink", 
            cexs = c(0.8, 0.8), 
            fonts = c(2, 4)) {
    
    library(ggplot2)
    library(ggrepel)
    library(ggalt)
    library(shadowtext)
    
    obj.rpc <- obj$rowcoord[, dim] %*% diag(obj$sv[dim] * axes.inv)
    obj.csc <- obj$colcoord[, dim] %*% diag(axes.inv)
    obj.cpc <- obj.csc %*% diag(obj$sv[dim])
    obj.ccc <- obj.csc * sqrt(obj$colmass)
    if (map == "symmetric") 
      obj.crd <- obj.cpc
    if (map == "asymmetric") 
      obj.crd <- obj.csc
    if (map == "contribution") 
      obj.crd <- obj.ccc
    if ((map != "symmetric") & (map != "asymmetric") & (map != 
                                                        "contribution")) 
      stop("map option is not a valid choice of present options: symmetric, asymmetric, contribution")
    perc.hor <- 100 * obj$sv[dim[1]]^2/sum(obj$sv^2)
    perc.ver <- 100 * obj$sv[dim[2]]^2/sum(obj$sv^2)
    
    obj.rpc.df <-  data.frame(obj.rpc)
    obj.rpc.df$groups_for_hulls <- str_extract(obj$rownames, "row\\d")
    
    
    # get some centre points to place the labels
    centroid_for_labels <- 
      data.frame(obj.rpc, 
                 label = obj$rownames) %>% 
      mutate(row = str_extract(label, "row\\d")) %>% 
      group_by(row) %>% 
      summarise(mean_x = mean(X1),
                mean_y = mean(X2))
    
    ggplot() +
      theme_minimal() +
      labs(x =  paste("LRA dimension ", dim[1], " (", 
                      round(perc.hor, 1), "%)", sep = ""),
           y = paste("LRA dimension ", 
                     dim[2], " (", round(perc.ver, 1), "%)", sep = "")) +
      geom_vline(xintercept = 0, col = "gray", lty = 2) +
      geom_hline(yintercept = 0, col = "gray", lty = 2)  +
      geom_point(data = data.frame(obj.rpc),
                 aes(X1, 
                     X2,
                     colour = str_extract(obj$rownames, "row\\d")),
                 size = 3) +
      geom_encircle(data = obj.rpc.df, 
                    aes(X1, 
                        X2,
                        group = groups_for_hulls), 
                    fill = "grey80",
                    s_shape = 1, 
                    expand = 0,
                    alpha = 0.2, 
                    color = "black", 
                    show.legend = FALSE) + 
      geom_text(data = data.frame(obj.crd,
                                  label = obj$colnames),
                aes(X1, 
                    X2, 
                    label = label),
                colour = "red",
                size = 5) + 
      geom_shadowtext(data = centroid_for_labels,
                      aes(mean_x, 
                          mean_y, 
                          label = row),
                      size = 3,
                      colour = "black",
                      bg.colour = "white") +
      scale_color_viridis_d() +
      guides(colour = guide_legend("Row"),
             shape = 'none')
    
  }
