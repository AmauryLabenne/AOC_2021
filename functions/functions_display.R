
color_thm <- c("#eb8e0c", "#0caff5", "#f20505", "#0a0000")

## genere un graphique
gg_sem <- function(Cause, Age, Data, Y_lab = y_lab, Couleur = color_thm, small_legend = FALSE){
  
  dt_cs <- Data %>%
    filter(cause == Cause, clage == Age)
  
  titre_plot <- str_replace(Age, "00", "0")
 
  ## labels pour les axes des abscisses
  lab = paste0("S",sprintf("%02.0f",1:52))
  lab_abs <- seq(1, 52, 4)
  lab2 <- rep("",52)
  lab2[(1:52) %in% lab_abs] <- lab[(1:52) %in% lab_abs]
  
  p <- ggplot(dt_cs, aes(x = semaine, y = nbj)) +
    geom_line(aes(color = an), 
              alpha = 0.8, position = position_dodge(0.8), size = 1) +
    scale_color_manual(values = color_thm) +
    scale_fill_manual(values = color_thm) +
    scale_x_continuous(breaks = 1:52,labels = lab2) +
    # scale_x_date(breaks = 1:52 , labels = lab) +
    theme_classic() +
    
    theme(legend.key.size = unit(0, 'lines'),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          axis.title = element_text(size=7),
          axis.text = element_text(size = 6),
          axis.line = element_line(colour = "darkblue", size = 0.7, linetype = "solid"),
          plot.title = element_text(hjust = 0.5, colour = "#004192", size =8),
    ) +
    
    ggtitle(titre_plot) +
    
    labs(x = "",
       y = Y_lab)
  
  if (small_legend)
  {
    p <- p +
      theme(legend.key.size = unit(0, 'lines'),
            legend.text = element_text(size = 5))
      
  }
  
  return(p)
  
}


gg_mois <- function(Cause, Age, Data, Y_lab = y_lab, Couleur = color_thm, small_legend = FALSE){
  
  dt_cs <- Data %>%
    filter(cause == Cause, clage == Age)
  
  titre_plot <- str_replace(Age, "00", "0")
  
  ## labels pour les axes des abscisses
  lab = paste0("S",sprintf("%02.0f",1:12))
  lab_abs <- seq(1, 12, 1)
  lab2 <- rep("",12)
  lab2[(1:12) %in% lab_abs] <- lab[(1:12) %in% lab_abs]
  
  p <- ggplot(dt_cs, aes(x = mois, y = nbj)) +
    geom_line(aes(color = an, group = an), 
              alpha = 0.8, position = position_dodge(0.8), size = 1) +
    scale_color_manual(values = color_thm) +
    scale_fill_manual(values = color_thm) +
    #scale_x_continuous(breaks = 1:12,labels = lab2) +
    # scale_x_date(breaks = 1:52 , labels = lab) +
    theme_classic() +
    
    theme(legend.key.size = unit(0, 'lines'),
          legend.title = element_blank(),
          #axis.text.x = element_text(angle = 90, vjust=0.5),
          axis.title = element_text(size=7),
          axis.text = element_text(size = 6),
          axis.line = element_line(colour = "darkblue", size = 0.7, linetype = "solid"),
          plot.title = element_text(hjust = 0.5, colour = "#004192", size =8),
    ) +
    
    ggtitle(titre_plot) +
    
    labs(x = "",
         y = Y_lab)
  
  if (small_legend)
  {
    p <- p +
      theme(legend.key.size = unit(0, 'lines'),
            legend.text = element_text(size = 5))
    
  }
  
  return(p)
  
}



# cette fonction doit etre réadapté pour les régions en fonction
# de la disposition des graphiques et des classes d age
# y _start est l'année de départ a inclure
get_plot_indic_nat <- function(source_name,
                               indic, region,
                               full_dict, full_data, y_start)
{
  ### On restreint le dictionnaire pour récupérer le nom court de l'indic
  ### et les classes d ages associées
  dic <- full_dict %>%
    filter(source == source_name) %>%
    filter(lib_pe == indic)
    #filter(grepl(indic, lib_pe)) #ligne où lib_pe contient indic
  
  dic <- dic %>%
    filter(pour_PE_nat == 1)

  
  ### On récupère les données d'interet
  indic_brut <- pull(dic[1, 1])
  dat <- filter(full_data, Region == "France")
  
  dat <- dat %>%
    filter(cause == indic_brut) %>%
    filter(clage %in% dic$cl_age_nom) %>%
    filter(as.numeric(as.character(an)) >= y_start)
  
  #write.csv(dat,file="test.csv")
  
  ### Liste des ages a plotter
  list_age <-  sort(setdiff(dic$cl_age_nom, "Tous âges"))
  list_plot <- list()
  nb_age <- length(list_age)
  
  ### Plot multiple en grille
  y_lab <- ifelse(source_name == "SAU", "Nombre de passages", "Nombre d'actes")
  for(i in 1 : nb_age)
  {
    p <- gg_sem(Cause = indic_brut, Y_lab = y_lab, Age = list_age[i],
                 Data = dat, small_legend = TRUE)
    list_plot[[i]] <- p
  }
  grid_plot <- gridExtra::arrangeGrob(grobs = list_plot, ncol = 2, as.table = FALSE) #as.table = F to fill by col
  
  # label des colonnes de plot
  col_lab_1 <-  text_grob("Enfants (0-17 ans)", face= "bold")
  col_lab_2 <-  text_grob("Jeunes et adultes ( \u2265 18 ans)", face= "bold")
  grid_labs <-  gridExtra::arrangeGrob(col_lab_1, col_lab_2, ncol = 2)
  
  multi_plot <- plot_grid(grid_labs, grid_plot,
                          align="v", byrow=TRUE, ncol=1,
                          rel_heights = c(1/10, 9/10))
  
  ### Plot unique de "Tous ages"
  if("Tous âges" %in% dic$cl_age_nom)
  {
    plot_tous <-  gg_sem(Cause = indic_brut, Y_lab = y_lab, Age = "Tous âges", Data = dat)
    
  } else {
    plot_tous <- NULL
  }
  
  res <- list("multi_plot" = multi_plot, "plot_tous" = plot_tous)
  return(res)
  
}


# cette fonction doit etre réadapté pour les régions en fonction
# de la disposition des graphiques et des classes d age
# y _start est l'année de départ a inclure
get_plot_indic_reg <- function(source_name,
                               indic, region,
                               full_dict, full_data, y_start,
                               global_chart_name)
{
  global_chart <- global_chart_name
  
  ### On restreint le dictionnaire pour récupérer le nom court de l'indic
  ### et les classes d ages associées
  dic <- full_dict %>%
    filter(source == source_name) %>%
    filter(lib_pe == indic) %>%
    filter(pour_PE_reg == 1) %>%
    filter(cl_age_type %in% c("age_group_reg", "age_det"))
  

  ### On récupère les données d'interet
  indic_brut <- pull(dic[1, 1])
  dat <- filter(full_data, Region == region)
  dat <- dat %>%
    filter(cause == indic_brut) %>%
    filter(clage %in% dic$cl_age_nom) %>%
    filter(as.numeric(as.character(an)) >= y_start)
  
  #write.csv(dat,file="test.csv")
  
  ### Liste des ages a plotter
  list_age <-  sort(setdiff(dic$cl_age_nom, global_chart))
  list_plot <- list()
  nb_age <- length(list_age)
  
  ### Plot multiple en grille
  y_lab <- ifelse(source_name == "SAU", "Nombre de passages", "Nombre d'actes")
  for(i in 1 : nb_age)
  {
    p <- gg_mois(Cause = indic_brut, Y_lab = y_lab, Age = list_age[i],
                 Data = dat, small_legend = TRUE)
    list_plot[[i]] <- p
  }
  grid_plot <- gridExtra::arrangeGrob(grobs = list_plot, ncol = 2, as.table = TRUE) #as.table = T to fill by row
  nb_row_multi <-  ceiling(length(grid_plot) / 2) 
  multi_plot <- as_ggplot(grid_plot)
  
  ### Plot unique de global_chart
  if(global_chart %in% dic$cl_age_nom)
  {
    plot_tous <-  gg_mois(Cause = indic_brut, Y_lab = y_lab, Age = global_chart, Data = dat)
    
  } else {
    plot_tous <- NULL
  }
  
  res <- list("multi_plot" = multi_plot, "plot_tous" = plot_tous, "nb_row_multi" = nb_row_multi )
  return(res)
  
}

