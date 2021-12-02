
### Fonction qui recode les footers
recode_footers <- function(rdocx, region_txt, rapport_num, date)
{
  list_old_new <- list("rapport_region" = region_txt,
                       "rapport_num" = rapport_num,
                       "rapport_date" = as.character(date))
  
  n_list <- length(list_old_new)
  
  for (i in 1 : n_list)
  {
    new <- as.character(list_old_new[[i]])
    old <- names(list_old_new)[i]
    
    rdocx <- footers_replace_all_text(rdocx,
                                      old_value = old,
                                      new_value = new,
                                      fixed = TRUE,
                                      warn=FALSE)
  }
  return(rdocx)
}



### Fonction qui remplit le template intro
###  - remplacement des element a remplacer (list_old_new)
###  - remplacement de la date en footer
fill_intro <- function(rdocx, region, rapport_num, date, week_m1 = 0, name_out,
                       with_foot = TRUE, with_semaine_cle = TRUE)
{
  str_disp <- "Generate temp file intro"
  print(str_disp)
  
  
  # recodage des balises dans le template
  if (with_semaine_cle) # cas national
  {
    list_old_new <- list("RAPPORT_NUM" = rapport_num,
                         "rapport_date" = date,
                         "rapport_semaine_cle" = week_m1)
    
  } else {
    list_old_new <- list("RAPPORT_REGION" = region,
                         "RAPPORT_NUM" = rapport_num,
                         "rapport_date" = date)
  }

  n_list <- length(list_old_new)
  
  for (i in 1 : n_list)
  {
    new <- as.character(list_old_new[[i]])
    old <- names(list_old_new)[i]
    
    rdocx <- body_replace_all_text(rdocx, old_value = old,
                                   new_value = new, fixed = TRUE)
    
  }
  
  # recodage des footers uniquement
  if (with_foot)
  {
    region_txt <- ifelse(region == "France entière", "nationale",region)
    rdocx <- recode_footers(rdocx, region_txt, rapport_num, date)
  }
  
  
  print(rdocx, target = name_out)
  
  
}

### Fonction qui remplit le template methode
###  - remplacement de la date en footer
fill_methode <- function(rdocx, region, rapport_num, date, name_out)
{
  str_disp <- "Generate temp file / methode "
  print(str_disp)
  
  is_nat = (region == "France entière")
  
  # recodage des noms de région
  if (is_nat == FALSE) # cas regional
  {
    list_old_new <- list("rapport_region" = region)
    n_list <- length(list_old_new)
    
    for (i in 1 : n_list)
    {
      new <- as.character(list_old_new[[i]])
      old <- names(list_old_new)[i]
      
      rdocx <- body_replace_all_text(rdocx, old_value = old,
                                     new_value = new, fixed = TRUE)
      
    }
  }
  
  
  # recodage des footers uniquement
  region_txt <- ifelse(region == "France entière", "nationale",region)
  rdocx <- recode_footers(rdocx, region_txt, rapport_num, date)
  
  print(rdocx, target = name_out)
}

### Fonction qui remplit le template covipred (region)
###  - remplacement de la date en footer
fill_covipred <- function(rdocx, region, rapport_num, date, name_out)
{
  str_disp <- "Generate temp file / covipred "
  print(str_disp)
  
  is_nat = (region == "France entière")
  
  # recodage des noms de région
  if (is_nat == FALSE) # cas regional
  {
    list_old_new <- list("rapport_region" = region)
    n_list <- length(list_old_new)
    
    for (i in 1 : n_list)
    {
      new <- as.character(list_old_new[[i]])
      old <- names(list_old_new)[i]
      
      rdocx <- body_replace_all_text(rdocx, old_value = old,
                                     new_value = new, fixed = TRUE)
      
    }
  }
  
  # recodage des footers uniquement
  region_txt <- ifelse(region == "France entière", "nationale",region)
  rdocx <- recode_footers(rdocx, region_txt, rapport_num, date)
  
  print(rdocx, target = name_out)
}

### Fonction qui remplit le template intro
###  - remplacement des element a remplacer (list_old_new)
fill_end <- function(rdocx, region, rapport_num, date_text, name_out)
{
  str_disp <- "Generate temp file / end"
  print(str_disp)
  
  # recodage des balises dans le template
  region_txt <- toupper(ifelse(region == "France entière", "nationale", region))
  
  list_old_new <- list("rapport_region" = region_txt,
                       "rapport_num" = rapport_num,
                       "rapport_date_text" = date_text)
  
  
  n_list <- length(list_old_new)
  
  for (i in 1 : n_list)
  {
    new <- as.character(list_old_new[[i]])
    old <- names(list_old_new)[i]
    
    rdocx <- body_replace_all_text(rdocx, old_value = old,
                                   new_value = new, fixed = TRUE)
    
  }

  print(rdocx, target = name_out)
  
}

### Fonction qui remplit les templates urg(sau) et sos
###  - remplacement des element a remplacer (list_old_new)
###  - extraction des données et création des graphiques
###  - remplacement de la date en footer
fill_urg_sos_indic <- function(rdocx, region, rapport_num, date, indic,
                                   source_name, full_dict, full_dat,
                                   name_out)
{
  is_nat <- ifelse(region == "France entière", 1, 0)
  
  str_disp <- paste("Generate temp file", source_name, indic, sep = " / " )
  print(str_disp)
  
  #Année actuelle et 2 ans de plus avant
  tod <- as.Date(date, format = "%d/%m/%Y")
  y_end <- year(tod)
  y_start <- y_end - 3
  
  # Recuperation du label de la catégories tous ages (différent dans certains cas)
  if (is_nat == FALSE)
  {
    if (indic == "Troubles psychologiques / enfants (<18 ans)")
    {
      global_chart <- "00-17 ans"
      age_rapport <- "chez les moins de 18 ans"
    } else if (indic == "Troubles psychologiques / adultes"){
      global_chart <- "18 ans ou +"
      age_rapport <- "18 ans ou plus"
    } else if (indic == "Geste suicidaire"){
      global_chart <-  "11 ans ou +"
      age_rapport <- "tous âges à partir de 11 ans"
    } else if (indic == "Idées suicidaires"){
      global_chart <-  "11 ans ou +"
      age_rapport <- "tous âges à partir de 11 ans"
    } else {
      global_chart <- "Tous âges"
      age_rapport <- "tous âges"
    }
  } else  {
    global_chart <- "Tous âges"
    age_rapport <- "tous âges"
  }
  
  
  # recodage des balises dans le template
  list_old_new <- list("rapport_urg_sos_title" = toupper(indic),
                       "rapport_urg_sos_region_indic" = paste(region, indic, sep = " - "),
                       "rapport_y_start" = y_start,
                       "rapport_y_end" = y_end,
                       "rapport_urg_sos_period" = ifelse(is_nat, "hebdomadaire", "mensuel"),
                       "rapport_urg_sos_tous_label" = age_rapport)
  
  n_list <- length(list_old_new)
  
  for (i in 1 : n_list)
  {
    new <- as.character(list_old_new[[i]])
    old <- names(list_old_new)[i]
    
    rdocx <- body_replace_all_text(rdocx, old_value = old,
                                   new_value = new, fixed = TRUE)
    
  }
  
  # Get plots
  if (is_nat)
  {
    res_plots <- get_plot_indic_nat(source_name = source_name,
                                    indic = indic,
                                    region = region,
                                    full_dict = dict,
                                    full_data = full_dat,
                                    y_start = y_start)
  } else {
    res_plots <- get_plot_indic_reg(source_name = source_name,
                                    indic = indic,
                                    region = region,
                                    full_dict = dict,
                                    full_data = full_dat,
                                    y_start = y_start,
                                    global_chart_name = global_chart)
  }
  
  
  # add chart global
  rdocx <- cursor_reach(rdocx,"rapport_urg_sos_plot_tous")
  rdocx <- body_remove(rdocx)
  rdocx <- body_add_gg(rdocx, res_plots$plot_tous,style="SPF-PE-Tab-05-Figures",
                       height=1.8)
  
  #add vignette chart
  h_vignette <- ifelse(is_nat, 5.3, res_plots$nb_row_multi * 2 )
  w_vignette <- ifelse(is_nat, 7, 7)
  rdocx <- cursor_reach(rdocx,"rapport_urg_sos_plot_par_age")
  rdocx <- body_remove(rdocx)
  rdocx <- body_add_gg(rdocx, res_plots$multi_plot,style="SPF-PE-Tab-05-Figures",
                       height = h_vignette,
                       width = w_vignette) #width=7
  
  
  #recodage des footers
  region_txt <- ifelse(region == "France entière", "nationale",region)
  rdocx <- recode_footers(rdocx, region_txt, rapport_num, date)
  
  print(rdocx, target = name_out)
}

### Cette fonction génère tous les fichiers individuels nécessaires
### a la création du rapport final dans doc_templates/temp_files/
generate_all_temp_files_nat <- function(region, sau_sem, sos_sem, dict,
                                    list_urg, list_sos,
                                    rapport_num, date, week_m1, date_letter)
{
  ### 0 - Recuperation des parametres (ici on gere que le niveau national)
  nb_urg <- length(list_urg)
  nb_sos <- length(list_sos)
  start_urg <- 2
  end_urg <- start_urg + nb_urg - 1
  start_sos <- end_urg + 1
  end_sos <- start_sos + nb_sos -1
  
  #creation des noms de fichiers temp dans un dossier temp de la region
  nb_files <- 1 + nb_urg + nb_sos + 1 + 1 # intro + methode + derniere page
  
  region_file <-strtrim (str_replace(region, " ", "_"), 8)
  mainDir <- paste0(getwd(),"/doc_templates/temp_files/")
  subDir <- region_file
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  temp_fold <- paste0(mainDir, subDir, "/")
  list_files <- paste0(temp_fold, paste0("file_", 1:nb_files), ".docx")
  
  ### 1 - Generer intro
  doc <- read_docx("doc_templates/temp_intro_nat.docx")
  fill_intro(doc, region, rapport_num, date, week_m1, list_files[1])
  k = 2
  
  ### 2 - Generer rapport_urg (sau)
  for (i in 1 : nb_urg)
  {
    doc <- read_docx("doc_templates/temp_urg.docx")
    end_section = (i==nb_urg)

    fill_urg_sos_indic(rdocx = doc,
                       region = region,
                       rapport_num = rapport_num,
                       date = date,
                       indic = list_urg[i],
                       source_name = "SAU",
                       full_dict = dict,
                       full_dat = sau_sem,
                       name_out = list_files[k])

    k = k + 1
  }

  ### 3 - Generer rapport_sos
  for (i in 1 : nb_sos)
  {
    doc <- read_docx("doc_templates/temp_sos.docx")
    end_section = (i==nb_urg)

    fill_urg_sos_indic(rdocx = doc,
                       region = region,
                       rapport_num = rapport_num,
                       date = date,
                       indic = list_sos[i],
                       source_name = "SOS",
                       full_dict = dict,
                       full_dat = sos_sem,
                       name_out = list_files[k])

    k = k + 1
  }

  ### 4 - Generer methode
  doc <- read_docx("doc_templates/temp_methode_nat.docx")
  fill_methode(doc, region, rapport_num, date, list_files[k])
  k = k + 1

  ### 5 - Generer derniere page
  doc <- read_docx("doc_templates/temp_end_nat.docx")
  fill_end(doc, region, rapport_num, date_letter, list_files[k])

  #also return params and list_files
  res <- list(nb_urg  = nb_urg,
              nb_sos = nb_sos,
              start_urg = start_urg,
              end_urg = end_urg,
              start_sos = start_sos,
              end_sos = end_sos,
              list_files = list_files)

  return(res)
  
}



### Cette fonction génère tous les fichiers individuels nécessaires
### a la création du rapport final dans doc_templates/temp_files/
generate_all_temp_files_reg <- function(region, sau_sem, sos_sem, dict,
                                        list_urg, list_sos,
                                        rapport_num, date, week_m1, date_letter)
{
  ### 0 - Recuperation des parametres (ici on gere que le niveau national)
  nb_urg <- length(list_urg)
  nb_sos <- length(list_sos)
  start_urg <- 2
  end_urg <- start_urg + nb_urg - 1
  start_sos <- end_urg + 1
  end_sos <- start_sos + nb_sos -1
  
  #creation des noms de fichiers temp dans un dossier temp de la region
  nb_files <- 1 + nb_urg + nb_sos + 1 + 1 + 1 # intro + covipred + methode + derniere page
  
  
  region_file <-strtrim (str_replace(region, " ", "_"), 8)
  mainDir <- paste0(getwd(),"/doc_templates/temp_files/")
  subDir <- region_file
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  temp_fold <- paste0(mainDir, subDir, "/")
  list_files <- paste0(temp_fold, paste0("file_", 1:nb_files), ".docx")
  
  ### 1 - Generer intro
  doc <- read_docx("doc_templates/temp_intro_reg.docx")
  fill_intro(doc, region, rapport_num, date, week_m1, list_files[1],
             with_semaine_cle = FALSE, with_foot = TRUE)
  k = 2
  
  ### 2 - Generer rapport_urg (sau)
  for (i in 1 : nb_urg)
  {
    doc <- read_docx("doc_templates/temp_urg.docx")
    end_section = (i==nb_urg)
    
    fill_urg_sos_indic(rdocx = doc,
                       region = region,
                       rapport_num = rapport_num,
                       date = date,
                       indic = list_urg[i],
                       source_name = "SAU",
                       full_dict = dict,
                       full_dat = sau_sem,
                       name_out = list_files[k])
    
    k = k + 1
  }
  
  ### 3 - Generer rapport_sos
  for (i in 1 : nb_sos)
  {
    doc <- read_docx("doc_templates/temp_sos.docx")
    end_section = (i==nb_urg)
    
    fill_urg_sos_indic(rdocx = doc,
                       region = region,
                       rapport_num = rapport_num,
                       date = date,
                       indic = list_sos[i],
                       source_name = "SOS",
                       full_dict = dict,
                       full_dat = sos_sem,
                       name_out = list_files[k])
    
    k = k + 1
  }
  
  ### 4 - Generer covipred
  doc <- read_docx("doc_templates/temp_covipred_reg.docx")
  fill_covipred(doc, region, rapport_num, date, list_files[k])
  k = k + 1
  
  ### 4 - Generer methode
  doc <- read_docx("doc_templates/temp_methode_reg.docx")
  fill_methode(doc, region, rapport_num, date, list_files[k])
  k = k + 1
  
  ### 5 - Generer derniere page
  doc <- read_docx("doc_templates/temp_end_reg.docx")
  fill_end(doc, region, rapport_num, date_letter, list_files[k])
  
  #also return params and list_files
  res <- list(nb_urg  = nb_urg,
              nb_sos = nb_sos,
              start_urg = start_urg,
              end_urg = end_urg,
              start_sos = start_sos,
              end_sos = end_sos,
              list_files = list_files)
  
  return(res)
  
}




### Merge tous les fichiers temporaires (list_files)
merge_temp_files <- function(start_urg, end_urg,
                             start_sos, end_sos,
                             list_files, name_out, is_nat = TRUE)
{
  # 1 : Intro
  main_doc <- read_docx(list_files[1])
  if(is_nat)
  {
    main_doc <- body_end_section_portrait(main_doc)
  } else {
    main_doc <- body_end_section_continuous(main_doc)
    
  }
  
  # 2 : Rapports_urg
  for(i in start_urg : end_urg){
    main_doc <- body_add_docx(main_doc, src = list_files[i])
    if (i != end_urg)
    {
      main_doc <- body_add_break(main_doc, pos = "after")
    }
  }
  main_doc <- body_end_section_portrait(main_doc)
  #main_doc <- body_end_section_continuous(main_doc)
  
  # 3 : Rapports_sos
  for(i in start_sos : end_sos){
    main_doc <- body_add_docx(main_doc, src = list_files[i])
    if (i != end_sos)
    {
      main_doc <- body_add_break(main_doc, pos = "after")
    }
  }
  # if(is_nat)
  # {
     main_doc <- body_end_section_portrait(main_doc)
  # } else {
  #   main_doc <- body_end_section_continuous(main_doc)
  # }
  
  k = end_sos + 1
  if(is_nat == FALSE)
  {
    # 4.0 : Covipred
    main_doc <- body_add_docx(main_doc, src = list_files[k])
    main_doc <- body_end_section_portrait(main_doc)
    k = k + 1
  }
  
  # 4 : Methode
  main_doc <- body_add_docx(main_doc, src = list_files[k])
  main_doc <- body_end_section_portrait(main_doc)
  
  # 5 : Page finale
  k = k + 1
  main_doc <- body_add_docx(main_doc, src = list_files[k])
  
  # Enregistrement
  print(main_doc, target = name_out)
  
}



### Generation du rapport national
generate_rapport_nat <- function(region = region,
                                 sau_sem, sos_sem, dict,
                                 list_urg, list_sos,
                                 rapport_num, date, week_m1, date_letter,
                                 nom_rapport_final)
{
  options(warn=1)
  res_temp <- generate_all_temp_files_nat(region = region,
                                          sau_sem, sos_sem, dict,
                                          list_urg, list_sos,
                                          rapport_num, date, week_m1, date_letter)
  
  
  merge_temp_files(res_temp$start_urg, res_temp$end_urg,
                   res_temp$start_sos, res_temp$end_sos,
                   res_temp$list_files, nom_rapport_final)
  
  
}


### Generation du rapport regional
generate_rapport_reg <- function(region = region,
                                 sau_mois, sos_mois, dict,
                                 list_urg, list_sos,
                                 rapport_num, date, week_m1, date_letter,
                                 nom_rapport_final)
{
  options(warn=1)
  
  res_temp <- generate_all_temp_files_reg(region = region,
                                          sau_mois, sos_mois, dict,
                                          list_urg, list_sos,
                                          rapport_num, date, week_m1, date_letter)
  
  
  merge_temp_files(res_temp$start_urg, res_temp$end_urg,
                   res_temp$start_sos, res_temp$end_sos,
                   res_temp$list_files, nom_rapport_final)
  
  
}





