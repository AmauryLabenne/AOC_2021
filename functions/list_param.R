

# On génere la liste des indicateurs à la main pour respecter l'ordre voulu
# Si on veut générer automatiquement on peut utiliser le code commenté dessous
# WARNING : les noms saisis ici doivent absolument correspondre au nom dans
# dict$lib_pe, ce sont les noms affichés dans les rapports.
list_urg_nat <-c("Geste suicidaire",
             "Idées suicidaires",
             "Troubles de l’humeur",
             "Troubles anxieux",
             "Troubles psychotiques",
             "Troubles de l’alimentation",
             "Intoxication éthylique (Alcool)")

list_sos_nat <-c("Angoisse",
                 "Etat dépressif",
                 "Trouble du comportement")



list_urg_reg <-c("Troubles psychologiques / adultes",
                 "Troubles psychologiques / enfants (<18 ans)",
                 "Geste suicidaire",
                 "Idées suicidaires" ,
                 "Troubles de l’humeur" ,
                 "Troubles anxieux",
                 "Troubles psychotiques",
                 "Troubles de l’alimentation")

list_sos_reg <-c("Angoisse",
                 "Etat dépressif",
                 "Trouble du comportement")


# NATIONAL
# list_urg <-dict %>%
#   filter(pour_PE_nat == 1) %>%
#   filter(source == "SAU") %>%
#   select(lib_pe) %>%
#   unique() %>%
#   pull()
# 
# list_sos <-dict %>%
#   filter(pour_PE_nat == 1) %>%
#   filter(source == "SOS") %>%
#   select(lib_pe) %>%
#   unique() %>%
#   pull()

# REGIONAL
# list_urg <-dict %>%
#   filter(pour_PE_reg == 1) %>%
#   filter(source == "SAU") %>%
#   select(lib_pe) %>%
#   unique() %>%
#   pull()
# 
# list_sos <-dict %>%
#   filter(pour_PE_reg == 1) %>%
#   filter(source == "SOS") %>%
#   select(lib_pe) %>%
#   unique() %>%
#   pull()

