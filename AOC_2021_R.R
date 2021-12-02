remove(list=ls())

#set wd to source file location
library("rstudioapi")                                 
setwd(dirname(getActiveDocumentContext()$path)) 
eval(parse("functions/requirements_packages.R", encoding="UTF-8"))


##############################################################################
####################     DAY 1     ###########################################
##############################################################################
# Load data
dt <- fread('data/day1.txt')
v <- dt$V1


##################  Day 1.1  #################################################
n <-length(v)
nb_increase <- 0

# Way 1
for (i in 2 : n)
{
  v_i = v[i]
  v_m1 = v[i- 1]
  if (v_i > v_m1) {
    nb_increase = nb_increase + 1
  }
}
nb_increase

#Way2
sum(diff(v, lag=1) > 0)

##################  Day 1.2  #################################################
n <-length(v)
window = 1:3
v_sum_window = c()

while (window[3] <= n)
{
  v_sum_window <- c(v_sum_window, sum(v[window]) )
  window = window+1
}
v_sum_window
sum(diff(v_sum_window, lag=1) > 0)

##############################################################################
####################     DAY 2     ###########################################
##############################################################################
# Load data
dt <- fread('data/day2.txt')

##################  Day 2.1  #################################################
dt_sum <- dt %>% group_by(V1) %>%
  summarize(sum= sum(V2))

x = dt_sum %>%
  filter(V1 == "forward") %>%
  select(sum)

y_plus = dt_sum %>%
  filter(V1 == "down") %>%
  select(sum)

y_minus = dt_sum %>%
  filter(V1 == "up") %>%
  select(sum)

res = (y_plus - y_minus) * x
res

##################  Day 2.2  #################################################
res = list("h" = 0, "v" = 0, "aim" = 0)

## Ici on utilise <<- pour modifier une variable globale
## (assign ne marche pas sur les listes)
increase_res <- function(...)
{
  vect <- tibble(...)
  val <- vect$V2
  move <- vect$V1
  if (move == "down"){
    # assign('res$aim', res$aim + val, envir = .GlobalEnv)
    res$aim <<- res$aim + val
  } else if (move == "up") {
    # assign('res$aim', res$aim - val, envir = .GlobalEnv)
    res$aim <<- res$aim - val
  } else {
    # assign('res$h', res$h + val, envir = .GlobalEnv)
    # assign('res$v', res$aim * val, envir = .GlobalEnv)
    res$h <<- res$h + val
    res$v <<- res$v + (res$aim * val)
  }
  return(res)
}

res = list("h" = 0, "v" = 0, "aim" = 0)
system.time(pwalk(dt, increase_res))# map sans rien ressortir car on veut juste la var globale
#pmap_df(dt, increase_res) # si on veut dans un df on utilise p_map_dfr
res$h * res$v

#idem un peu plus long
# res = list("h" = 0, "v" = 0, "aim" = 0)
# system.time(
# for (i in 1 :nrow(dt))
# {
#   increase_res(dt[i,])
# }
# )

