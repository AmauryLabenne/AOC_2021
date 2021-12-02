
requiredPackages <- c("dplyr",
                      #"plyr",
                      "tidyverse",
                      "ggplot2",
                      "ggforce",
                      "ggpubr",
                      "data.table",
                      "gridExtra",
                      "cowplot",
                      "officer",
                      "magrittr",
                      "lubridate")


for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}
# 
# 
# requiredPackages <- c("xlsx",
#                       "dplyr",
#                       "textshape",
#                       "tidyr",
#                       "tidyverse",
#                       "ggplot2",
#                       "ggforce",
#                       "ggpubr",
#                       "flextable",
#                       "lubridate",
#                       "stringr",
#                       "knitr",
#                       "rmarkdown",
#                       "scales",
#                       "huxtable")
