
# Function to parse district factsheets
require(tidyverse)
source("code/nr_functions.R")

nss75_mapping <- readxl::read_excel("code/NSS75 Variable List.xlsx",sheet="nss75 mapping")
nss75_variables <- readxl::read_excel("code/NSS75 Variable List.xlsx",sheet="variables")

table1 <- map2_dfr(c(0,1),c("Rural","Urban"),
                   function(p,t){
                     nss75_read(p,t)
                     
                   }
) %>% 
  left_join(nss75_mapping,
            by=c("STATE"="nss75_state")) %>% 
  dplyr::select(-STATE) %>% 
  dplyr::select(contains("nfhs4"),everything())