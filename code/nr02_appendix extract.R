
# Function to parse district factsheets
require(tidyverse)
source("code/nr68_functions.R")

nss68_mapping <- readxl::read_excel("code/NSS68 Variable List.xlsx",sheet="nss68 mapping")
nss68_variables <- readxl::read_excel("code/NSS68 Variable List.xlsx",sheet="variables")

# Table 1 -----------
vec_tab1 <- data.frame(
  p = c(0:11),
  t = rep(c("Rural","Urban"),each=2,times=3),
  m = rep(c("URP","MRP","MMRP"),each=4)
)

table1 <- map_dfr(1:12,
                  function(i){
                    p = vec_tab1[i,]$p;
                    t = vec_tab1[i,]$t;
                    m = vec_tab1[i,]$m;
                    
                    nss68_read(p,t,m)
                    
                  }
) %>% 
  left_join(nss68_mapping,
            by=c("STATE"="nss68_state")) %>% 
  dplyr::select(-STATE) %>% 
  dplyr::select(contains("nfhs4"),everything())

# Table 2 --------------
vec_tab2 <- data.frame(
  p = c(12:17),
  t = rep(c("Rural","Urban"),times=3),
  m = rep(c("URP","MRP","MMRP"),each=2)
  
)

table2 <- map_dfr(c(1:6),
                  function(i){
                    p = vec_tab2[i,]$p;
                    t = vec_tab2[i,]$t;
                    m = vec_tab2[i,]$m;
                    
                    nss68_read(p,t,m) %>% 
                      mutate_at(vars(starts_with("P")),~as.numeric(.))
                    
                  }
) %>% 
  left_join(nss68_mapping,
            by=c("STATE"="nss68_state")) %>% 
  dplyr::select(-STATE) %>% 
  dplyr::select(contains("nfhs4"),everything())

# Table 3 -------------
vec_tab3 <- data.frame(
  p = c(18:53),
  t = rep(c("Rural","Urban"),each=6,times=3),
  m = rep(c("URP","MRP","MMRP"),each=12)
  
)

table3 <- map_dfr(1:36,
                  function(i){
                    p = vec_tab3[i,]$p;
                    t = vec_tab3[i,]$t;
                    m = vec_tab3[i,]$m;
                    
                    nss68_read(p,t,m) %>% 
                      mutate_at(vars(-ITEM_CATEGORY,-region,-measure),~as.numeric(.)) %>% 
                      pivot_longer(cols=-one_of("SL_NO","ITEM_CATEGORY","region","measure"),
                                   names_to="nfhs4_statecode",values_to="mpce")
                    
                  }
) %>% 
  left_join(nss68_mapping,
            by=c("nfhs4_statecode")) %>% 
  dplyr::select(-nss68_state) %>% 
  dplyr::select(contains("nfhs4"),everything())

# Table 4 -------------

vec_tab4 <- data.frame(
  p = c(54:65),
  t = rep(c("Rural","Urban"),each=2,times=3),
  m = rep(c("URP","MRP","MMRP"),each=4)
  
)

table4 <- map_dfr(1:12,
                  function(i){
                    p = vec_tab4[i,]$p;
                    t = vec_tab4[i,]$t;
                    m = vec_tab4[i,]$m;
                    
                    nss68_read(p,t,m) %>% 
                      mutate_at(vars(starts_with("P"),one_of("ALL")),~as.numeric(.)) %>% 
                      pivot_longer(cols=-one_of("SL_NO","ITEM_CATEGORY","region","measure"),
                                   names_to="percentile",values_to="mpce")
                    
                  }
) %>% 
  pivot_wider(names_from=percentile,values_from=mpce) %>% 
  mutate(nfhs4_state = "India",
         nfhs4_statecode = "IN") %>% 
  dplyr::select(contains("nfhs4"),everything())

# All in Value by Row x Column.csv ---------
write_csv(table1,"data/Table 1 Persons by States x Classes of MPCE.csv")
write_csv(table2,"data/Table 2 Persons by States x Fractiles of MPCE.csv")
write_csv(table3,"data/Table 3 MPCE by States x Items.csv")
write_csv(table4,"data/Table 4 MPCE by Classes of MPCE x Items.csv")