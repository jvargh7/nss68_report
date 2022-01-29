nss68_read <- function(tabula_page,title,mrp){
  
  page_variables = nss68_variables %>% 
    dplyr::filter(page == (tabula_page+1)) %>% 
    dplyr::select(variable) %>% 
    pull()
  
  
  csv_data = read_csv(paste0("raw/tabula-KI-68th-HCE Appendix A-",tabula_page,".csv")) %>% 
    mutate_all(~as.character(.)) %>% 
    dplyr::select(-one_of("X3"))
  
  if("X1" %in% colnames(csv_data)){
    x1_data = csv_data %>% 
      dplyr::filter(!is.na(X1)) 
    
    
    
    x1_data <- x1_data[,-length(names(x1_data))]
    
    names(x1_data) <- names(csv_data)[-1]
    nonx1_data = csv_data %>% 
      dplyr::filter(is.na(X1)) 
    
    csv_data <- bind_rows(nonx1_data,
                          x1_data)
    
    
    
  }
  
  csv_data %>% 
    dplyr::select(-one_of("X1")) %>% 
    rename_all(~page_variables) %>% 
    mutate(region = title,
           measure = mrp) %>% 
    return(.)
  
}
