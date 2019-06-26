







social_capital <- tibble(city = character(0), neighbourhood_name = character(0), population = numeric(0),
                         households = numeric(0), 
                   
n = 1

repeat{
  
  CTs_neighbourhood <- CTs_canada %>% 
    st_join(st_buffer(neighbourhoods[n, "geometry"], 200),
            join = st_within, left = FALSE)  
  
  social_capital[n, 1] <- cityname
  
  social_capital[n, 2] <- neighbourhoods$neighbourhood[n]
  
  social_capital[n, 3] = sum(CTs_neighbourhood$population)
  
  social_capital[n, 4] = sum(CTs_neighbourhood$households)
  
  
  
  
  n = n+1
  
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}




neighbourhoods
CTs_canada <- CTs_canada %>% 
  st_transform(32618)

CTs_canada
n = 1




neighbourhoods[1, 1]

CTs_canada
neighbourhoods
