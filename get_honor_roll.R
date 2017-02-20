library(rvest); library(stringr); library(dplyr)

honor_roll <- read_html("http://health.usnews.com/health-care/best-hospitals/articles/best-hospitals-honor-roll-and-overview")
top_20 <- honor_roll %>% html_nodes("table") %>% html_table(header = TRUE) %>% .[[1]]
top_20$link <- honor_roll %>% html_nodes("table a") %>% html_attr("href")

hospital_details <- vector(mode = "list", length = 20)

for(i in 1:20){
  print(i)
  print(top_20$link[i])
  link.html <- read_html(top_20$link[i])

  table_adult <- 
    link.html %>% html_nodes(".table#adult-rankings") %>% .[[1]] %>% 
    html_table(header = TRUE) %>% select(-4) %>% mutate(ranking_type = "adult", hospital = top_20$Hospital[i])

  if(length(link.html %>% html_nodes(".table#children-rankings")) != 0){
    table_children <- 
      link.html %>% html_nodes(".table#children-rankings") %>% .[[1]] %>% 
      html_table(header = TRUE) %>% select(-4) %>% mutate(ranking_type = "children", hospital = top_20$Hospital[i])
  } else{
    table_children <- 
      data_frame(Specialty = NA, `National Rank` = NA, `Overall Score` = NA)
  }
  
  hospital_details[[i]] <-   bind_rows(table_adult, table_children) %>% filter(!is.na(ranking_type))
}

hospital_details1 <- bind_rows(hospital_details)

write_csv(hospital_details1, "~/honor_roll_hospital_details.csv")
