library(dplyr)
library(rvest)
library(magrittr)
library(zoo)
url <- "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/key_ecb_interest_rates/html/index.en.html"
# Introducimos la página web a buscar
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
#Creamos una función con read_html para que lea la página web.
pagina %>%  
  html_nodes("table") %>% 
  #Aquí indicamos que es una tabla lo que queremos extraer.
  .[[1]] %>% 
  #Aquí ponemos de que tabla del HTML se trata, 
  #en nuestro ejemplo es la tercera tabla de la web.
  html_table(fill=T, header=T) -> tipos_interes
tipos_interes %<>% as.data.frame(tipos_interes)

nombres <- names(tipos_interes)
names(tipos_interes) <- c("Año", nombres[2:3], "Fixed rate tenders Fixed rate", 
"Variable rate tenders Minimum bid rate", nombres[6:length(nombres)])
tipos_interes %<>% slice(2:(nrow(tipos_interes) - 2))
tipos_interes[tipos_interes=="-"] <- NA
tipos_interes[tipos_interes==""] <- NA

tipos_interes$Año <- na.locf(tipos_interes$Año)
write_csv(tipos_interes)
# tipos_interes <- tipos_interes %>% mutate_if(is.character, ~as.numeric(gsub("-", NA, .)))

# Minimum Bid Rate es el que puede tener más influencia
write.csv(tipos_interes, file = "tipos_interes.csv", row.names = FALSE)
