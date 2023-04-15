library(dplyr)
library(ggplot2)
library(magrittr)
precios <- read.csv("mydatasets/house_price_index.csv")
precios_ordenados <- read.csv("mydatasets/index_ordenado.csv")
tipos_interes <- read.csv("mydatasets/tipos_interes.csv")
names(tipos_interes) <- c("year", names(tipos_interes)[2:ncol(tipos_interes)])
EU <- subset(precios_ordenados, Codigos == "EU27_2020")
ggplot(data = EU, aes(year,index)) + geom_line(color = "blue") + 
  ggtitle("Índice de vivienda en europa") +
  xlab("Año") +
  ylab("Indice") + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())


write.csv(EU, "gpt.csv")

deposit <- tapply(tipos_interes$Deposit.facility, tipos_interes$year, function(x) mean(x, na.rm = TRUE))
deposit %<>% as.data.frame()
deposit$year <- row.names(deposit)
names(deposit) <- c("media_deposito", "year")
ggplot(deposit, aes(year, media_deposito, group = 1)) + geom_line(color="green") + 
  ggtitle("Facilidad de depósito") +
  xlab("Año") +
  ylab("%") + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())

# Create a new data frame with all years from min_year to max_year
all_years <- data.frame(year = 1999:2023)

# Merge the all_years data frame with precios_ordenados to add missing years
deposit_all_years <- merge(all_years, deposit, by = "year", all.x = TRUE)

# Fill missing index values with the previous year's index value
deposit_all_years$media_deposito <- zoo::na.locf(deposit_all_years$media_deposito)

# Replace the original precios_ordenados data frame with the updated data
deposit <- tipos_all_years


ggplot(deposit, aes(year, media_deposito, group = 1)) + geom_line(color="green") + 
  ggtitle("Facilidad de depósito") +
  xlab("Año") +
  ylab("%") + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
deposit_y_index <- left_join(EU, deposit, by = "year")

ggplot(deposit_y_index, aes(x = year)) + geom_line(aes(y = index), color = "blue") + geom_line(aes(y = media_deposito*100), color = "green") + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Indice de precios de vivienda",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Facilidad de deposito")) + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
