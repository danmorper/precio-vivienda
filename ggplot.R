library(dplyr)
library(ggplot2)
library(magrittr)

precios <- read.csv("mydatasets/house_price_index.csv")
precios_ordenados <- read.csv("mydatasets/index_ordenado.csv")
tipos_interes <- read.csv("mydatasets/tipos_interes.csv")

EU <- subset(precios_ordenados, Codigos == "EU27_2020")
ggplot(data = EU, aes(year,index)) + geom_line(color = "blue") + 
  ggtitle("Índice de vivienda en europa") +
  xlab("Año") +
  ylab("Indice") + 
  theme(panel.background = element_blank(), 
          plot.background = element_blank(),
          panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
          panel.grid.minor = element_blank()) + geom_text(aes(label = Deposit.facility), hjust = -0.1, vjust = 1) +
  geom_text(aes(label = `Fixed.rate.tenders.Fixed.rate`), hjust = -0.1, vjust = 1) +
  geom_text(aes(label = `Variable.rate.tenders.Minimum.bid.rate`), hjust = -0.1, vjust = 1) +
  geom_text(aes(label = Marginal.lending.facility), hjust = -0.1, vjust = 1)


write.csv(EU, "gpt.csv")

deposit <- tapply(tipos_interes$Deposit.facility, tipos_interes$Año, function(x) mean(x, na.rm = TRUE))
deposit %<>% as.data.frame()
deposit$Año <- row.names(deposit)
names(deposit) <- c("media_deposito", "Año")
ggplot(deposit, aes(Año, media_deposito, group = 1)) + geom_line(color="green") + 
  ggtitle("Facilidad de depósito") +
  xlab("Año") +
  ylab("%") + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())
