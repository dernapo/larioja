##########################################################################
#### Create a map from La Rioja
#### Credits: https://danielredondo.com/posts/20200125_joy_division/
##########################################################################

## Load libraries -----
pacman::p_load(data.table, here, ggplot2, ggridges, mapproj, colorfindr)

## Load data ----------
larioja_dt <- fread(here("data", "larioja3.csv"), col.names = c("fid", "Elev", "Lon", "Lat"))

## Prepare colors ----
download.file("https://nuevecuatrouno.com/wp-content/uploads/2015/06/Bandera_de_La_Rioja-1.jpg", 
              here("data", "bandera_rioja.jpg"))

rioja_colors <- get_colors(here("data", "bandera_LaRioja.jpg"))
setDT(rioja_colors)
rioja_colors_list <- rioja_colors[order(-col_freq)][1:4]$col_hex

breaks_rioja <- min(larioja_dt$Lat) + 0:4 * (max(larioja_dt$Lat) - min(larioja_dt$Lat)) / 4
larioja_dt$rioja_color <- cut(larioja_dt$Lat, breaks = breaks_rioja, include.lowest = T, labels = rioja_colors_list)

## Visualization -----
ggplot(larioja_dt, aes(x = Lon, y = Lat, group = Lat, height = Elev)) +
  geom_density_ridges(aes(colour = rioja_color), stat = "identity", scale = 7, fill = "black") +
  scale_color_manual(values = rioja_colors_list) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "black"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "white", size = 18),
        legend.position = "none", 
        plot.title = element_text(size = 18, hjust = 0.5, face = "italic", color = "white")) +
  coord_map() +
  labs(title = "La Rioja", colour = "white", x = NULL, y = NULL)


## Save ----

ggsave(here("output", "larioja_tr.png"), dpi = 300, bg = "transparent")
