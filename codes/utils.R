## Utils

# ggplot theme ------------------------------------------------------------
RColorBrewer::display.brewer.all()
my_font <- "Palatino Linotype"
my_bkgd <- "white"
pal <- RColorBrewer::brewer.pal(9, "Spectral")


my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_rect(color="black"), 
                  strip.background = element_rect(color="black", fill="gray85"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 14, family = my_font),
                  legend.title = element_text(size=14),
                  plot.title = element_text(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=14, family=my_font),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 13, face="italic"))
theme_set(theme_bw() + my_theme)


