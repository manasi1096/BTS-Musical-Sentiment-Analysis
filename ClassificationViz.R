#Code to create the four quadrant graph


ClassificationViz %>% 
  rename("Energy"="energy",
         "Valence" = "valence") %>% 
  mutate(quadrant = case_when(Valence > x_mid & Energy> y_mid   ~ "Q1",
                              Valence <= x_mid & Energy > y_mid  ~ "Q2",
                              Valence <= x_mid & Energy<= y_mid ~ "Q3",
                              TRUE                                         ~ "Q4")) %>% 
  mutate(Size = case_when(track_name %like% "Boy With" ~ 6,
                          track_name %like% "Persona"~4,
                          track_name %like% "Shadow"~5,
                          track_name %like% "Sia" ~ 4,
                          track_name %like% "UG" ~ 4,
                          track_name %like% "Zero" ~ 5,
                          track_name %like% "Jama" ~ 5,
                          track_name %like% "Filter" ~5,
                          track_name %like% "Friends" ~ 4,
                          track_name %like% "Time" ~4,
                          track_name %like% "Bull" ~4,
                          track_name %like% "Moon" ~ 3.5,
                          TRUE
                          ~3))%>% 
  ggplot(aes(x = Valence, y = Energy, color = quadrant)) +
  geom_vline(xintercept = x_mid) + # plot vertical line
  geom_hline(yintercept = y_mid) + # plot horizontal line
  geom_point(show.legend = F,aes(size = Size))+
  geom_label_repel(label = chart$track_name, label.size = 0.03, colour = "#08306b",
                   segment.colour = "#555555", box.padding = 0.16)+
  labs(title="Mood Board : Map Of The Soul 7",
       subtitle = "Musical Sentiment Analysis", size=8)+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(size=.4),
        legend.position = NULL)+theme_wsj(color = "green")+t1+
  geom_label(aes(x = 0.15, y = 0.9, label = "Turbulent/Angry"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "#fff7f3", 
             label.size = NA, 
             family="Arial Rounded MT Bold", 
             size = 4)+
  geom_label(aes(x = 0.75, y = 0.9, label = "Happy/Joyful"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "#fff7f3", 
             label.size = NA, 
             family="Arial Rounded MT Bold", 
             size = 4)+
  geom_label(aes(x = 0.15, y = 0.65, label = "Sad/Depressing"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "#fff7f3", 
             label.size = NA, 
             family="Arial Rounded MT Bold", 
             size = 4)+
  geom_label(aes(x = 0.75, y = 0.65, label = "Chill/Peaceful"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "#fff7f3", 
             label.size = NA, 
             family="Arial Rounded MT Bold", 
             size = 4)+  
  scale_color_manual(values = c("#ffff00", "#de2d26", "#045a8d","#31a354"))+
  theme(axis.title=element_text(size=12))
