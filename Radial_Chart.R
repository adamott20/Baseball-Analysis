library(tidyverse)
library(baseballr)


devers <- scrape_statcast_savant(start_date = "2019-03-15", playerid = playerid_lookup("Devers", "Rafael")$mlbam_id)

head(devers)


launch_radial_chart <- function(df, point_size=1){
  df <- filter(df, description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score"))
  df2 <- count(df, launch_speed_angle)
  ggplot() +
    geom_polygon(mapping=aes(x=c(-90,90,90,-90,-90), y=c(0,0,60,60,0)), alpha = .2, fill = "gold") +
    geom_polygon(mapping=aes(x=c(-90,-90,-10,2,4,13,22,-90),y=c(60,120,120,93,85,72,60,60)), alpha = .2, fill = "darkgreen")+
    geom_polygon(mapping=aes(x=c(4,26,30,50,50,4), y=c(120,98,98,334/3,120,120)), alpha = .8, fill = "red")+
    geom_polygon(mapping=aes(x=c(0,24,63/2,52,52,50,50,30,26,4,0,0), y=c(119,95,95,326/3,120,120,334/3,98,98,120,120,119)), fill = "pink", alpha=.5)+
    geom_polygon(mapping=aes(x=c(-10,2,4,13,22,34,41,41,32,26,26,20,20,22,0,0,-10), y =c(120,93,85,72,60,60,64,68,72,77,79,85,96,97,119,120,120)), alpha = .3, fill = "darkorange") +
    geom_polygon(mapping=aes(x=c(52,90,90,34,41,41,32,26,26,20,20,22,24,63/2,52,52),y=c(120,120,60,60,64,68,72,77,79,85,96,97,95,95,326/3,120)), fill="cornflowerblue", alpha=.2)+
    geom_point(data=df, mapping = aes(x=launch_angle, y=launch_speed), size = point_size)+
    scale_x_continuous(limits = c(-180,180), breaks = seq(-180,180, by = 30))+
    coord_polar(theta = "x", start = pi/2, direction = -1)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                  axis.title.x=element_blank(),
                                                                  axis.title.y=element_blank(),legend.position="none",
                                                                  panel.background=element_blank(),panel.border=element_rect(colour = "black", fill=NA, size=1),
                                                                  panel.grid.minor=element_blank(),plot.background=element_blank()) +
    annotate("text", x = 142, y = 111.6, label = paste("Barrels:",df2 %>% filter(launch_speed_angle == "6") %>% select(n)), col = "red", size = 3.2) +
                annotate("text", x = 155, y = 97, label = paste("Solid Contact:",df2 %>% filter(launch_speed_angle == "5") %>% select(n)), col = "pink", size = 3.2) +
                annotate("text", x = 171, y = 89, label = paste("Flares & Burners:",df2 %>% filter(launch_speed_angle == "4") %>% select(n)), col = "orange", size = 3.2) +
                annotate("text", x = -171, y = 89, label = paste("Topped:",df2 %>% filter(launch_speed_angle == "3") %>% select(n)), col = "darkgreen", size = 3.2) +
                annotate("text", x = -155, y = 97, label = paste("Hit Under:",df2 %>% filter(launch_speed_angle == "2") %>% select(n)), col = "cornflowerblue", size = 3.2) +
                annotate("text", x = -142, y = 111.6, label = paste("Weak Contact:",df2 %>% filter(launch_speed_angle == "1") %>% select(n)), col = "gold", size = 3.2)
}


launch_radial_chart(devers, point_size = .9)

