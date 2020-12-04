
# NFO ---------------------------------------------------------------------

---
# title: "Final Project"
# author: "Kovács Ádám József, Nguyen Nam Son"
# date: "04/12/2020"
---
  
# Setup -------------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, data.table, ggplot2, scales,
               StatsBombR, SBpitch, soccermatics,
               extrafont, ggupset, tibbletime,
               ggtext, ggrepel, glue, patchwork,
               cowplot, gtable, grid, magick, fmsb, RColorBrewer)

matches <- FreeMatches(FreeCompetitions())

df <- get.matchFree(subset(matches, match_id == 18236))

wd <- file.path("~", "eltecon-datascience")
setwd(wd)

#setwd("C:\\ELTECON\\eltecon-datascience")

# EDA ---------------------------------------------------------------------

#Statsbomb (the data provider) has a built in data cleaning function

df <- allclean(df)

#Since the name of the footballers is given as their birth names and many of them are known on simpler names, we correct for this

df_clean <- df %>% 
  mutate(player.name = case_when(
    player.name == "Lionel Andrés Messi Cuccittini" ~  "Lionel Messi",
    player.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
    player.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
    player.name == "Daniel Alves da Silva" ~ "Dani Alves",
    player.name == "Xavier Hernández Creus" ~ "Xavi",
    player.name == "Víctor Valdés Arribas" ~ "Victor Valdes",
    player.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
    player.name == "David Villa Sánchez" ~ "David Villa",
    player.name == "Andrés Iniesta Luján" ~ "Andres Iniesta",
    player.name == "Gerard Piqué Bernabéu" ~ "Gerard Pique",
    player.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
    player.name == "Seydou Kéita" ~ "Keita",
    player.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
    player.name == "Ji-Sung Park" ~  "Park Ji-Sung",
    player.name == "Patrice Evra" ~ "Patrice Evra",
    player.name == "Edwin van der Sar" ~ "Edwin van der Sar",
    player.name == "Paul Scholes" ~ "Paul Scholes",
    player.name == "Javier Hernández Balcázar" ~ "Chicharito",
    player.name == "Ryan Giggs" ~ "Ryan Giggs",
    player.name == "Nemanja Vidić" ~ "Nemanja Vidić",
    player.name == "Fábio Pereira da Silva" ~ "Fábio",
    player.name == "Wayne Mark Rooney" ~ "Wayne Rooney",
    player.name == "Luis Antonio Valencia Mosquera" ~ "Valencia",
    player.name == "Michael Carrick" ~ "Michael Carrick",
    player.name == "Rio Ferdinand" ~ "Rio Ferdinand",
    player.name == "Luís Carlos Almeida da Cunha" ~ "Nani"))

df_clean <- df_clean %>% 
  mutate(pass.recipient.name = case_when(
    pass.recipient.name == "Lionel Andrés Messi Cuccittini" ~  "Lionel Messi",
    pass.recipient.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
    pass.recipient.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
    pass.recipient.name == "Daniel Alves da Silva" ~ "Dani Alves",
    pass.recipient.name == "Xavier Hernández Creus" ~ "Xavi",
    pass.recipient.name == "Víctor Valdés Arribas" ~ "Victor Valdes",
    pass.recipient.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
    pass.recipient.name == "David Villa Sánchez" ~ "David Villa",
    pass.recipient.name == "Andrés Iniesta Luján" ~ "Andres Iniesta",
    pass.recipient.name == "Gerard Piqué Bernabéu" ~ "Gerard Pique",
    pass.recipient.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
    pass.recipient.name == "Seydou Kéita" ~ "Keita",
    pass.recipient.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
    player.name == "Ji-Sung Park" ~  "Park Ji-Sung",
    player.name == "Patrice Evra" ~ "Patrice Evra",
    player.name == "Edwin van der Sar" ~ "Edwin van der Sar",
    player.name == "Paul Scholes" ~ "Paul Scholes",
    player.name == "Javier Hernández Balcázar" ~ "Chicharito",
    player.name == "Ryan Giggs" ~ "Ryan Giggs",
    player.name == "Nemanja Vidić" ~ "Nemanja Vidić",
    player.name == "Fábio Pereira da Silva" ~ "Fábio",
    player.name == "Wayne Mark Rooney" ~ "Wayne Rooney",
    player.name == "Luis Antonio Valencia Mosquera" ~ "Valencia",
    player.name == "Michael Carrick" ~ "Michael Carrick",
    player.name == "Rio Ferdinand" ~ "Rio Ferdinand",
    player.name == "Luís Carlos Almeida da Cunha" ~ "Nani"))

#If a shot has no XG value it is because it had 0 chance of earning a goal

df_clean <- df_clean %>% mutate(shot.statsbomb_xg = if_else(is.na(shot.statsbomb_xg), 
                                                            0, shot.statsbomb_xg))

## removing NAs from the pass.outcome.name and pass.type.name columns
df_clean$pass.outcome.name <- ifelse(is.na(df$pass.outcome.name), "Complete", as.character(df$pass.outcome.name))
df_clean$pass.type.name <- ifelse(is.na(df$pass.type.name),"-",as.character(df$pass.type.name))
df_clean$pass.shot_assist <- ifelse(is.na(df$pass.shot_assist),FALSE, TRUE)
df_clean$pass.goal_assist <- ifelse(is.na(df$pass.goal_assist),FALSE, TRUE)
df_clean$pass.backheel <- ifelse(is.na(df$pass.backheel),FALSE, TRUE)
df_clean$pass.cross <- ifelse(is.na(df$pass.cross),FALSE, TRUE)
df_clean$pass.switch <- ifelse(is.na(df$pass.switch),FALSE, TRUE)
df_clean$pass.aerial_won <- ifelse(is.na(df$pass.aerial_won),FALSE, TRUE)

#Assign the xg values
df_clean_xg <- df_clean %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

#Join the correct xg values to the dataset
df_clean <- df_clean %>% 
  left_join(df_clean_xg, by = "team.name") %>% 
  mutate(player_label = case_when(
    shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {shot.statsbomb_xg %>%
                                             signif(digits = 2)} xG"), TRUE ~ ""))

# Descriptives ------------------------------------------------------------

#Since our dataset contatins a LOT of columns we do not need we filter on the ones that we are interested and on Barcelona and save it in a df entitled Messi

#We also create one entitled Rooney for the same attributes for Manutd

barca <- df_clean[,c("team.name",
                     'player.name',
                     'minute',
                     'type.name',
                     'play_pattern.name',
                     'pass.length',
                     'pass.cross',
                     'pass.switch',
                     'pass.backheel',
                     'pass.recipient.name',
                     'pass.shot_assist',
                     'pass.goal_assist',
                     'pass.height.name',
                     'pass.type.name',
                     'pass.outcome.name', 
                     'ball_receipt.outcome.name',
                     'dribble.outcome.name',
                     'shot.statsbomb_xg',
                     'shot.outcome.name',
                     'shot.type.name',
                     'foul_won.defensive',
                     'foul_won.advantage',
                     'location.x',
                     'location.y',
                     "pass.end_location.x",
                     "pass.end_location.y",
                     "carry.end_location.x",
                     "carry.end_location.y",
                     "shot.end_location.x",
                     "shot.end_location.y")] %>%
  filter(team.name == "Barcelona")

mun <- df_clean[,c("team.name",
                     'player.name',
                     'minute',
                     'type.name',
                     'play_pattern.name',
                     'pass.length',
                     'pass.cross',
                     'pass.switch',
                     'pass.backheel',
                     'pass.recipient.name',
                     'pass.shot_assist',
                     'pass.goal_assist',
                     'pass.height.name',
                     'pass.type.name',
                     'pass.outcome.name', 
                     'ball_receipt.outcome.name',
                     'dribble.outcome.name',
                     'shot.statsbomb_xg',
                     'shot.outcome.name',
                     'shot.type.name',
                     'foul_won.defensive',
                     'foul_won.advantage',
                     'location.x',
                     'location.y',
                     "pass.end_location.x",
                     "pass.end_location.y",
                     "carry.end_location.x",
                     "carry.end_location.y",
                     "shot.end_location.x",
                     "shot.end_location.y")] %>%
  filter(team.name == "Manchester United")

#From the above mentioned dataset we create datasets specifically for statistics about passes

passesmu <- mun %>% 
  filter(type.name == 'Pass' & player.name != is.na(player.name)) %>%
    group_by(player.name) %>%
      summarise(n = n(),
                avg_length = mean(pass.length),
                unsuccessful = sum(pass.outcome.name %in% c('Incomplete', 'Out')),
                successful = n - unsuccessful,
                crosses = sum(!is.na(pass.cross)),
                switch = sum(!is.na(pass.switch)),
                backheel = sum(!is.na(pass.backheel)),
                shot_assist = sum(!is.na(pass.shot_assist)),
                assist = sum(!is.na(pass.goal_assist)),
                forward = sum(pass.end_location.x > location.x))

passes <- barca %>% filter(type.name == 'Pass' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n(),
            avg_length = mean(pass.length),
            unsuccessful = sum(pass.outcome.name %in% c('Incomplete', 'Out')),
            successful = n - unsuccessful,
            crosses = sum(!is.na(pass.cross)),
            switch = sum(!is.na(pass.switch)),
            backheel = sum(!is.na(pass.backheel)),
            shot_assist = sum(!is.na(pass.shot_assist)),
            assist = sum(!is.na(pass.goal_assist)),
            forward = sum(pass.end_location.x > location.x))

passes

#Get the columns we want to use for this plot

passes2<-passes %>%
  select(1, 4:5)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  na.omit()

#Plot

ggplot(passes2, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + 
  geom_bar(stat="identity", colour="white")+
  labs(title = "Passes", subtitle = "UCL Final 2011-12",
       x="Players",y ='', caption ="Unsuccessful = Incomplete or out") +
  theme(axis.text.y = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24 , face="bold"),
        plot.subtitle=element_text(size=18, face="bold"),
        plot.caption=element_text(size =10),
        legend.title=element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom") + 
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "Unsuccessful","Successful")) +
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE)) 

#create datasets specifically for statistics about shots

shotsmu <- mun %>% filter(type.name == 'Shot' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n(),
            offtarget = sum(shot.outcome.name == "Off T"),
            blocked = sum(shot.outcome.name == "Blocked"),
            goal = sum(shot.outcome.name == "Goal"),
            saved = sum(shot.outcome.name == "Saved"))

shots <- barca %>% filter(type.name == 'Shot' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n(),
            offtarget = sum(shot.outcome.name == "Off T"),
            blocked = sum(shot.outcome.name == "Blocked"),
            goal = sum(shot.outcome.name == "Goal"),
            saved = sum(shot.outcome.name == "Saved"))

shots

#Get the columns we want to use for this plot

shots2<-shots %>%
  select(1, 3:6) %>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  na.omit()

#Plot

ggplot(shots2, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + 
  geom_bar(stat="identity", colour="white")+
  labs(title = "Shots", subtitle = "UCL Final 2011-12",
       x="Players",y ='')+
  theme(axis.text.y = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24 , face="bold"),
        plot.subtitle=element_text(size=18, face="bold"),
        plot.caption=element_text(size =10),
        legend.title=element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom") +
  scale_fill_manual(values = c("red", "green", "yellow", "blue"), labels = c( "Saved","Off-target","Goal","Blocked")) +
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE))

#create datasets specifically for statistics about dribbles

dribblemu <- mun %>% filter(type.name == 'Dribble' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n(),
            successful = sum(dribble.outcome.name == "Complete"),
            unsuccessful = sum(dribble.outcome.name == "Incomplete"))

dribble <- barca %>% filter(type.name == 'Dribble' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n(),
            successful = sum(dribble.outcome.name == "Complete"),
            unsuccessful = sum(dribble.outcome.name == "Incomplete"))

dribble

#Get the columns we want to use for this plot

dribble2<-dribble %>%
  select(1, 3:4) %>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  na.omit()

#Plot

ggplot(dribble2, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + 
  geom_bar(stat="identity", colour="white")+
  labs(title = "Dribbles", subtitle = "UCL Final 2011-12",
       x="Players",y ='')+
  theme(axis.text.y = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24 , face="bold"),
        plot.subtitle=element_text(size=18, face="bold"),
        plot.caption=element_text(size =10),
        legend.title=element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom") +
  scale_fill_manual(values = c("red", "green"), labels = c("Unsuccessful", "Successful")) +
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE)) 

#create datasets specifically for statistics about fould won

foulwonmu <- mun %>% filter(type.name == 'Foul Won' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(n = n())

foulwon <- barca %>% filter(type.name == 'Foul Won' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
    summarise(n = n())

foulwon

#Plot for Barcelona

ggplot(foulwon, aes(x =reorder(player.name, n), y = n)) + 
  geom_bar(stat="identity", colour="white")+
  labs(title = "Fouls won", subtitle = "UCL Final 2011-12",
       x="Players",y ='')+
  theme(axis.text.y = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24 , face="bold"),
        plot.subtitle=element_text(size=18, face="bold"),
        plot.caption=element_text(size =10),
        legend.title=element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom") +
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE))

# Progressive plays -------------------------------------------------------

togoal_carry <- barca %>% filter(type.name == 'Carry' & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(progresstogoal = sum(!is.na(sqrt((carry.end_location.y - location.y)^2)+(carry.end_location.x - location.x)^2)))

togoal_pass <- barca %>% filter(type.name == 'Pass' & pass.end_location.x>location.x & player.name != is.na(player.name)) %>%
  group_by(player.name) %>% 
  summarise(progresstogoalpass = sum(!is.na(pass.length)))

prog <- left_join(passes, shots, by = "player.name") %>% left_join(., dribble, by = "player.name") %>%
  left_join(.,togoal_carry, by = "player.name")%>%
  left_join(.,togoal_pass, by = "player.name") %>% na.omit() %>% mutate(progressgoalwards = progresstogoal + progresstogoalpass)

prog <- prog[3:5,]

ggplot(data=prog, aes(x=reorder(player.name, -progressgoalwards), y=progressgoalwards)) +
  geom_bar(stat="identity") +
  labs(title = "Forward Progress of BAR Players",
       subtitle = "Sum of progressive passes and ball carries",
       x = element_blank(),
       y = "Count") +
  theme_classic() +
  geom_label(aes(label = progressgoalwards), vjust = 0.5, position = position_dodge(0.9), color = "black", fontface = "bold", size = 4, show.legend = FALSE) +
  theme(legend.position = c(0.8, 0.7))

# Function for creating a pitch -------------------------------------------

createPitch <- function(grass_colour, line_colour, background_colour, goal_colour, BasicFeatures){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 0 # minimum length
  xmax <- 120 # maximum length
  
  # Defining features along the length
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  halfwayline <- 60
  sixYardDef <- 6
  sixYardOff <- 114
  penSpotDef <- 12
  penSpotOff <- 108
  
  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # other dimensions
  centreCirle_d <- 20     
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create center circle ####
  center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)
  
  if(BasicFeatures == TRUE){
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
    
  }else{
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) +
      # add the six yard box Defensive
      geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour)  +
      # add the six yard box offensive
      geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour) +
      # add centre circle 
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
      # add penalty spot left 
      geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour) + 
      # add penalty spot right
      geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour) + 
      # add centre spot 
      geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1) 
  }
  
  return(p)
  
}

# Pass network for Barca-------------------------------------------------

barcapasses <- df_clean %>%
  filter(type.name == 'Pass' & team.name == "Barcelona" & !is.na(player.name) & !is.na(pass.recipient.name)) %>% 
  group_by(player.name, pass.recipient.name) %>% 
  summarise(weight=n()) 

positions2 <- df_clean %>%
  filter(team.name == 'Barcelona') %>%
  group_by(player.name)%>%
  summarize(avg_position_x = mean(na.omit(location.x)), avg_position_y = mean(na.omit(location.y)))


forpassmap2 <- merge(barcapasses, positions2)

colnames(positions2)[1] <- "pass.recipient.name"
positions2

forpassmap2 <- left_join(forpassmap2, positions2, by = "pass.recipient.name")

myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(forpassmap2$weight)), name = "Nr. of passes")

m <- createPitch(
  "#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE
)  +
  geom_segment(
    data = forpassmap2, aes(x = avg_position_x.x, y = avg_position_y.x, xend = avg_position_x.y, yend = avg_position_y.y, color = weight),size = 1, alpha = 0.8)+
  geom_point(
    data = forpassmap2, aes(x = avg_position_x.x, y = avg_position_y.x, label = player.name), alpha = 0.7, size = 7, shape = 1
  )+ 
  geom_label(data = forpassmap2, aes(x = avg_position_x.x, y = avg_position_y.x, label=player.name),hjust=0, vjust=0, size = 3.5) +sc + labs(
    title = "Barcelona pass network", 
    subtitle = 'vs Manchester United',
    caption = "made by Ádám József Kovács and Nguyen Nam Son"
  )

m
# Radar plot for forward Barca trio ---------------------------------------------

radar <- left_join(passes[,c(1,2,4,5,11)], shots, by = "player.name") %>% left_join(., dribble, by = "player.name") %>% left_join(., foulwon, by = "player.name")
radar$pass_acc <- (radar$successful.x / radar$n.x) * 100
radar$shot_acc <- ((radar$goal + radar$saved) / radar$n.y) * 100
radar$conversion_rate <- (radar$goal / radar$n.y) * 100
radar$dribble_rate <- (radar$successful.y / radar$n.x.x) * 100
radar$forward_pass <- (radar$forward/radar$n.x) * 100
radar2 <- radar[radar$player.name %in% c("David Villa", "Lionel Messi", "Pedro"),]

# Construct the data set
data <- data.frame(Pass_Accuracy = c(100, 0, radar2$pass_acc),
                   Dribble_Success_Rate = c(100, 0, radar2$dribble_rate),
                   Shots_Accuracy = c(100, 0, radar2$shot_acc),
                   Conversion_Rate = c(100, 0, radar2$conversion_rate),
                   Forward_Pass_Rate = c(100, 0, radar2$forward_pass),
                   row.names = c("max", "min", "David Villa", "Messi", "Pedro"))

# Define fill colors
colors_fill <- c(scales::alpha("gray", 0.1),
                 scales::alpha("gold", 0.1),
                 scales::alpha("tomato", 0.1))

# Define line colors
colors_line <- c(scales::alpha("blue", 0.9),
                 scales::alpha("gold", 0.9),
                 scales::alpha("tomato", 0.9))

# Create plot
radarchart(data, 
           seg = 4,  # Number of axis segments
           title = "Performance of the BAR Forward Trio",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 4,
           plty = c(1,1,1),
           vlabels = c("Pass Accuracy", "Dribble Success Rate", "Shots Accuracy", "Conversion Rate", "Forward Pass Rate"),
           vlcex = 0.7)

# Add a legend
legend(x=1, 
       y=1.15, 
       legend = rownames(data[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)

# Radar plot for forward MU trio ---------------------------------------------

radarmu <- left_join(passesmu[,c(1,2,4,5,11)], shotsmu, by = "player.name") %>% left_join(., dribblemu, by = "player.name")  %>% left_join(., foulwonmu, by = "player.name")
radarmu$pass_acc <- (radarmu$successful.x / radarmu$n.x) * 100
radarmu$shot_acc <- ((radarmu$goal + radarmu$saved) / radarmu$n.y) * 100
radarmu$conversion_rate <- (radarmu$goal / radarmu$n.y) * 100
radarmu$dribble_rate <- (radarmu$successful.y / radarmu$n.x.x) * 100
radarmu$forward_pass <- (radarmu$forward/radarmu$n.x) * 100
radarmu2 <- radarmu[radarmu$player.name %in% c("Wayne Rooney", "Chicharito", "Nani"),]

# Construct the data set
datamu <- data.frame(Pass_Accuracy = c(100, 0, radarmu2$pass_acc),
                   Dribble_Success_Rate = c(100, 0, radarmu2$dribble_rate),
                   Shots_Accuracy = c(100, 0, radarmu2$shot_acc),
                   Conversion_Rate = c(100, 0, radarmu2$conversion_rate),
                   Forward_Pass_Rate = c(100, 0, radarmu2$forward_pass),
                   row.names = c("max", "min", "Chicarito", "Nani", "Rooney"))

# Define fill colors
colors_fill <- c(scales::alpha("gray", 0.1),
                 scales::alpha("gold", 0.1),
                 scales::alpha("tomato", 0.1))

# Define line colors
colors_line <- c(scales::alpha("blue", 0.9),
                 scales::alpha("gold", 0.9),
                 scales::alpha("tomato", 0.9))

# Create plot
radarchart(datamu, 
           seg = 4,  # Number of axis segments
           title = "Performance of the MU Forward Trio",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 4,
           plty = c(1,1,1),
           vlabels = c("Pass Accuracy", "Dribble Success Rate", "Shots Accuracy", "Conversion Rate", "Forward Pass Rate"),
           vlcex = 0.9)

# Add a legend
legend(x=1, 
       y=1.15, 
       legend = rownames(datamu[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)

# xG (expected goals) -----------------------------------------------------

#Plot the timeline of the game with the shots displayed in the size of their XG with the goal highlighted 

xg_timelineplot <- df_clean %>% 
  ggplot() +
  geom_segment(x = 0, xend = 95,
               y = 0, yend = 0) +
  geom_rect(data = df_clean %>% filter(shot.outcome.name == "Goal"),
            aes(xmin = minute - 1, xmax = minute + 1,
                ymin = -0.005, ymax = 0.005), 
            alpha = 0.3, fill = "green") +
  geom_vline(xintercept = c(0, 45, 95)) +
  geom_label_repel(data = df_clean %>% filter(shot.outcome.name == "Goal"),
                   aes(x = minute, y = 0,
                       color = team.name, label = player_label), 
                   nudge_x = 4, nudge_y = 0.003, show.legend = FALSE) +
  geom_point(data = df_clean %>% filter(shot.statsbomb_xg != 0),
             shape = 21,
             aes(x = minute, y = 0, 
                 size = shot.statsbomb_xg, fill = team.name)) +
  scale_color_manual(values = c("Barcelona" = "red",
                                "Manchester United" = "black")) +
  scale_fill_manual(values = c("Barcelona" = "red",
                               "Manchester United" = "white")) +
  facet_wrap(vars(team_label), ncol = 1) +
  scale_x_continuous(breaks = seq(0, 95, by = 5)) +
  scale_y_continuous(limits = c(-0.005, 0.005),
                     expand = c(0, 0)) +
  scale_size(range = c(2, 6)) +
  labs(caption = "By Kovács Ádám, Nguyen N. Son") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16, 
                                  face = "bold", color = "grey20"),
        plot.caption = element_text(color = "grey20",
                                    hjust = 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(x=2, label="Kick-off", y=-0.004), colour="black") +
  geom_text(aes(x=46, label="HT", y=-0.004), colour="black") +
  geom_text(aes(x=94, label="FT", y=-0.004), colour="black")

xg_timelineplot

# xGA ---------------------------------------------------------------------

# Non penalty XG and XG contribution of team members 

xGA = df_clean %>%
  filter(type.name=="Shot") %>%
    select(shot.key_pass_id, xGA = shot.statsbomb_xg)

shot_assists = left_join(df_clean, xGA, by = c("id" = "shot.key_pass_id")) %>%
  select(team.name, player.name, player.id, type.name, pass.shot_assist, pass.goal_assist, xGA) %>%
    filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)

player_xGA = shot_assists %>%
  group_by(player.name, player.id, team.name) %>%
    summarise(xGA = sum(xGA, na.rm = TRUE)) 

player_xG = df_clean %>%
  filter(type.name=="Shot") %>%
    filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
      group_by(player.name, player.id, team.name) %>%
        summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
          left_join(player_xGA) %>%
            mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE)) 

player_minutes = get.minutesplayed(df_clean)

player_minutes = player_minutes %>%
  group_by(player.id) %>%
    summarise(minutes = sum(MinutesPlayed))

player_xG_xGA = left_join(player_xG, player_minutes) %>%
  mutate(nineties = minutes/90,
         xG_90 = round(xG/nineties, 2),
         xGA_90 = round(xGA/nineties,2),
         xG_xGA90 = round(xG_xGA/nineties,2))

chart = player_xG_xGA %>%
  ungroup() %>%
    top_n(n = 7, w = xG_xGA90)

chart<-chart %>%
  select(1, 10:11)%>%
    pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
      na.omit()

ggplot(chart, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) +
  geom_bar(stat="identity", colour="white")+
  labs(title = "Expected Goal Contribution", subtitle = "UCL Final 2011-12",
       x="", y="Per 90",caption ="nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted")+
  theme(axis.text.y = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24 , face="bold"),
        plot.subtitle=element_text(size=18, face="bold"),
        plot.caption=element_text(size =10),
        legend.title=element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom") + 
  scale_fill_manual(values=c("blue", "red"), labels = c( "xG Assisted","NPxG")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + #4
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))

# Shot map of Lionel Messi ------------------------------------------------

shots = df_clean %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Lionel Messi")

#Get colors to be used on the plot

shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F",
                     "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000")

#Draw the football pitch

ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_segment(
    data = shots,aes(x = location.x, y = location.y, xend = shot.end_location.x, yend = shot.end_location.y), arrow = arrow(length = unit(0.08,"inches")), alpha = 0.7 
  ) +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name),
             size = 6) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13, hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.margin = margin(c(10, 5, -30, 20)),
        legend.key.size = unit(1.0, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100)) +
  labs(title = "Lionel Messi, Shot Map", subtitle = "UCL Final 2011") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,1), oob=scales::squish, name = "Expected Goals Value") +
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") +
  guides(fill = guide_colourbar(title.position = "top"), 
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) +
  coord_flip(xlim = c(85, 125))

# Comparisons of Messi and Rooney -----------------------------------------

# Barplot comparison ----------------------------------

radarvs <- rbind(radar2, radarmu2)
radarvs2 <- melt(radarvs, id.vars = "player.name")
barvsrm <- radarvs2[(radarvs2$player.name %in% c("Wayne Rooney", "Lionel Messi") & radarvs2$variable %in% c("n.x", "forward", "n.y", "n.x.x", "n.y.y")),]

ggplot(barvsrm, aes(variable, value, fill = player.name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Messi and Rooney's performance in the UCL Final",
       subtitle = "Messi performs better in every aspects of the game",
       x = "Metrics",
       y = "Count",
       fill = "Player") +
  scale_x_discrete(labels = c("No. Pass", "No. Forward Pass", "No. Shots", "No. Dribbles", "No.Fouls Won")) +
  theme_classic() +
  geom_label(aes(label = value), vjust = 0.5, position = position_dodge(0.9), color = "black", fontface = "bold", size = 4, show.legend = FALSE) +
  theme(legend.position = c(0.8, 0.7))

# Radar comparison ------------------------------------

radarvs <- radarvs[(radarvs$player.name %in% c("Wayne Rooney", "Lionel Messi")),]

datavs <- data.frame(Pass_Accuracy = c(100, 0, radarvs$pass_acc),
                     Dribble_Success_Rate = c(100, 0, radarvs$dribble_rate),
                     Shots_Accuracy = c(100, 0, radarvs$shot_acc),
                     Conversion_Rate = c(100, 0, radarvs$conversion_rate),
                     Forward_Pass_Rate = c(100, 0, radarvs$forward_pass),
                     row.names = c("max", "min", "Messi", "Rooney"))

# Define fill colors
colors_fill <- c(scales::alpha("red", 0.1),
                 scales::alpha("blue", 0.1))

# Define line colors
colors_line <- c(scales::alpha("red", 0.9),
                 scales::alpha("blue", 0.9))

# Create plot
radarchart(datavs, 
           seg = 4,  # Number of axis segments
           title = "Messi vs Rooney",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 4,
           plty = c(1,1),
           vlabels = c("Pass Accuracy", "Dribble Success Rate", "Shots Accuracy", "Conversion Rate", "Forward Pass Rate"),
           vlcex = 0.9)

# Add a legend
legend(x=1, 
       y=1.15, 
       legend = rownames(datavs[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)

# Pass plot comparison ----------------------------------------------------

#Create dataframe to be used for next plot by filtering on passes and Messi

Messiforwardpasses <- df_clean %>% filter(type.name == "Pass", player.name == "Lionel Messi", pass.end_location.x>location.x)

#Plot

p <- createPitch(
  "#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE
) +
  geom_point(
    data = Messiforwardpasses, aes(x = location.x, y = location.y), alpha = 0.7
  ) +
  geom_segment(
    data = Messiforwardpasses, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y, colour = pass.outcome.name),alpha = 0.7, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  scale_colour_manual(
    values = c("blue", "red", "green") , name = "Outcome", labels = c("Successful", "Unsuccessful", "Resulted in shot"), guide = FALSE
  ) +
  scale_y_reverse()+ 
  geom_segment(
    data = Messiforwardpasses[Messiforwardpasses$pass.shot_assist == TRUE, ], aes(x = location.x, y = location.y,  xend = pass.end_location.x, yend = pass.end_location.y, color = "purple"), alpha = 0.7, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  labs(
    title = "Progressive passes of Messi", 
    subtitle = 'vs Manchester United'
  ) +
  geom_text(
    aes(x = 2, y=26,label = paste("Forward passes completed: ", nrow(Messiforwardpasses))), hjust=0, vjust=0.5, size = 4.5, colour = "black"
  ) +
  geom_text(
    aes(x = 2, y=30,label = paste("Successful passes: ", sum(Messiforwardpasses$pass.outcome.name == "Complete"))), hjust=0, vjust=0.5, size = 4, colour = "blue"
  ) +
  geom_text(
    aes(x = 2, y=34,label = paste("Resulted in shot: ", sum(Messiforwardpasses$pass.shot_assist == TRUE))), hjust=0, vjust=0.5, size = 4, colour = "green"
  ) +
  draw_image("C:\\ELTECON\\eltecon-datascience\\messi4.png",  x = 30, y = -10, scale = 30)

Rooneyforwardpasses <- df_clean %>% filter(type.name == "Pass", player.name == "Wayne Rooney", pass.end_location.x>location.x)

pb <- createPitch(
  "#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE
) +
  geom_point(
    data = Rooneyforwardpasses, aes(x = location.x, y = location.y), alpha = 0.7
  ) +
  geom_segment(
    data = Rooneyforwardpasses, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y, colour = pass.outcome.name),alpha = 0.7, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  scale_colour_manual(
    values = c("blue", "red", "purple", "green") , name = "Outcome", labels = c("Successful", "Unsuccessful", "Out of play", "Resulted in shot")
  ) +
  geom_segment(
    data = Rooneyforwardpasses[Rooneyforwardpasses$pass.shot_assist == TRUE, ], aes(x = location.x, y = location.y,  xend = pass.end_location.x, yend = pass.end_location.y, color = "purple"), alpha = 0.7, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  scale_y_reverse()+ 
  labs(
    title = "Progressive passes of Rooney", 
    subtitle = 'vs Barcelona',
    caption = "made by Ádám József Kovács and Nguyen Nam Son"
  ) +
  geom_text(
    aes(x = 2, y=26,label = paste("Forward passes completed:", nrow(Rooneyforwardpasses))), hjust=0, vjust=0.5, size = 4.5, colour = "black"
  ) +
  geom_text(
    aes(x = 2, y=30,label = paste("Successful passes: ", sum(Rooneyforwardpasses$pass.outcome.name == "Complete"))), hjust=0, vjust=0.5, size = 4, colour = "blue"
  ) +
  geom_text(
    aes(x = 2, y=34,label = paste("Resulted in shot: ", sum(Rooneyforwardpasses$pass.shot_assist == TRUE))), hjust=0, vjust=0.5, size = 4, colour = "green"
  )  +
  draw_image("C:\\ELTECON\\eltecon-datascience\\Wayne-Rooney.png",  x = 30, y = -10, scale = 30)

comp<- p + pb

comp

# Ball carry comparison ---------------------------------------------------

#Create dataframes to be used for plot by filtering on type of event and Messi

Messicarry <- df_clean %>% filter (type.name == 'Carry', player.name == 'Lionel Messi')

Messicarry$under_pressure <- ifelse(is.na(Messicarry$under_pressure), FALSE, TRUE)

Messidribble <- df_clean %>% filter(type.name == 'Dribble', player.name == 'Lionel Messi')


p2 <- createPitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE) +
  geom_point(
    data = Messicarry, aes(x = ifelse(carry.end_location.x>location.x, location.x,NA), y = location.y), alpha = 0.7) + 
  geom_segment(
    data = Messicarry, aes(x = ifelse(carry.end_location.x>location.x,location.x,NA), y = location.y, xend = carry.end_location.x, yend = carry.end_location.y, colour = under_pressure),alpha = 0.9, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  scale_colour_manual(
    values = c("blue", "red") , name = "Under Pressure", labels = c('Not', 'Yes'), guide = FALSE
  ) +
  scale_y_reverse()+ 
  labs(
    title = "Ball carries and dribbles by Messi", 
    subtitle = 'vs Manchester United'
  ) +
  geom_point(
    data = Messidribble, aes(x = location.x, y = location.y, shape = dribble.outcome.name), size = 4, alpha = 0.8, color = 'green'
  ) +
  scale_shape_manual(
    values = c("O", 'X'), name = "Dribble", labels = c('Complete','Incomplete' ), guide = FALSE
  ) +
  geom_text(
    aes(x = 2, y=26,label = paste("Carries completed: ", nrow(Messicarry))), hjust=0, vjust=0.5, size = 4.5, colour = "black"
  ) +
  geom_text(
    aes(x = 2, y=29,label = paste("Carries under pressure: ", sum(Messicarry$under_pressure==TRUE))), hjust=0, vjust=0.5, size = 4, colour = "red"
  ) +
  geom_text(
    aes(x = 2, y=32,label = paste("Dribbles attempted: ", nrow(Messidribble))), hjust=0, vjust=0.5, size = 4.5, colour = "green"
  ) +
  geom_text(
    aes(x = 2, y=35,label = paste("Dribbles completed: ", sum(Messidribble$dribble.outcome.name=='Complete'))), hjust=0, vjust=0.5, size = 4, colour = "green"
  ) +
  draw_image("C:\\ELTECON\\eltecon-datascience\\messi4.png",  x = 30, y = -10, scale = 30)

#create same dataframes for Rooney

Rooneycarry <- df_clean %>% filter (type.name == 'Carry', player.name == 'Wayne Rooney')

Rooneycarry$under_pressure <- ifelse(is.na(Rooneycarry$under_pressure), FALSE, TRUE)

Rooneydribble <- df_clean %>% filter(type.name == 'Dribble', player.name == 'Wayne Rooney')

p2b <- createPitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE) +
  geom_point(
    data = Rooneycarry, aes(x = ifelse(carry.end_location.x>location.x, location.x,NA), y = location.y), alpha = 0.7) + 
  geom_segment(
    data = Rooneycarry, aes(x = ifelse(carry.end_location.x>location.x,location.x,NA), y = location.y, xend = carry.end_location.x, yend = carry.end_location.y, colour = under_pressure),alpha = 0.9, arrow = arrow(length = unit(0.08,"inches"))
  ) +
  scale_colour_manual(values = c("blue", "red") , name = "Under Pressure", labels = c('Not', 'Yes')) +
  scale_y_reverse()+ 
  labs(
    title = "Ball carries and dribbles by Rooney", 
    subtitle = 'vs Barcelona',
    caption = "made by Ádám József Kovács and Nguyen Nam Son"
  ) +
  geom_point(
    data = Rooneydribble, aes(x = location.x, y = location.y, shape = dribble.outcome.name), size = 4, alpha = 0.8, color = 'green'
  ) +
  scale_shape_manual(
    values = c("O", 'X'), name = "Dribble", labels = c('Complete','Incomplete' )
  ) +
  geom_text(
    aes(x = 2, y=26,label = paste("Carries completed: ", nrow(Rooneycarry))), hjust=0, vjust=0.5, size = 4.5, colour = "black"
  ) +
  geom_text(
    aes(x = 2, y=29,label = paste("Carries under pressure: ", sum(Rooneycarry$under_pressure==TRUE))), hjust=0, vjust=0.5, size = 4, colour = "red"
  ) +
  geom_text(
    aes(x = 2, y=32,label = paste("Dribbles attempted: ", nrow(Rooneydribble))), hjust=0, vjust=0.5, size = 4.5, colour = "green"
  ) +
  geom_text(
    aes(x = 2, y=35,label = paste("Dribbles completed: ", sum(Rooneydribble$dribble.outcome.name=='Complete'))), hjust=0, vjust=0.5, size = 4, colour = "green"
  ) +
  draw_image("C:\\ELTECON\\eltecon-datascience\\Wayne-Rooney.png",  x = 30, y = -10, scale = 30)

comp2 <- p2 + p2b 

comp2

######################
#     END OF CODE    #
######################
