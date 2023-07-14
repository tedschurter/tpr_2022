library(tidyverse)
library(tigris)
library(patchwork)
library(ggrepel)
library(ggtext)
library(ggimage)


# import nonresident abortion data
nres <- read_csv("exported_data/nonresident_ab_19_22.csv")

# panel color
panel_c <- "#fdfdf2"

# set custom theme ####
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme( 
      #grid elements
      panel.grid.major.y = element_line(color = "gray85", size = .25),
      #panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(), 
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      
      #axis.line = element_line(color = "gray90"),
      #axis.line.y = element_blank(),
      
      axis.text = element_text(color = "gray55",
                               size  = rel(.65)),
      axis.title.x = element_blank(),
      # element_text(color = "gray55",
      # size = rel(.65), hjust = 0),
      axis.title.y = element_blank(),
      # element_text(color = "gray55",
      #             size = rel(.6)),
      
      plot.title = element_textbox_simple(size = rel(1.5), color = "gray25", 
                                          hjust = 0,lineheight = 1, 
                                          margin = margin(0.2, 0, .2, 0, unit = "cm"),
                                          family = "serif",face = "plain"),
      plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                             lineheight = 1,margin = margin(0.1, 0, .4, 0, unit = "cm"),),
      plot.caption = element_textbox_simple(family = "sans", size = rel(.7),
                                            color = "gray40", halign = 1,
                                            lineheight = 1.2, margin = margin(0, 0, .1, 0, unit = "cm"),),
      plot.title.position = "plot",
      plot.caption.position = "panel", 
      plot.margin = margin(.5, .5, .5,.5, unit = "cm"),
      plot.background = element_rect(fill  = panel_c,
                                     color = panel_c),
      panel.background = element_rect(fill = panel_c,
                                      color = panel_c)
    )
}


# set colors ####

# kentucky color
ky_c <-  "#fc8d59"
# ohio color
oh_c <-  "#4575b4"
# Tennessee color
tn_c <-  "#91bfdb"

s_cols <- c("Kentucky" = ky_c,
            "Illinois" = "gray",
            "Michigan" = "gray",
            "Ohio"     = oh_c,
            "Tennessee" = tn_c,
            "Other"    = "gray")

# create plot showing nonresident abortions by state and year
nres %>% group_by(state) %>% arrange(desc(abortions)) %>% ungroup() %>% 
  mutate(
    year = as.factor(year),
    state2 = tidytext::reorder_within(state, by = rev(abortions), within = year)) %>%
  # added state2 as additional column to retain ability to scale_fill_manual via state column
  ggplot(aes(state2, abortions, fill = state))+
  geom_col(show.legend = F)+
  scale_fill_manual(values = s_cols)+
  facet_wrap(~year, scales="free_y")+
  coord_flip()+
  tidytext::scale_x_reordered()+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = .25),
    plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                           lineheight = 1,margin = margin(.25, 0, .4, 0, unit = "cm"),),
  )+
  labs(title =
         paste0("Indiana included non-resident abortion totals for individual 
      states starting in 2019.<span style = 'color:",ky_c,"'> Kentucky</span> 
      residents recieved the most non-resident abortions in Indiana while <span 
      style = 'color:",tn_c,"'>Tennessee</span>, which had the second-highest totals 
      in 2019, was the lowest state in 2021 and 2022."),
       subtitle = 
         paste0("<span style = 'color:",ky_c,"'> Kentucky</span> and 
      <span style = 'color:",tn_c,"'>Tennessee</span> enacted near-total abortion 
      bans after the Dobbs decision in 2022; <span style = 'color:",oh_c,"'>Ohio's
      </span>ban is currently blocked while a case proceeds. Illinois and Michigan 
      retain abortion rights.")
  )

#


# create state map for nonresident abortions #

# using tigris package

# import states geometry  ####
states <- states(cb=T)

# filter to neighboring states
states <- states %>% filter(NAME == "Illinois" | NAME == "Michigan" | NAME == "Kentucky" |
                              NAME == "Tennessee" | NAME == "Indiana" | NAME == "Ohio")

# order states as factor
nres$state <- factor(nres$state, levels = c("Kentucky", "Ohio", "Tennessee", 
                                            "Illinois", "Michigan", "Other"))

# add colors to match bar chart #### 
states$color[states$NAME == "Kentucky"]  <- ky_c 
states$color[states$NAME == "Ohio"]      <- oh_c
states$color[states$NAME == "Tennessee"] <- tn_c
states$color[states$NAME == "Illinois" ] <- "gray"
states$color[states$NAME == "Michigan"]  <- "gray"
states$color[states$NAME == "Other"]     <- "gray"

# visualize neighboring states with abortion bans

ggplot()+
  geom_sf(data = states,
          fill = states$color)+
  
  theme_void()+
  t_theme()+
  theme(
    axis.text = element_blank()
  )

#### # create combo bar chart inset map ####

# bar chart:

pt <- nres %>% group_by(state) %>% arrange(desc(abortions)) %>% ungroup() %>% 
  ggplot(aes(year, abortions, fill = state))+
  geom_col(show.legend = F)+
  scale_fill_manual(values = s_cols)+
  facet_grid(~state, scales="free_y", switch = "x")+
  scale_y_continuous(limits = c(0, 1000),
                     breaks = c(0, 250, 500, 750, 1000),
                     labels = c("0", "250", "500", "750", "1,000"))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    
    plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                           lineheight = 1,margin = margin(.25, 0, .4, 0, unit = "cm"),),
  )

# map:

p1 <- ggplot()+
  geom_sf(data = states,
          fill = states$color)+
  t_theme()+
  theme(
    axis.text = element_blank(),
  )

# inset text 

map_label <- "<span style = 'color:#fc8d59;'> Kentucky</span>, <span style = 
      'color:#376bae;'>Ohio</span> and <span style = 'color:#82b6d6;'>Tennessee</span> 
      enacted near-total abortion bans after the Dobbs decision in 2022. Like Indiana, 
      <span style = 'color:#376bae;'>Ohio's </span>ban is currently blocked while 
      litigation proceeds. Illinois and Michigan retain abortion rights."

# inset text object

text1 <- ggplot(data = tibble(x = 0, y = 1, label = map_label)) +
  aes(x = x, y = y, label = label) +
  geom_textbox(
    box.color = NA,
    color = "gray25",
    fill = NA,
    width = unit(10, "cm"),
    hjust = 0,
    vjust = 1 
  ) +
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1))+
  theme_void() 

# add it all up with patchwork 

pt + inset_element(p1, 
                   left = 0.75, 
                   bottom = 0.1, 
                   right = 1, 
                   top = 1.25,
                   align_to = "plot")+
  inset_element(text1, 
                left = 0.36, 
                bottom = 0.45, 
                right = .76, 
                top = .80,
                align_to = "plot")+
  
  plot_annotation(
    theme = t_theme(),
    title =
      paste0("The number of nonresident abortions in Indiana in 2022 increased 
      dramatically following the Dobbs decision ending federal abortion protections. 
      <span style ='color:#fc8d59;'>Kentucky</span> residents received ",
             nres$abortions[nres$year == 2022 & nres$state == "Kentucky"],
             ", the most in 2022 and a ",
             round(100*((nres$abortions[nres$year == 2022 & nres$state == "Kentucky"]-
                           nres$abortions[nres$year == 2021 & nres$state == "Kentucky"])/
                          nres$abortions[nres$year == 2021 & nres$state == "Kentucky"])),
             "% increase from 2021. <span style ='color:#4575b4;'>Ohio's</span> totals increased ",
             formatC(round(100*((nres$abortions[nres$year == 2022 & nres$state == "Ohio"]-
                                   nres$abortions[nres$year == 2021 & nres$state == "Ohio"])/
                                  nres$abortions[nres$year == 2021 & nres$state == "Ohio"])),big.mark = ","),
             "% from 2021.")
  )

ggsave("plots/combo_bar_map.svg",
       width = 3200, height = 1700,
       units = "px")


# plot showing distance from facilities to different states ####

dist <- read_csv("exported_data/facility_state_distance.csv") %>% 
  filter(state != "Indiana")



#
# create one small png image of each state outline and color according to 
# previous charts - ie gray for states with no bans, color for those with bans


for (i in 1:length(states$NAME[states$NAME != "Indiana"])) {
  
  # set aspect ratio for image 
  asp_ratio <- 1.618
  # create list of states to iterate through
  state <- unique(states$NAME[states$NAME != "Indiana"])[i]
  
  ggplot()+
    geom_sf(data = states %>% filter(NAME == state),
            fill =  states$color[states$NAME == state])+
    theme_void()+
    t_theme()+
    theme(
      axis.text = element_blank(),
      plot.margin = margin(0, 0, 0,0, unit = "cm"),
      # in order to not obscure states on the timeline, change background of plot 
      # and panelto transparent
      plot.background = element_rect(fill   = "transparent",
                                     color  = "transparent"),
      panel.background = element_rect(fill  = "transparent",
                                      color = "transparent")
    )
  
  ggsave(plot = last_plot(), 
         filename = paste0("plots/png/",state,".png"),
         width  = 100, 
         height = 100, units = "px")
}



# create scatter plot showing distance of each facility from neighboring states ####
# with state images as points 

# import provider location data
ploc <- read_csv("exported_data/provider_residency.csv") %>% 
  filter(year == 2022)

# order facility by number of nonresident abortions
fac_nr <- ploc %>% arrange(desc(nonresident)) %>% 
  select(facility)

# order dist$facility column by above vector
dist$facility <- factor(dist$facility, ordered = T,levels = fac_nr$facility)


# dataframe s_points below from first attempt using images - abandoned in favor of easier
# implementation with ggtext - no aspect ratio issues


s_points <- data.frame(state = states$NAME[states$NAME!= "Indiana"],
           image = c("plots/png/Kentucky.png",
                     "plots/png/Tennessee.png",
                     "plots/png/Michigan.png",
                     "plots/png/Illinois.png",
                     "plots/png/Ohio.png")
           )

# join distance with points directory address
dist_states <- inner_join(dist, s_points, by = "state")


# add colors to match bar chart #### - UPDATE: gray shades lighter for nonban states
# to avoid confusion with gray columns in combo chart. 
states$color[states$NAME == "Kentucky"] <- ky_c 
states$color[states$NAME == "Ohio"] <- oh_c
states$color[states$NAME == "Tennessee"] <- tn_c
states$color[states$NAME == "Illinois" ] <- "gray80"
states$color[states$NAME == "Michigan"]  <- "gray80"
states$color[states$NAME == "Other"]     <- "gray80"

# remake images with transparent background to preserve overlays when points
# overlap

for (i in 1:length(states$NAME[states$NAME != "Indiana"])) {
  
  # # set aspect ratio for image 
  asp_ratio <- 1.618
  # create list of states to iterate through
  state <- unique(states$NAME[states$NAME != "Indiana"])[i]
  
  ggplot()+
    geom_sf(data = states %>% filter(NAME == state),
            fill =  states$color[states$NAME == state])+
    theme_void()+
    t_theme()+
    theme(
      axis.text = element_blank(),
      plot.margin = margin(0, 0, 0,0, unit = "cm"),
      # in order to not obscure states on the timeline, change background of plot 
      # and panelto transparent
      plot.background = element_rect(fill   = "transparent",
                                     color  = "transparent"),
      panel.background = element_rect(fill  = "transparent",
                                      color = "transparent")
    )
  
  ggsave(plot = last_plot(), 
         filename = paste0("plots/png/",state,".png"),
         width  = 200*asp_ratio, 
         height = 200, units = "px")
}

# above files uploaded to github for access by ggtext in plot below

# base url to folder containing images
url_base <- "<img src='https://github.com/tedschurter/indiana_abortion/raw/main/"

# change url_base filename 'plot' to 'Plot'
dist_states <- dist_states %>% 
  mutate(image = str_replace(dist_states$image, "p", "P"))

dist_states <- dist_states %>% 
  mutate(url = paste0(url_base, dist_states$image,"' width ='25' />"))

c_fill <- c("Marion"      = "gray45",
            "Monroe"     = "gray65",
            "Lake"       = "gray65",
            "Tippecanoe" = "gray65",
            "St. Joseph" = "gray65")


ggplot()+
  
  # add icons of five states for nonresident abortions at distance calculated 
  # from center of each state to facility
  geom_richtext(data = dist_states, # %>% filter(state != "Indiana"),
                aes(x = facility, y = distance, label = url), 
                vjust = -.2, 
                size = 1,
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
  )+
  scale_fill_manual(values = c_fill)+

    # add horizontal lines to make it easier to track state icons to facilities
  geom_segment(aes(x=7, xend = 7, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=6, xend = 6, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=5, xend = 5, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=4, xend = 4, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=3, xend = 3, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=2, xend = 2, y = 0, yend = 600), color = "gray93")+
  geom_segment(aes(x=1, xend = 1, y = 0, yend = 600), color = "gray93")+
  
  # add columns for count of nonresident abortions by facility
  geom_col(data = ploc,
           aes(fct_rev(fct_reorder(facility, nonresident)), nonresident, fill =
                 county),
           width = .15, show.legend = F)+
 
  # flip axis
  coord_flip()+
  
  # set y axis breaks and labels
  scale_y_continuous(limits = c(-76, 630),
                     breaks = c(0, 100, 200, 300, 400, 500, 600),
                     labels = c("0", "100", "200", "300", "400", "500", "600"),
                     name = "Number of abortions and \nmiles to state center")+
  
  # labels for facilities
  geom_textbox(data = ploc,
               aes(x = facility, y  = 0),
               label = paste0(ploc$facility,": **",ploc$county," Co.**"), colour = panel_c, fill = panel_c,
               hjust = 1, text.color = "gray40", size = 2.4, width = unit(1.5, "inch"))+
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color  = "gray30", size = 8),
    axis.title.x = element_text(color = "gray30", size = 8, hjust = 0.02, vjust = 5)
  )+
  
  # add titles, captions
  labs(title = 
       paste0("The three abortion clinics in **Marion County** are a slightly closer
              drive on average to nearby states (measured from the center of each)
              than those in Lake, Monroe, St. Joseph and Tippecanoe counties. They accounted for ",
              formatC(sum(ploc$nonresident[ploc$county == "Marion"]),big.mark = ","),
              " nonresident abortions, ",
              round(100*(sum(ploc$nonresident[ploc$county == "Marion"])/
              sum(ploc$nonresident))),
              "% of all."),
       
       subtitle = 
         paste0(
        "Leading the nonresident abortion counts in 2022 were states that enacted 
        their own bans after the Dobbs decision, including <span style = 
        'color:#fc8d59;'>**Kentucky,**</span> (",
        nres$abortions[nres$year == 2022 & nres$state == "Kentucky"],
        ") <span style = 'color:#376bae;'>**Ohio**</span> (",
        nres$abortions[nres$year == 2022 & nres$state == "Ohio"],
        "), and <span style = 'color:#82b6d6;'>**Tennessee**</span> (",
        nres$abortions[nres$year == 2022 & nres$state == "Tennessee"],
        "). Illinois (",
        nres$abortions[nres$year == 2022 & nres$state == "Illinois"],
        ") and Michigan (",
        nres$abortions[nres$year == 2022 & nres$state == "Michigan"],
        ") retain abortion rights."),
      
       caption = 
         "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
    **Graphic:** Ted Schurter 2023")

  
# ggsave(plot = last_plot(),
#        filename = "plots/nr_state_fac.svg", width = 3200, height = 1600, units = "px")

