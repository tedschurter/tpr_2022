library(tidyverse)

# import data

ploc <- read_csv("exported_data/provider_residency.csv")



# add columns to show pct change from year to year and pct change of 2022 from
# average number of abortions by facility clinic for resident and nonresident 
# abortions 2018:2021

# calculate resident averages by facility 2018:2021

r_ag <- aggregate(ploc$resident[ploc$year != 2022], 
                  list(ploc$facility[ploc$year != 2022]), mean)

# rename columns
r_ag <- rename(r_ag, "r_avg" = "x", "facility" = "Group.1")

# join to ploc
ploc <- inner_join(ploc, r_ag, by = "facility") 


# calculate nonresident averages by facility 2018:2021

nr_ag <- aggregate(ploc$nonresident[ploc$year != 2022], 
                   list(ploc$facility[ploc$year != 2022]), mean)

# rename columns
nr_ag <- rename(nr_ag, "nr_avg" = "x", "facility" = "Group.1")

# join to ploc then round new columns
ploc <- inner_join(ploc, nr_ag, by = "facility") %>% 
  mutate(across(c(r_avg, nr_avg), round)) %>% 
  select(year, county, facility, resident, r_avg, nonresident,  nr_avg, total)

# add columns for difference from average, yoy difference for resident and nonresident
ploc_df <- 
  ploc %>% 
  group_by(facility) %>% 
  arrange(year) %>% 
  # add column for year to year % change for residents
  mutate(r_yoy_df_avg = round(100*((resident-lag(resident))/lag(resident)),1)) %>%
  # add column for year to year % change for nonresidents
  mutate(nr_yoy_df_avg = round(100*((nonresident-lag(nonresident))/lag(nonresident)),1)) %>% 
  # calculate what percent of change from previous year
  arrange(year) %>% 
  # calculate resident difference from resident average '18:21
  mutate(r_p_df_avg = round(100*((resident-r_avg)/r_avg)),
         # calculate resident difference from nonresident average '18:21
         nr_p_df_avg = round(100*((nonresident-nr_avg)/nr_avg))) %>% 
  # without modification, above nr_p_df_avg returns Inf for PP of Lafayette because
  # you can't calculate a percentage increase from 0. Solution is to replace Inf - the
  # value created by calculating ((new value - zero)/zero)*100 - with NA using na_if:
  mutate(across(c(nr_yoy_df_avg, nr_p_df_avg), ~ na_if(., Inf)))


# set theme ####
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



# resident percent change from average 
ggplot(ploc_df)+
  geom_col(aes(
           year, r_p_df_avg, fill = facility),
           position = "dodge", show.legend = F)+
  facet_wrap(~facility, ncol = 6)+
  theme_classic()

# nonresident percent change from average 
ggplot(ploc_df)+
  geom_col(aes(
    year, nr_p_df_avg, fill = facility),
    position = "dodge", show.legend = F)+
  facet_wrap(~facility, ncol = 6)+
  theme_classic()
     
# filter to 2022 and show both on either side of vertical axis

ploc_df %>% filter(year == 2022)

ggplot(ploc_df%>% filter(year == 2022))+
  geom_col(aes(
    year, nr_p_df_avg, fill = facility),
    position = "dodge", show.legend = F, width = .2)+
  geom_col(aes(
    year, r_p_df_avg, fill = facility, alpha = .3),
    position = "dodge", show.legend = F, color = "black")+
  coord_flip()+
  theme_classic()


# as tidydf

ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_p_df_avg, nr_p_df_avg) %>% 
  #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
  pivot_longer(cols = c(3:4), 
               names_to = "loc",
               values_to = "pct") %>% 
  ggplot()+
  geom_col(aes(
    fct_rev(fct_reorder(facility, pct)), pct, fill = facility),
    position = "dodge")+
  coord_flip()
  


ggplot()+
  geom_col(data = ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_p_df_avg, nr_p_df_avg) %>% 
             #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
             pivot_longer(cols = c(3:4), 
                          names_to = "loc",
                          values_to = "pct") %>% 
             filter(loc == "r_p_df_avg"),
           aes(
    fct_rev(fct_reorder(facility, pct)), pct, fill = facility, alpha  = .2),
    width = .2)+
  geom_col(data = ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_p_df_avg, nr_p_df_avg) %>% 
             #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
             pivot_longer(cols = c(3:4), 
                          names_to = "loc",
                          values_to = "pct") %>% 
             filter(loc == "nr_p_df_avg"),
           aes(
             fct_rev(fct_reorder(facility, pct)), pct, fill = facility),
           width = .2, position = "dodge")+
  coord_flip()



##
ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_p_df_avg, nr_p_df_avg) %>% 
  #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
  pivot_longer(cols = c(3:4), 
               names_to = "loc",
               values_to = "pct") %>% 
  ggplot()+
  geom_col(aes(facility, pct, fill = facility))+
  facet_grid(~loc)
#

# set alpha scale
alpha_loc <- c("r_p_df_avg" = 0.25,
              "nr_p_df_avg" = 1)

# set fill scale 
fac_fl <- c("Planned Parenthood of Merrillville" = "#7570b3",
            "Planned Parenthood of Bloomington"  = "#d95f02",
            "Planned Parenthood of Lafayette"    = "#1b9e77",
            "Planned Parenthood of Indianapolis" = "#c2e699",
            "Clinic for Women"                   = "#31a354",
            "The Women's Med Center of Indianapolis" = "#006837" 
              )

ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_p_df_avg, nr_p_df_avg) %>% 
  #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
  pivot_longer(cols = c(3:4), 
               names_to = "loc",
               values_to = "pct") %>% 
  ggplot()+
  geom_col(aes(
    fct_rev(fct_reorder(facility, pct)), pct, fill = facility, alpha = loc),
    position = position_dodge(width = .7), width = .65, color = "gray50", show.legend = F)+
  # geom_segment(aes(x = .5, xend = 6, y = 0, yend = 0),
  #              size = .2, color = "gray70")+
  t_theme()+
  scale_alpha_manual(values = alpha_loc)+
  scale_fill_manual(values  = fac_fl)+
  scale_y_continuous(limits = c(-300, 2000),
                     breaks = c(-250, -100, 0, 500, 1000, 1500, 2000),
                     labels = c("", "-100%", "0%", "500%", "1,000%", "1,500%", "2,000%"))+
  #coord_flip()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = rel(1.2))
  )+
  coord_flip()
# 

# yoy nres

ploc_df %>% filter(year == 2022) %>%  select(year, facility, r_yoy_df_avg, nr_yoy_df_avg) %>% 
  #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
  pivot_longer(cols = c(3:4), 
               names_to = "loc",
               values_to = "pct") %>% 
  ggplot()+
  geom_col(aes(
    fct_rev(fct_reorder(facility, pct)), pct, fill = facility, alpha = loc, 
     padding = 1),
    position = position_dodge(width = .7), width = .65, color = "gray80", show.legend = F)+
  # geom_segment(aes(x = .5, xend = 6, y = 0, yend = 0),
  #              size = .2, color = "gray70")+
  t_theme()+
  scale_alpha_manual(values = c("r_yoy_df_avg" = .3,
                                "nr_yoy_df_avg" = 1))+
  scale_fill_manual(values  = fac_fl)+
  scale_y_continuous(limits = c(-300, max(ploc_df$nr_yoy_df_avg, na.rm = T)),
                     breaks = c(-250, -100, 0, 500, 1000, 1500, 2000),
                     labels = c("", "-100%", "0%", "500%", "1,000%", "1,500%", "2,000%")
                     )+
  #coord_flip()+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = rel(1.2))
  )









# line chart by facility
ploc_pct <- ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) 


  ggplot()+
  geom_line(data = ploc_pct,
            aes(
    year, pct_nr, group = facility, color = facility
  ), show.legend = F)+
  scale_x_continuous(expand = expansion(mult = 0, add = c(.5, 1)), 
                     limits = c(2018, 2023),
                     breaks = c(2018, 2019, 2020, 2021, 2022),
                     labels = c("2018", "2019", "2020", "2021", "2022"))+
  geom_text_repel(data = ploc_pct  %>% filter(year == 2022),
             aes(x = 2022.1, 
                 y = pct_nr),
             label = ploc_pct$facility[ploc_pct$year == 2022],
             xlim = 2022.1,
             #point.padding = .5,
             #box.padding = .2,
             #nudge_y = -.2, 
             #nudge_x = .2, # .7
             force = 1,
             color = "gray50", 
             segment.color = "gray75",
             size = rel(2.5), 
             fontface = "plain", 
             direction = "both",
             segment.size = 0.1,
             min.segment.length = 1.15,
             hjust = 0, #vjust = .5,
             seed = 2
  )+
  t_theme()


#

#bar chart by facility

ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) %>% 
  ggplot()+
  geom_col(aes(
    year, pct_nr, group = facility, fill = facility
  ), show.legend = T)+
  theme_classic()
  

# bar chart dodge

ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) %>% 
  ggplot()+
  geom_col(
    aes(
        year, pct_nr, group = facility, fill = facility), 
    show.legend = T, position = "dodge")+
  theme_classic()+
  facet_wrap(~facility)
#

ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(resident),
         pct_r = round((resident/tot)*100,2)) %>% 
  ggplot()+
  geom_col(
    aes(
      year, pct_r, group = facility, fill = facility), 
    show.legend = T, position = "dodge")+
  theme_classic()+
  coord_flip()+
  facet_wrap(~facility)+
  theme(
    strip.background = element_blank()
  )

#
# combine the two?
ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(resident),
         pct_r = round((resident/tot)*100,2),
         tot_nr = sum(nonresident),
         pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
  ggplot()+
  geom_col(
    aes(
      year, -1*pct_r, group = facility, fill = facility), 
    show.legend = F, position = "dodge", alpha = .2)+
  # neg for nr
  geom_col(
    aes(
      year, pct_nr, group = facility, fill = facility), 
    show.legend = F, position = "dodge")+
  # geom_segment(aes(x=2017.5, xend = 2022.5,
  #                  y = 0, yend = 0), linewidth = .15, color = "gray30",
              # show.legend = F)+
  theme_classic()+
  coord_flip()+
  facet_wrap(~facility)+
  theme(
    strip.background = element_blank()
  )





# calculating percent change #### 


#



ggplot()+
  geom_col(
    aes(
      year, pct_df, group = facility, fill = facility), 
    show.legend = F, position = "dodge")+
  theme_classic()+
  facet_wrap(~facility, ncol = 6)+
  theme(
    strip.background = element_blank()
  )
# filter to just 2022

ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) %>% 
  ungroup() %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(
    nr2 = case_when(
      nonresident == 0 ~ NA, 
      .default = nonresident),
    pct_df = round(100*(nr2-lag(nr2))/lag(nr2))) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  geom_col(
    aes(
      facility, pct_df, fill = facility), 
    show.legend = F, position = "dodge")+
  theme_classic()
#
# same as above but for residents

ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(resident),
         pct_r = round((resident/tot)*100,2)) %>% 
  ungroup() %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(
    r2 = case_when(
      resident == 0 ~ NA, 
      .default = resident),
    pct_df = round(100*(r2-lag(r2))/lag(r2))) %>% 
  ggplot()+
  geom_col(
    aes(
      year, pct_r, group = facility, fill = facility), 
    show.legend = F, position = "dodge")+
  theme_classic()+
  facet_wrap(~facility, ncol = 6)+
  theme(
    strip.background = element_blank()
  )

# filter to just 2022
ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(resident),
         pct_r = round((resident/tot)*100,2)) %>% 
  ungroup() %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(
    r2 = case_when(
      resident == 0 ~ NA, 
      .default = resident),
    pct_df = round(100*(r2-lag(r2))/lag(r2))) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  geom_col(
    aes(
      facility, pct_r, fill = facility), 
    show.legend = F, position = "dodge")+
  theme_classic()

# 
# above but for nonresident
# filter to just 2022
ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) %>% 
  ungroup() %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(
    nr2 = case_when(
      nonresident == 0 ~ NA, 
      .default = nonresident),
    pct_df = round(100*(nr2-lag(nr2))/lag(nr2))) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  geom_col(
    aes(
      facility, pct_nr, fill = facility), 
    show.legend = F, position = "dodge")+
  theme_classic()


#


  ggplot()+
geom_col(
  aes(
    year, pct_df, group = facility, fill = facility), 
  show.legend = F, position = "dodge")+
  theme_classic()+
  facet_wrap(~facility, ncol = 6)+
  theme(
    strip.background = element_blank()
  )

#


ploc %>% 
  group_by(year) %>% 
  mutate(tot = sum(nonresident),
         pct_nr = round((nonresident/tot)*100,2)) %>% 
 ungroup() %>% 
   group_by(facility) %>% arrange(facility, year) %>% 
  mutate(
         pct_df = round(100*(nonresident-lag(nonresident))/lag(nonresident))) %>% 
  ggplot()+
  geom_jitter(aes(
    year, pct_df, group = facility, color = facility, size = pct_df
  ), show.legend = T)+
  theme_classic()






# add location data for facilities


ploc_df_ll <- 
  ploc_df %>% filter(year==2022) %>% 
  #mutate(r_p_df_avg = r_p_df_avg*-1) %>% 
  pivot_longer(cols = c(11:12), 
               names_to = "loc",
               values_to = "pct")%>% 
  
  mutate(location = str_c(facility, 
                          paste0(county, " County, IN"), 
                          sep=", ")) %>% 
  mutate_geocode(location)

fac_fl <- c("Planned Parenthood of Merrillville"     = "#7570b3",
            "Planned Parenthood of Lafayette"        = "#1b9e77",
            "Planned Parenthood of Bloomington"      =  "#d95f02",
            "Planned Parenthood of Indianapolis"     = "#c2e699",
            "Clinic for Women"                       = "#31a354",
            "The Women's Med Center of Indianapolis" = "#006837" 
)




ggplot()+
  geom_sf(data = states, fill = states$color
  )+
  #scale_fill_manual(values = s_cols)+
  geom_point(data=ploc_df_ll %>% 
               filter(loc == "nr_p_df_avg"),
             aes(x=lon, y=lat,
                 size = pct),
             color = "gray50",
             fill = fac_fl,
             shape = 21,
             alpha = .5,
             show.legend = F)+
  scale_fill_manual(values = fac_fl)+
  scale_size_area(max_size = 10)+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()
  )






ploc_11 <- ploc %>% 
  mutate(location = str_c(ploc$facility, 
                          paste0(ploc$county, " County, IN"), 
                          sep=", ")) %>% 
  mutate_geocode(location)


ploc %>% select(county, facility)



# bar plots 

ggplot()+
  geom_col(data = ploc %>% filter(year==2022) %>%
             summarise(r_tot = sum(resident),
                       nr_tot = sum(nonresident)) %>% 
             pivot_longer(cols = 1:2,
                          names_to = "loc",
                          values_to = "count") %>% 
             mutate(pct = 100*(count/sum(count)),
                    year = 2022),
           aes(x = factor(year), y=pct, fill = rev(loc)), position = position_stack(),
           width = .25)+
  geom_col(data = ploc %>% filter(year==2021) %>%
             summarise(r_tot = sum(resident),
                       nr_tot = sum(nonresident)) %>% 
             pivot_longer(cols = 1:2,
                          names_to = "loc",
                          values_to = "count") %>% 
             mutate(pct = 100*(count/sum(count)),
                    year = 2022),
           aes(x = factor(year), y=pct, fill = rev(loc)), position = position_stack(),
           width = .25)

# 
t <- ploc %>%  group_by(year) %>% 
  summarise(r_tot = sum(resident),.keep_all = T,
            nr_tot = sum(nonresident), .keep_all = T) %>% 
  select(year, r_tot, nr_tot) %>% 
  pivot_longer(cols = 2:3,
               names_to = "loc",
               values_to = "count") %>% 
  group_by(year) %>% 
  mutate(pct = 100*(count/sum(count))) 


t2 <- ploc %>% filter(year == 2022) %>% select(year, facility, nonresident) %>% 
  mutate(pct = round(100*nonresident/sum(nonresident),2))

t2$year <- "Facilities"

r_tot_c <- "#762a83"
nr_tot_c <-  "#1b7837"

a_cols = c("r_tot" = r_tot_c,
           "nr_tot" = nr_tot_c,
           "Planned Parenthood of Merrillville"     = "#c7e9c0",
           "Planned Parenthood of Lafayette"        = "#edf8e9",
           "Planned Parenthood of Bloomington"      = "#41ab5d",
           "Planned Parenthood of Indianapolis"     = "#74c476",
           "Clinic for Women"                       = "#a1d99b",
           "The Women's Med Center of Indianapolis" = "#238b45"
)

ggplot()+
  geom_col(data = t %>% filter(year %in% 2022:2021),
           aes(x = factor(year), y=pct, fill = loc), position = position_stack(),
           width = .5, show.legend = F)+
  theme_classic()+
  
  theme_classic()+
  geom_col(data = t2,
           aes(factor(year), pct, fill = fct_reorder(facility, pct)), 
           position = position_stack(),
           width = .25,
           show.legend = F)+
  scale_fill_manual(values = a_cols)+
  geom_col(data = t3,
           aes(factor(year), pct, fill = fct_reorder(facility, pct)), 
           position = position_stack(),
           width = .25, fill = panel_c,
           show.legend = F)+
  scale_fill_manual(values = a_cols)+
  t_theme()+
  # 2021 label
  geom_textbox(
    aes(x= 1, y = 105),
    label = paste0("2021"),
    label.colour = NA, fill = panel_c,
    size = rel(3), color = "gray30", 
    hjust = 0.3, box.color = NA,   width = unit(.5, "inch"))+
  theme(
    panel.grid.major.y = element_blank()
  )+
  # 2022 label
  geom_textbox(
    aes(x= 2, y = 105),
    label = paste0("2022"),
    label.colour = NA, fill = panel_c,
    size = rel(3), color = "gray30", 
    hjust = 0.3, box.color = NA,   width = unit(.5, "inch"))+
  # curve to facilities bar
  geom_curve(aes(
    x = 2.5, xend = 3, y = 80, yend =50), arrow = arrow(length = unit(0.03, "npc")),
    curvature = .4, angle = -110, color = nr_tot_c, linewidth = .85, ncp =15)+
  geom_curve(aes(
    x = 2.35, xend = 2.5, y = 95, yend =80),
    curvature = -.4, angle = -80, color = nr_tot_c, linewidth = .85)+
  # label for facilities
  geom_textbox(
    aes(x= 3.1, y = 45),
    label = paste0("Nonresident abortions were provided in six clinics in four counties."),
    label.colour = NA, fill = panel_c,
    size = rel(4), color = "gray30", 
    hjust = 0.3, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  # Women's Med Center label
  geom_textbox(
    aes(x= 3.65, y = 1),
    label = "Women's Med Center of Indianapolis",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 0, vjust = 0, halign = 1, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  # PP Bloomington label
  geom_textbox(
    aes(x= 3.65, y = 34),
    label = "Planned Parenthood of Bloomington",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 0, vjust = 0, halign = 1, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  # PP Bloomington label
  geom_textbox(
    aes(x= 3.65, y = 60),
    label = "Planned Parenthood of Indianapolis",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 0, vjust  = 0, halign = 1, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  # Clinic for Women
  geom_textbox(
    aes(x= 3.65, y = 88),
    label = "Clinic for Women",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 0, halign = 1, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  # PP Merrillville label
  geom_textbox(
    aes(x= 3.84, y = 96),
    label = "Planned Parenthood of Merrillville",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 1, halign = 1, box.color = NA,   width = unit(2.5, "inch"), lineheight = 1)+
  # PP Lafayette label
  geom_textbox(
    aes(x= 3.84, y = 100.6),
    label = "Planned Parenthood of Lafayette",
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 1, halign = 1, box.color = NA,   width = unit(2.5, "inch"), lineheight = 1)+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank()
  )+
  labs(
    title = paste0(
      "Though the majority of abortions in Indiana were to <span style= 
    'color:",r_tot_c,"'>residents, </span><span style= 
    'color:",nr_tot_c,"'>nonresident </span>abortions in 2022 increased to ",
      round(t$pct[t$year == 2022 & t$loc == "nr_tot"]),
      "% of all abortions, an increase of ",
      round(100*((sum(ploc$nonresident[ploc$year == 2022])-
                    sum(ploc$nonresident[ploc$year == 2021]))/
                   sum(ploc$nonresident[ploc$year == 2021]))),
      "% from 2021."),
    subtitle = 
      paste0(
        "Though located in the middle of the state, ",
        round(100*sum(ploc$nonresident[ploc$year == 2022 & 
                                         ploc$county == "Marion"])/sum(ploc$nonresident[ploc$year == 2022])),
        "% of nonresident abortions occur in Marion County at three clinics. The rest occur in 
      Monroe, Lake and Tippacanoe counties."
      ),
    caption = 
      "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
    **Graphic:** Ted Schurter 2023")

# 

sp <-   
  ggplot()+
  geom_col(data = nres %>% filter(year == 2022) %>% 
             mutate(
               pct = 100*(abortions/sum(abortions)),
               year = 2023),
           aes(fct_rev(fct_reorder(state, pct)), fill = state, pct),
           show.legend = F)+
  scale_fill_manual(values = c("Kentucky" = nr_tot_c,
                               "Illinois" = "gray",
                               "Michigan" = "gray",
                               "Ohio"     = nr_tot_c,
                               "Tennessee" = nr_tot_c,
                               "Other"    = "gray"))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank())


p + 
  inset_element(sp, 
                left = 0.01, 
                bottom = -.07, 
                right = .45, 
                top = .56,
                align_to = "panel")+
  plot_annotation(
    theme = t_theme(),
    title = paste0(
      "Though the majority of abortions in Indiana were to <span style=
      'color:",r_tot_c,"'>residents, </span><span style=
      'color:",nr_tot_c,"'>nonresident </span>abortions in 2022 increased to ",
      round(t$pct[t$year == 2022 & t$loc == "nr_tot"]),
      "% of all abortions, an increase of ",
      round(100*((sum(ploc$nonresident[ploc$year == 2022])-
                    sum(ploc$nonresident[ploc$year == 2021]))/
                   sum(ploc$nonresident[ploc$year == 2021]))),
      "% from 2021."),
    subtitle =
      paste0(
        "Though located in the middle of the state, ",
        round(100*sum(ploc$nonresident[ploc$year == 2022 &
                                         ploc$county == "Marion"])/sum(ploc$nonresident[ploc$year == 2022])),
        "% of nonresident abortions occur in Marion County at three clinics. The rest occur in
        Monroe, Lake and Tippacanoe counties."
      ),
    caption =
      "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")
#

# working version 20230707 3:49 pm

nrp <- nres %>% filter(year == 2022) %>% 
  mutate(
    pct = 100*(abortions/sum(abortions)),
    year = 2023)
sp <-   
  ggplot()+
  geom_col(data = nrp,
           aes(fct_rev(fct_reorder(state, pct)), pct),
           show.legend = F, fill = nr_tot_c, )+ 
  
  geom_textbox(
             aes(x= 5.75, y = 32),
             label = paste0("<span style='color:",r_tot_c,"'>Resident</span> abortions 
             in Indiana declined ",
              round(abs(100*((sum(ploc$resident[ploc$year == 2022])-
              sum(ploc$resident[ploc$year == 2021]))/
              sum(ploc$resident[ploc$year == 2021])))),
             "% from 2021."),
             label.colour = NA, fill = panel_c,
             size = rel(3.5), color = "gray30", 
             hjust = 0.3, box.color = NA,   width = unit(1, "inch"), lineheight = 1)+ 
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank())


p <- 
  ggplot()+
  geom_col(data = t %>% filter(year %in% 2022),
           aes(x = factor(year), y=pct, fill = loc), position = position_stack(),
           width = .4, show.legend = F)+
  geom_col(data = t %>% filter(year %in% 2021),
           aes(x = factor(year), y=pct), fill = panel_c, position = position_stack(),
           width = .5, show.legend = F)+
  geom_col(data = t %>% filter(year %in% 2020),
           aes(x = factor(year), y=pct), fill = panel_c, position = position_stack(),
           width = .5, show.legend = F)+
  geom_col(data = t2,
           aes(factor(year), pct, fill = fct_reorder(facility, pct)), 
           position = position_stack(),
           width = .30,
           show.legend = F)+
  scale_fill_manual(values = a_cols)+
  scale_y_continuous(limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100, 110),
                     labels = c("0", "25%", "50%", "75%", "100%", "Percent of\nabortions"))+
  geom_col(data = t3,
           aes(factor(year), pct, fill = fct_reorder(facility, pct)), 
           position = position_stack(),
           width = .25, fill = panel_c,
           show.legend = F)+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  # 2022 label
  geom_textbox(
    aes(x= 3, y = 107),
    label = paste0("**2022 abortions**"),
    label.colour = NA, fill = panel_c,
    size = rel(3), color = "gray30", 
    hjust = .7, box.color = NA,   width = unit(.5, "inch"))+
  # curve to facilities bar
  geom_curve(aes(
    x = 3.5, xend = 4.2, y = 80, yend =50), arrow = arrow(length = unit(0.03, "npc")),
    curvature = .4, angle = -110, color = nr_tot_c, linewidth = .85, ncp =15)+
  geom_curve(aes(
    x = 3.35, xend = 3.5, y = 95, yend =80),
    curvature = -.4, angle = -80, color = nr_tot_c, linewidth = .85) +
  # label for facilities
  geom_textbox(
    aes(x= 3.65, y = 29),
    label = paste0("<span style='color:",nr_tot_c,"'>Nonresident</span> abortions 
    occured in six clinics in four Indiana counties."),
    label.colour = NA, fill = panel_c,
    size = rel(4), color = "gray30", 
    hjust = 0.3, box.color = NA,   width = unit(1, "inch"), lineheight = 1)+  
  # curve to states bars
  geom_curve(aes(
    x = 1.65, xend = 2.6, y = 55, yend =90),
    curvature = -.2, angle = 80, color = nr_tot_c, linewidth = .85,
    arrow = arrow(length = unit(0.03, "npc")))+
  
  # label for state bars
  geom_textbox(
    aes(x= 1.8, y = 86),
    label = paste0("The three states that tallied the most <span style='color:",
    nr_tot_c,"'>nonresident</span> abortions in Indiana - **Kentucky**, **Ohio** and **Tennessee** -
    all enacted abortion bans in 2022."),
    label.colour = NA, fill = panel_c,
    size = rel(4.15), color = "gray30", 
    hjust = 1, box.color = NA,   width = unit(2.35, "inch"), lineheight = 1)+ 
  
  
  # Women's Med Center label
  geom_textbox(
    aes(x= 4.82, y = .9),
    label = paste0("**Marion County:**<br>Women's Med Center of Indianapolis:<br>
                   **", t3$pct[t3$facility == "The Women's Med Center of Indianapolis"],
                   "%**"),
    label.colour = NA, fill = panel_c, size = rel(2.5), color = "gray30", hjust = 1, 
    vjust = 0, halign = 1, box.color = NA, width = unit(1.2, "inch"), lineheight = 1.1)+
  # PP Bloomington label
  geom_textbox(
    aes(x= 4.82, y = 34),
    label = paste0("**Monroe County:**<br>Planned Parenthood of Bloomington:<br>
                   **", t3$pct[t3$facility == "Planned Parenthood of Bloomington"],
                   "%**"),
    label.colour = NA, fill = panel_c, size = rel(2.5), color = "gray30", 
    hjust = 1, vjust = 0, halign = 1, box.color = NA,   width = unit(1.2, "inch"), 
    lineheight = 1.2)+
  # PP Indy label
  geom_textbox(
    aes(x= 4.82, y = 58),
    label = paste0("**Marion County:**<br>Planned Parenthood of Indianapolis:<br>
                   **", t3$pct[t3$facility == "Planned Parenthood of Indianapolis"],
                   "%**"),
    label.colour = NA, fill = panel_c, size = rel(2.5), color = "gray30", hjust = 1,
    vjust  = 0, halign = 1, box.color = NA,   width = unit(1.2, "inch"), lineheight = 1.1)+
  # Clinic for Women
  geom_textbox(
    aes(x= 4.82, y = 86),
    label = paste0("**Marion County:** Clinic for Women: **", 
                   t3$pct[t3$facility == "Planned Parenthood of Indianapolis"],
                   "%**"),
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 1, halign = 1, box.color = NA,   width = unit(2.5, "inch"), lineheight = 1)+
  # PP Merrillville label
  geom_textbox(
    aes(x= 4.82, y = 96.5),
    label = paste0("**Lake County:** Planned Parenthood of Merrillville: **", 
                   t3$pct[t3$facility == "Planned Parenthood of Merrillville"],
                   "%**"),
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 1, halign = 1, box.color = NA,   width = unit(2.5, "inch"), lineheight = 1)+
  # PP Lafayette label
  geom_textbox(
    aes(x= 4.82, y = 103.5),
    label = paste0("**Tippecanoe County:** Planned Parenthood of Lafayette: **", 
                   t3$pct[t3$facility == "Planned Parenthood of Lafayette"],
                   "%**"),
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray30", 
    hjust = 1, halign = 1, box.color = NA,   width = unit(3, "inch"), lineheight = 1)+
  # label for y axis
  # geom_textbox(
  #   aes(x= .7, y = 100),
  #   label = "% of all abortions",
  #   label.colour = NA, fill = panel_c,
  #   size = rel(.6), color = "gray30", 
  #   hjust = 1, halign = 1, box.color = NA,   width = unit(.5, "inch"), lineheight = 1)+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    )

##
p + 
  inset_element(sp, 
                left = 0.01, 
                bottom = -.07, 
                right = .45, 
                top = .56,
                align_to = "panel")+
  plot_annotation(
    theme = t_theme(),
    title = paste0(
      
      
      "Post Dobbs abortion bans in neighboring states fueled a **",
      round(100*((sum(ploc$nonresident[ploc$year == 2022])-
                    sum(ploc$nonresident[ploc$year == 2021]))/
                   sum(ploc$nonresident[ploc$year == 2021]))),
      "%** increase in <span style=
      'color:",nr_tot_c,"'>nonresident</span> abortions in Indiana in 2022.
      One in five abortions went to someone from out of state. <span style=
      'color:",r_tot_c,"'>Resident</span> abortions declined ",
      round(abs(100*((sum(ploc$resident[ploc$year == 2022])-
                        sum(ploc$resident[ploc$year == 2021]))/
                       sum(ploc$resident[ploc$year == 2021])))),
      "%."),
    subtitle =
      paste0(
        "Three Marion County clinics performed ",
        round(100*sum(ploc$nonresident[ploc$year == 2022 &
                                         ploc$county == "Marion"])/sum(ploc$nonresident[ploc$year == 2022])),
        "% of <span style=
      'color:",nr_tot_c,"'>nonresident</span> abortions. The rest occurred in Monroe, Lake and Tippacanoe counties."
      ),
    caption =
      "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")

ggsave("plots/nres_combo.svg", width = 3400, height = 1800, units = "px")



ploc_nr_labs <- 
  ploc %>% filter(year %in% 2021:2022) %>% 
  select(year, county, facility, nonresident, resident) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_nr = round(100*((nonresident-lag(nonresident))/lag(nonresident))),
         pct_r  = round(100*((resident-lag(resident))/lag(resident))))

ploc_r_labs <- 
ploc %>% filter(year %in% 2021:2022) %>% 
  select(year, county, facility, resident) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_r  = round(100*((resident-lag(resident))/lag(resident)))) %>% 
  filter(year == 2022) %>% 
   mutate(year = 2021)


ploc %>% filter(year %in% 2021:2022) %>% select(-nonresident) %>% 
  group_by(year) %>% mutate(pct_all_r = 100*(resident/sum(resident))) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_cng_r  = round(100*((pct_all_r-lag(pct_all_r))/lag(pct_all_r))))
  
ploc %>% filter(year %in% 2021:2022) %>% select(-resident) %>% 
  group_by(year) %>% mutate(pct_all_nr = 100*(nonresident/sum(nonresident))) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_cng_nr  = round(100*((pct_all_nr-lag(pct_all_nr))/lag(pct_all_nr))))


ploc %>% filter(year %in% 2021:2022) %>% select(-nonresident) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(r_pct_cng = 100*((resident-lag(resident))/lag(resident)))




ploc %>% filter(year %in% 2021:2022) %>% select(-resident) %>% 
  group_by(year) %>% mutate(pct_all_nr = 100*(nonresident/sum(nonresident))) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_cng_nr  = round(100*((pct_all_nr-lag(pct_all_nr))/lag(pct_all_nr)))) %>%  
  ggplot()+
  # geom_point(
  #            aes(
  #              year, pct_cng_nr, color = facility), show.legend = F)+
  # geom_point(
  #   aes(
  #     year, pct_all_nr, color = facility), show.legend = F)+
  geom_segment(
            aes(
              x = 2021, xend = 2022, 
              y = pct_all_nr, yend = pct_cng_nr,
              color = facility), show.legend = F)
#





ggplot()+
  geom_point(data = ploc_nr_labs,
             aes(
               year, nonresident, color = facility), show.legend = F)+
  geom_line(data = ploc_nr_labs,
            aes(
              year, nonresident, group = facility, color = facility),
            show.legend = F)+
  # geom_richtext(data = ploc_nr_labs %>% filter(year == 2022),
  #           aes(x = 2022.1, y = nonresident,
  #               label = paste0("**",county," County:** ", facility,": ", formatC(pct, big.mark = ","),"%")),
  #           hjust = 0, fill = panel_c, color = panel_c, text.color = "gray50",
  #           size = 3.5)+
  geom_text_repel(data= ploc_nr_labs %>% filter(year == 2022),
                  aes(2022.5, nonresident,
                      label = paste0(county," County: ", facility,": ", formatC(pct, big.mark = ","),"%")),
                  xlim = c(2022.05),
                  #point.padding = .025,
                  #box.padding = .2,
                  #nudge_y = -.2,
                  #nudge_x = 1.2, # .7
                  #force = 1,
                  color = "gray40",
                  segment.color = "gray45",
                  size = rel(3),
                  fontface = "plain",
                  direction = "y",
                  segment.size = 0.1,
                  min.segment.length = 3.15,
                  hjust = 0, vjust = .5,
                  seed = 2
  )+
  scale_x_continuous(limits = c(2021, 2023))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank()
    )+
  labs(
    title = "working title goes here.",
    subtitle = "Working subtitle goes here.",
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")
    
    
# experimenting with lines and points and residency by facility

nr <- ploc %>% filter(year %in% 2021:2022) %>% select(-resident) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_cng = round(100*((nonresident-lag(nonresident))/lag(nonresident)),2),
         pct_cng = replace_na(pct_cng, 0))

r <- ploc %>% filter(year %in% 2021:2022) %>% select(-nonresident) %>% 
  group_by(facility) %>% arrange(facility, year) %>% 
  mutate(pct_cng = round(100*((resident-lag(resident))/lag(resident)),2),
         pct_cng = replace_na(pct_cng, 0))

r$year[r$year==2022] <- 2020

ggplot()+
  geom_point(data = nr %>% filter(year == 2021),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_point(data = nr %>% filter(year == 2022),
             aes(
               year, pct_cng, color = facility),  show.legend = F)+
  geom_line(data = nr,
            aes(x = year, y = pct_cng, color = facility, group = facility), 
            show.legend = F)+
  
  
  geom_point(data = r %>% filter(year == 2021),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_point(data = r %>% filter(year == 2020),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_line(data = r,
            aes(x = year, y = pct_cng, color = facility, group = facility), 
            show.legend = F)+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )+
  geom_segment(aes(x = 2021, xend = 2021,
                   y = -400, yend = 2000),
               color = "gray80", linewidth = .15)+
  scale_x_continuous(limits = c(2019.5, 2022.5))+
  scale_size_area(max_size = 15)+
  coord_flip()+
  
  labs(
    title = "working title goes here.",
    subtitle = "Working subtitle goes here.",
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")
)
#
ggplot()+
  geom_point(data = nr %>% filter(year == 2021),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_point(data = nr %>% filter(year == 2022),
             aes(
               year, pct_cng, color = facility, size = nonresident), shape = 18, show.legend = F)+
  geom_line(data = nr,
            aes(x = year, y = pct_cng, color = facility, group = facility), 
            show.legend = F)+
  
  
  geom_point(data = r %>% filter(year == 2021),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_point(data = r %>% filter(year == 2020),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_line(data = r,
            aes(x = year, y = pct_cng, color = facility, group = facility), 
            show.legend = F)+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )+
  geom_segment(aes(x = 2021, xend = 2021,
                   y = -400, yend = 2000),
               color = "gray80", linewidth = .15)+
  scale_x_continuous(limits = c(2019.5, 2022.5))+
  scale_size_area(max_size = 15)+
  coord_flip()+
  
  labs(
    title = "working title goes here.",
    subtitle = "Working subtitle goes here.",
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")

#
ggplot()+
  geom_point(data = nr %>% filter(year == 2021),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_point(data = nr %>% filter(year == 2022),
             aes(
               year, pct_cng, color = facility), show.legend = F)+
  geom_line(data = nr,
            aes(x = year, y = pct_cng, color = facility, group = facility), 
            show.legend = F)+
  
  
  # geom_point(data = r %>% filter(year == 2021),
  #            aes(
  #              year, pct_cng, color = facility), show.legend = F)+
  # geom_point(data = r %>% filter(year == 2020),
  #            aes(
  #              year, pct_cng, color = facility), show.legend = F)+
  # geom_line(data = r,
  #           aes(x = year, y = pct_cng, color = facility, group = facility), 
  #           show.legend = F)+
t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )+
  geom_segment(aes(x = 2021, xend = 2021,
                   y = -400, yend = 2000),
               color = "gray60", linewidth = .15)+
  coord_polar(theta= "y")+
  
  labs(
    title = "working title goes here.",
    subtitle = "Working subtitle goes here.",
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")



#


fac_fl <- c("Planned Parenthood of Merrillville" = "#7570b3",
            "Planned Parenthood of Bloomington"  = "#d95f02",
            "Planned Parenthood of Lafayette"    = "#1b9e77",
            "Planned Parenthood of Indianapolis" = "#31a354",
            "Clinic for Women"                   = "#c2e699",
            "The Women's Med Center of Indianapolis" = "#006837")


ploc %>% 
  group_by(year) %>% 
  mutate(tot_r = sum(resident),
         pct_r = -1*round((resident/tot_r)*100,2),
         tot_nr = sum(nonresident),
         pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  geom_col(
    aes(fct_reorder(facility, total), pct_nr, fill = facility),
    width = .5, show.legend = F)+
  geom_col(
    aes(facility, pct_r, fill = facility),
    alpha = .4, width = .5, show.legend = F
  )+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()
  )+
  geom_segment(aes(
    x = 0, xend = 6.5, y = 0, yend = 0),
    color = panel_c, linewidth = .15)+
  coord_flip()+
  scale_fill_manual(values  = fac_fl)+
  scale_y_continuous(limits = c(-35,35))+
  scale_x_discrete(expand = expansion(add = 1))+
  geom_text(aes(facility, pct_nr, label = paste0(pct_nr,"%")),
            size = rel(2.75), color = "gray20", hjust = -.35)+
  geom_text(aes(facility, pct_r, label = paste0(-1*pct_r,"%")),
            size = rel(2.75), color = "gray20", hjust = 1.3)+
  geom_textbox(
    aes(x= 1.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][2]," County: ",ploc$facility[ploc$year == 2022][2]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 2.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][3]," County: ",ploc$facility[ploc$year == 2022][3]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 3.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][1]," County: ",ploc$facility[ploc$year == 2022][1]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 4.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][5]," County: ",ploc$facility[ploc$year == 2022][5]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 5.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][4]," County: ",ploc$facility[ploc$year == 2022][4]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 6.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][6]," County: ",ploc$facility[ploc$year == 2022][6]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  labs(
    title = paste0("Planned Parenthood in Bloomington, IN had the second-highest
    total of nonresident abortions and ",
    round(100*((ploc$nonresident[ploc$year == 2022 & ploc$county == "Monroe"]-
    ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])/
    ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])),
    "% more than it did in 2021."),
    subtitle = paste0(stringr::str_to_sentence(english::as.english(round(100*(sum(ploc$total[ploc$county=="Marion"])/sum(ploc$total))))),
               " percent of all abortions occured in Marion County."),
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")

ploc$facility[ploc$year == 2022][2]
ploc$county[ploc$year == 2022][2]

round(100*((ploc$nonresident[ploc$year == 2022 & ploc$county == "Monroe"]-
  ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])/
  ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"]))

round(100*((ploc$total[ploc$year == 2022 & ploc$county == "Monroe"]-
              ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])/
             ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"]))


round(100*(sum(ploc$total[ploc$county=="Marion"])/sum(ploc$total)))

stringr::str_to_sentence(english::as.english(round(100*(sum(ploc$total[ploc$county=="Marion"])/sum(ploc$total))))) 


#

ploc %>% 
  group_by(year) %>% 
  mutate(tot_r = sum(resident),
         pct_r = -1*round((resident/tot_r)*100,2),
         tot_nr = sum(nonresident),
         pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  
  
  
  geom_col(
    aes(fct_reorder(facility, total), pct_nr), fill = "gray",
    width = .5, alpha = .5, color = "gray80", show.legend = F)+
  geom_col(
    aes(facility, pct_r), fill = "gray",
    alpha = .08, width = .5, color = "gray80", show.legend = F
  )+
  geom_col(data = ploc %>% 
             group_by(year) %>% 
             mutate(tot_r = sum(resident),
                    pct_r = -1*round((resident/tot_r)*100,2),
                    tot_nr = sum(nonresident),
                    pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
             filter(year == 2022 & county == "Monroe"),
           aes(fct_reorder(facility, total), pct_nr, fill = facility),
           width = .5, alpha = 1, color = "black", show.legend = F)+
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()
  )+
  geom_segment(aes(
    x = 0, xend = 6.5, y = 0, yend = 0),
    color = panel_c, linewidth = .15)+
  coord_flip()+
  scale_fill_manual(values  = fac_fl)+
  scale_y_continuous(limits = c(-35,35))+
  scale_x_discrete(expand = expansion(add = 1))+
  geom_text(aes(facility, pct_nr, label = paste0(pct_nr,"%")),
            size = rel(2.75), color = "gray20", hjust = -.35)+
  geom_text(aes(facility, pct_r, label = paste0(-1*pct_r,"%")),
            size = rel(2.75), color = "gray20", hjust = 1.3)+
  geom_textbox(
    aes(x= 1.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][2]," County: ",ploc$facility[ploc$year == 2022][2]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 2.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][3]," County: ",ploc$facility[ploc$year == 2022][3]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 3.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][1]," County: ",ploc$facility[ploc$year == 2022][1]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 4.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][5]," County: ",ploc$facility[ploc$year == 2022][5]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 5.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][4]," County: ",ploc$facility[ploc$year == 2022][4]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 6.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][6]," County: ",ploc$facility[ploc$year == 2022][6]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  labs(
    title = paste0("Planned Parenthood in Bloomington, IN had the second-highest
    total of nonresident abortions and ",
                   round(100*((ploc$nonresident[ploc$year == 2022 & ploc$county == "Monroe"]-
                                 ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])/
                                ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])),
                   "% more than it did in 2021."),
    subtitle = paste0(stringr::str_to_sentence(english::as.english(round(100*(sum(ploc$total[ploc$county=="Marion"])/sum(ploc$total))))),
                      " percent of all abortions occured in Marion County."),
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")


###



ploc %>% 
  group_by(year) %>% 
  mutate(tot_r = sum(resident),
         pct_r = -1*round((resident/tot_r)*100,2),
         tot_nr = sum(nonresident),
         pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
  filter(year == 2022) %>% 
  ggplot()+
  
  
  
  geom_col(
    aes(fct_reorder(facility, total), pct_nr, fill = facility),
    width = .5, alpha = .5, color = "gray80", show.legend = F)+
  geom_col(
    aes(facility, pct_r, fill = facility),
    alpha = .08, width = .5, color = "gray80", show.legend = F
  )+
  geom_col(data = ploc %>% 
             group_by(year) %>% 
             mutate(tot_r = sum(resident),
                    pct_r = -1*round((resident/tot_r)*100,2),
                    tot_nr = sum(nonresident),
                    pct_nr = round((nonresident/tot_nr)*100,2)) %>% 
             filter(year == 2022 & county == "Monroe"),
           aes(fct_reorder(facility, total), pct_nr, fill = facility),
           width = .5, alpha = 1, color = "black", show.legend = F)+
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()
  )+
  geom_segment(aes(
    x = 0, xend = 6.5, y = 0, yend = 0),
    color = panel_c, linewidth = .15)+
  coord_flip()+
  scale_fill_manual(values  = fac_fl)+
  scale_y_continuous(limits = c(-35,35))+
  scale_x_discrete(expand = expansion(add = 1))+
  geom_text(aes(facility, pct_nr, label = paste0(pct_nr,"%")),
            size = rel(2.75), color = "gray20", hjust = -.35)+
  geom_text(aes(facility, pct_r, label = paste0(-1*pct_r,"%")),
            size = rel(2.75), color = "gray20", hjust = 1.3)+
  geom_textbox(
    aes(x= 1.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][2]," County: ",ploc$facility[ploc$year == 2022][2]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 2.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][3]," County: ",ploc$facility[ploc$year == 2022][3]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 3.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][1]," County: ",ploc$facility[ploc$year == 2022][1]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 4.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][5]," County: ",ploc$facility[ploc$year == 2022][5]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 5.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][4]," County: ",ploc$facility[ploc$year == 2022][4]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  geom_textbox(
    aes(x= 6.45, y = 0),
    label = paste0(ploc$county[ploc$year == 2022][6]," County: ",ploc$facility[ploc$year == 2022][6]),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(4, "inch"))+
  labs(
    title = paste0("Planned Parenthood in Bloomington, IN had the second-highest
    total of nonresident abortions and ",
                   round(100*((ploc$nonresident[ploc$year == 2022 & ploc$county == "Monroe"]-
                                 ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])/
                                ploc$nonresident[ploc$year == 2021 & ploc$county == "Monroe"])),
                   "% more than it did in 2021."),
    subtitle = paste0(stringr::str_to_sentence(english::as.english(round(100*(sum(ploc$total[ploc$county=="Marion"])/sum(ploc$total))))),
                      " percent of all abortions occured in Marion County."),
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
      **Graphic:** Ted Schurter 2023")
