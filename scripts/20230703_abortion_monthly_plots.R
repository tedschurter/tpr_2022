library(tidyverse)
library(ggplot2)
library(tigris)
library(ggtext)
library(ggrepel)
library(patchwork)


# Script creates plots showing 2022 monthly abortion data for residents and nonnresidents
# in context of the same data from the past nine years to better understand the impact 
# of the overturning of Roe v Wade had on all abortions in Indiana and also the 
# state's short-lived abortion ban.

# import data

mnth <- read_csv("exported_data/monthly_abortions_14_22.csv")


# order months chronologically

mnth$month <- factor(mnth$month, levels = c(
  "January", "February", "March", "April", "May", "June", "July","August",
  "September", "October", "November", "December"))


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


# set colors  ####

# panel color
panel_c <- "#fdfdf2"

# 2021 color 
c_2021 <- "#1b7837"

# 2022 color 
c_2022 <- "#762a83"

# assign colors for labeling
c_mnth <- mnth %>% filter(month == "December") %>% mutate(
  color = 
    case_when(year == 2022 ~ c_2022,
              year == 2021 ~ c_2021, 
              .default = "black")
)

# create plot ####

# timeline plot with total abortions
ggplot()+ 
  # lines for all months
  geom_line(data = mnth %>% group_by(year) %>% arrange(desc(tot)),
            aes(month, tot, group = year), color = "gray85", size = .25)+
  
  # Dobbs leak text and marker 
  # vertical line
  geom_segment(aes(x=5+(2/31), xend = 5+(2/31),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 5, y = 240,
        label = "**May 2, 2022:**<br><br>Draft of US Supreme Court Dobbs v. Jackson 
        decision is leaked, raising speculation about overturn of Roe v. Wade."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0, box.color = NA, width = unit(.9, "inch"))+
  
  # Dobbs decision text and marker line
  # vertical line
  geom_segment(aes(x=6+(24/30), xend = 6+(24/30),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label 
  geom_textbox(
    aes(x= 6+(24/30)-.01, y = 240,
        label = "**June 24, 2022:**<br><br>Dobbs decision overturning 
        Roe v. Wade and removing constitutional right to abortion is released."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0, box.color = NA, width = unit(1.15, "inch"))+
  
  # Aug. 5 text and marker line
  # vertical line
  geom_segment(aes(x=8+(5/31), xend = 8+(5/31),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 8+(5/31)-.01, y = 240,
        label = "**Aug. 5, 2022:**<br><br>Indiana passes legislation prohibiting 
    abortion from conception with few exceptions."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0,  box.color = NA, width = unit(.9, "inch"))+
  
  # text and line for Sept. 15 law going into effect
  # vertical line
  geom_segment(aes(x=9+(15/30), xend = 9+(15/30),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 9+(15/30)-.01, y = 240,
        label = "**Sept. 15, 2022:**<br><br>Indiana law goes into effect."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0,  box.color = NA, width = unit(.9, "inch"))+
  
  # text and line for Sept. 22 law blocked
  # vertical line
  geom_segment(aes(x=9+(22/30), xend = 9+(22/30),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 9+(22/30)+.01, y = 1110,
        label = "**Sept. 22, 2022:**<br><br>Indiana Circuit Court Judge Kelsey 
        Blake Hanlon blocks new law because it restricts bodily autonomy."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 0, vjust = 1,  box.color = NA, width = unit(1.08, "inch"))+
  
  # Marion judge again blocks law
  # vertical line
  geom_segment(aes(x=12+(2/31), xend = 12+(2/31),
                   y = 240, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 12+(2/31)+.01, y = 1110,
        label = "**Dec. 2, 2022:**<br><br>Marion County Judge Heather Welch 
        blocks new law because it likely violates Indiana’s Religious Freedom 
        Restoration Act."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray45", 
    hjust = 0, vjust = 1,  box.color = NA, width = unit(1.08, "inch"))+
  
  # monthly average indicator line
  geom_segment(aes(x=1, xend = 12,
                   y = mean(mnth$tot[mnth$year !=2022]), 
                   yend = mean(mnth$tot[mnth$year!=2022])),
               color = "gray45", linewidth = .2) +
  
  # monthly average label
  geom_textbox(
    aes(x= 0.5, y = mean(mnth$tot[mnth$year !=2022])),
    label = "Overall monthly average 2014-2021", 
    label.colour = NA, fill = panel_c,
    size = rel(2.1), color = "gray45", 
    hjust = .5, box.color = NA,   width = unit(.4, "inch"))+
  
  # monthly average label line
  geom_segment(aes(x= .75, xend = .95, 
                   y = mean(mnth$tot[mnth$year !=2022]), 
                   yend = mean(mnth$tot[mnth$year!=2022])),
               color = "gray45", linewidth = .1)+
  
  # line for 2021 totals
  geom_line(data = mnth %>% filter(year == 2021),
            aes(month, tot, group = year), color = c_2021, size = .65)+
  # line for 2022 totals
  geom_line(data = mnth %>% filter(year == 2022),
            aes(month, tot, group = year), color = c_2022, size = .65)+
  # labels for years 
  geom_text_repel(data= mnth %>% 
                    group_by(year) %>% mutate(a_tot = sum(res)+sum(nonres)) %>% 
                    filter(month == "December"),
                  aes(12.25, tot, label = paste0(year, ":  ", prettyNum(a_tot, 
                                                                        big.mark = ",", scientific = FALSE))),
                  #xlim = c(2021.4),
                  point.padding = .025,
                  box.padding = .2,
                  #nudge_y = -.2, 
                  nudge_x = 1.2, # .7
                  force = 1,
                  color = c_mnth$color, 
                  segment.color = "gray75",
                  size = rel(2.35), 
                  fontface = "plain", 
                  direction = "y",
                  segment.size = 0.1,
                  min.segment.length = .15,
                  hjust = .4,
                  seed = 2
  )+
  
  # label for July peak
  geom_textbox(
    aes(x= 5.17, y = 1135),
    label = paste0("Monthly abortions peaked in July <span style= 
    'color:",c_2022,"'>2022 </span> at 1,182."),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(1.1, "inch"))+
  
  # set y scale
  scale_y_continuous(limits = c(240, 1200),
                     breaks = c( 400, 600, 800, 1000, 1200),
                     labels = c("400", "600", "800", "1,000", "1,200"),
                     name = "Abortions")+
  
  # set x scale
  scale_x_discrete(
    breaks = c("January", "February", "March", "April","May", "June",
               "July", "August", "September", "October", "November","December"),
    labels = c("January", "February", "March", "April","May", "June",
               "July", "August", "September", "October", "November","December"),
    expand = expansion(add = 1.25))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
  )+
  
  # titles, caption
  labs(title = 
         paste0("Though the draft of the Dobbs decision to end federal abortion 
    protection began circulating in May 2022, the biggest impact on Indiana's <span
    style= 'color:",c_2022,"'>2022 </span>monthly abortion totals occurred in July 
    when they jumped **66%** from the same month in <span style= 'color:",c_2021,"'>2021 
    </span>to a nine-year high of **1,182**. They fell to 447 by October, their 
    lowest total since 2014. "),
       
       caption = 
         "<br>**Data:**'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
    **Graphic:** Ted Schurter 2023")

# ggsave("plots/month_yr_ab_2022.svg", width = 3200, height = 1725, unit = "px",
#        plot = last_plot())

####

# line plot with totals separated by residents and nonresidents  ####
  
# calculate monthly means  ####
mnth_2 <- mnth %>% group_by(month) %>% arrange(year, month) %>%
  mutate(
    res_yoy = round(((res-lag(res))/lag(res))*100,2),
    n_res_yoy = round(((nonres-lag(nonres))/lag(nonres))*100,2)
  ) %>% ungroup()

# calculate year to year change and pivot to long data
mnth_lg <- mnth %>% arrange(year, month) %>% 
  mutate(
    yoy_ch = round(((tot-lag(tot))/lag(tot))*100,2) 
  ) %>% 
  pivot_longer(
    3:4, names_to = "location", values_to = "count"
  )

# calculate percent change from 2014 to 2021 averages
mnth_pc <- mnth %>% filter(year != 2022) %>% group_by(month) %>% mutate(
  mn_res = round(mean(res)),
  mn_nres = round(mean(nonres)),
  pct_d_res  = round(100*((res-mn_res)/mn_res),1),
  pct_d_nres = round(100*((nonres-mn_nres)/mn_nres),1)
) 


# set colors ####
  
  yr <- seq(2014, 2022, by=1)
  lc_col <- c(rep("black", 7), c_2021,  c_2022)
  
  lab_c <- data.frame(year = yr, color = lc_col)
  lab_c$month <- "December"
  
  lab_c$color[lab_c$year == 2019]
  
# resident color  
res_c_2022 <- "#1b7837"

# nonresident color 
nres_c_2022 <- "#762a83"

# create plot #### 
ggplot()+
  # major y axis grid lines
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  
  # scale
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  # yearly nonresident lines
  geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#8c6bb1", size = .35, alpha = .2)+
  
  # yearly resident lines
  geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#41ae76", size = .35, alpha = .2)+
  
  # 2022 resident line
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = .75)+
  
  # 2022 nonresident line
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = .75)+
  
  # average nonresident line
  geom_line(data = mnth_2 %>% group_by(month) %>% filter(year != 2022) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = "gray30", linewidth = .25)+
  
  # average resident line
  geom_line(data = mnth_2 %>% group_by(month) %>%  filter(year != 2022) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = "gray30", linewidth = .25)+  
  
  # expand x scale to make room for labels
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 1.5)))+
  
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
               label = "2022 resident abortions", label.colour = NA, fill = panel_c, size = rel(3), 
               color = res_c_2022, hjust = 0, vjust = .8,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for average nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                                 year == 2022) %>% pull(8)),
               label = "Average nonresident abortions 2014:2021", label.colour = NA, fill = panel_c, size = rel(2.35),
               color = "grey30", hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for average resident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)) %>% filter(month == "December" & 
                                                                                             year == 2022) %>% pull(8)),
               label = "Average resident abortions 2014:2021", label.colour = NA, fill = panel_c, size = rel(2.35),
               color = "grey30", hjust = 0, vjust = .45,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = "2022 nonresident abortions", label.colour = NA, fill = panel_c, size = rel(3),
               color = nres_c_2022, hjust = 0, vjust = .35,  box.color = NA, width = unit(1.25, "inch"))+
  
  # nonresident July annotation
  geom_textbox(aes( x = 6.4,
                    y = mnth_2 %>% filter(year == 2022 & month == "July") %>% arrange(nonres) %>% pull(4)),
               label = paste0("Nonresident abortions reach an all-time high of ",
                              mnth$nonres[mnth$year == 2022 & mnth$month == "July"],
                              " in July 2022, ",
                              ((mnth$nonres[mnth$year == 2022 & mnth$month == "July"]-
                              mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])/
                              mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])*100,
                              "% above average."),
               label.colour = NA, fill = panel_c, size = rel(4),
               color = nres_c_2022, hjust = 1, vjust = .85,  box.color = NA, width = unit(2.25, "inch"))+
  
  # label for 2022 October resident
  geom_textbox(aes( x = 10.25,
                    y = mnth_2 %>% filter(year == 2022 & month == "October") %>% arrange(res) %>% pull(3)),
               label = paste0("Resident abortions reach an all-time low in October 2022, ",
                              round(((mnth$nonres[mnth$year == 2022 & mnth$month == "October"]-
                              mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "October"])/
                              mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "October"])*100),
                              "% below average."),
               label.colour = NA, fill = panel_c, size = rel(2.85),
               color = res_c_2022, hjust = 0, vjust = .85,  box.color = NA, width = unit(1.25, "inch"))+
  
  # labels for resident years 
  geom_text_repel(data= mnth %>% filter (month == "January"),
                  aes(.95, res, label = year),
                  xlim = 0.2,
                  point.padding = .015,
                  box.padding = .2,
                  nudge_y = 3, 
                  #nudge_x = .2, # .7
                  force = 1,
                  color = "gray55",
                  segment.color = "gray85",
                  size = rel(2.15), 
                  fontface = "plain", 
                  direction = "y",
                  segment.size = 0.2,
                  #min.segment.length = 3.05,
                  hjust = 1,
                  seed = 2
    )+
  # label for nonresident years - not enough separation to label individually.
  geom_textbox(aes(x = .03,
                   y = 110),
               label = "Each line represents a year between 2014 to 2022.", label.colour = NA, fill = panel_c, size = rel(2.15),
               color = "gray60", hjust = 0, vjust = .55,  box.color = NA, width = unit(.65, "inch"))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    )+
  
  # titles and caption
  labs(
    title = 
      paste0("Following the post-Dobbs enactment of abortion bans in neighboring states, 
      Indiana's
      <span style= 'color:",nres_c_2022,"'>nonresident abortions</span> surged to
      record highs in 2022, soaring ", 
      formatC(((mnth$nonres[mnth$year == 2022 & mnth$month == "July"]-
      mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])/
      mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])*100,
      format = "f", digits = 0, big.mark = ","),
      "% past their average July level and ending ",
      round(((mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2022) %>% pull(2)-
      mnth %>% filter(year != 2022) %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
      summarise(mean(tot)))/mnth %>% filter(year != 2022) %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
      summarise(mean(tot)))*100),
      "% above the 2014 to 2021 annual average."),
    
    subtitle = 
      paste0("Monthly <span style= 'color:",res_c_2022,"'>resident abortions</span> were 
      higher than average in 2022 until September. They fell to 
       ",
      mnth$res[mnth$year == 2022 & mnth$month == "October"],
      " in October, their lowest level of the past nine years; total <span style= 'color:",res_c_2022,"'>resident abortions</span> in 2022 ended ",
      round(abs(((mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2022) %>% pull(2)-
      mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2021) %>% pull(2))/
      mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2021) %>% pull(2))*100)),
      "% below average."),
    
    caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
    **Graphic:** Ted Schurter 2023"
    )

ggsave("plots/month_yr_ab_res_2022.svg", width = 3200, height = 1725, unit = "px",
       plot = last_plot())
#



