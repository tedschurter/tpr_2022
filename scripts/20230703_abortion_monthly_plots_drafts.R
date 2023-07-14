library(tidyverse)
library(ggplot2)
library(plotly)


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
                                             lineheight = 1,margin = margin(0, 0, .4, 0, unit = "cm"),),
      plot.caption = element_textbox_simple(family = "sans", size = rel(.7),
                                            color = "gray40", halign = 1,
                                            lineheight = 1.2),
      plot.title.position = "plot",
      plot.caption.position = "panel", 
      plot.margin = margin(.5, .5, .5,.5, unit = "cm"),
      plot.background = element_rect(fill  = panel_c,
                                     color = panel_c),
      panel.background = element_rect(fill = panel_c,
                                      color = panel_c)
    )
}


# set colors 

# panel color
panel_c <- "#fdfdf2"

# 2021 color 
c_2021 <- "#1b7837"

# 2022 color 
c_2022 <- "#762a83"

yr <- seq(2014, 2022, by=1)
lc_col <- c(rep("black", 7), c_2021,  c_2022)

lab_c <- data.frame(year = yr, color = lc_col)
lab_c <- as_tibble(lab_c)
#
ggplot()+ theme_classic()+
  # lines for all months
  geom_line(data = mnth %>% group_by(year) %>% arrange(desc(tot)),
            aes(month, tot, group = year), color = "gray90", size = .25)+
  
  # Dobbs leak text and marker 
  # vert line
  geom_segment(aes(x=5+(2/31), xend = 5+(2/31),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 5, y = 200,
        label = "**May 2, 2022:**<br><br>Draft of US Supreme Court Dobbs v. Jackson decision is 
        leaked, raising speculation about overturn of Roe v. Wade."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0, box.color = NA, width = unit(.9, "inch"))+
  
  # Dobbs text and marker line
  # vert line
  geom_segment(aes(x=6+(24/30), xend = 6+(24/30),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label 
  geom_textbox(
    aes(x= 6+(24/30)-.01, y = 200,
        label = "**June 24, 2022:**<br><br>Dobbs decision overturning 
        Roe v. Wade and removing constitutional right to abortion is released."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0, box.color = NA, width = unit(1.15, "inch"))+
  
  # Aug. 5 text and marker line
  # vert line
  geom_segment(aes(x=8+(5/31), xend = 8+(5/31),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 8+(5/31)-.01, y = 200,
        label = "**Aug. 5, 2022:**<br><br>Indiana passes legislation prohibiting 
    abortion from conception with few exceptions."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0,  box.color = NA, width = unit(.9, "inch"))+
  
  # text and line for Sept. 15 law going into effect
  # vert line
  geom_segment(aes(x=9+(15/30), xend = 9+(15/30),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 9+(15/30)-.01, y = 200,
        label = "**Sept. 15, 2022:**<br><br>Indiana law goes into effect."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 1, vjust = 0,  box.color = NA, width = unit(.9, "inch"))+
  
  # text and line for Sept. 22 law blocked
  # vert line
  geom_segment(aes(x=9+(22/30), xend = 9+(22/30),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 9+(22/30)+.01, y = 1150,
        label = "**Sept. 22, 2022:**<br><br>Indiana Circuit Court Judge Kelsey 
        Blake Hanlon blocks new law because it restricts bodily autonomy."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 0, vjust = 1,  box.color = NA, width = unit(1.08, "inch"))+
  
  # Marion judge again blocks law
  # vert line
  geom_segment(aes(x=12+(2/31), xend = 12+(2/31),
                   y = 200, yend = 1200),
               color = "gray80", linetype = "longdash", linewidth = .15)+
  # label
  geom_textbox(
    aes(x= 12+(2/31)+.01, y = 1150,
        label = "**Dec. 2, 2022:**<br><br>Marion County Judge Heather Welch 
        blocks new law because it likely violates Indiana’s Religious Freedom 
        Restoration Act."),
    label.colour = NA, fill = panel_c,
    size = rel(2.65), color = "gray50", 
    hjust = 0, vjust = 1,  box.color = NA, width = unit(1.08, "inch"))+
  
  # monthly average indicator line
  geom_segment(aes(x=.3, xend = 12,
                   y = mean(mnth$tot), yend = mean(mnth$tot)),
               color = "gray65", linewidth = .2) +
  
  # monthly average line labels
  # geom_textbox(
  #   aes(x= .2, y = 120+mean(mnth$tot)),
  #   label = "Above line = more than the overall monthly average", 
  #   label.colour = NA, fill = panel_c,
  #   size = rel(2.25), color = "gray50", 
  #   hjust = 0, box.color = NA,   width = unit(.6, "inch"))+
  # geom_textbox(
  #   aes(x= .2, y = mean(mnth$tot) - 120),
  #   label = "Below line = less than the overall monthly average.", 
  #   label.colour = NA, fill = panel_c,
  #   size = rel(2.25), color = "gray50", 
  #   hjust = 0, box.color = NA,   width = unit(.6, "inch"))+
  
  # monthly average label
  geom_textbox(
    aes(x= 0, y = mean(mnth$tot)),
    label = "Overall monthly average", 
    label.colour = NA, fill = panel_c,
    size = rel(1.9), color = "gray50", 
    hjust = .5, box.color = NA,   width = unit(.4, "inch"))+
  
  # line for 2021 totals
  geom_line(data = mnth %>% filter(year == 2021),
            aes(month, tot, group = year), color = c_2021, size = .5)+
  # line for 2022 totals
  geom_line(data = mnth %>% filter(year == 2022),
            aes(month, tot, group = year), color = c_2022, size = .5)+
  # labels for years 
  geom_text_repel(data= mnth %>% group_by(year) %>% mutate(a_tot = sum(res)+sum(nonres)) %>% filter(month == "December"),
                  aes(12.25, tot, label = paste0(year, ":  ", prettyNum(a_tot, big.mark = ",", scientific = FALSE))),
                  #xlim = c(2021.4),
                  point.padding = .025,
                  box.padding = .2,
                  #nudge_y = -.2, 
                  nudge_x = 1.2, # .7
                  force = 1,
                  color = lab_c$color, 
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
    aes(x= 5.18, y = 1135),
    label = paste0("Monthly abortions peaked in July <span style= 
    'color:",c_2022,"'>2022 </span> at 1,182."),
    label.colour = NA, fill = panel_c,
    size = rel(2.75), color = "gray30", 
    hjust = 0, box.color = NA,   width = unit(1.1, "inch"))+
  
  
  scale_y_continuous(limits = c(200, 1200),
                     breaks = c(200, 400, 600, 800, 1000, 1200),
                     labels = c("200", "400", "600", "800", "1,000", "1,200"),
                     name = "Abortions")+
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
  labs(title = 
         paste0("Though the draft of the Dobbs decision to end federal abortion 
    protection began circulating in May 2022, the biggest impact on Indiana's <span style= 
    'color:",c_2022,"'>2022 </span>monthly abortion totals occurred in July when they
    jumped 65% from the same month in <span style= 
    'color:",c_2021,"'>2021 </span>to a nine-year high of 1,182. They fell to 447 by 
                October, their lowest total since 2014. "),
       
       caption = 
         "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
    **Graphic:** Ted Schurter 2023")

####

mnth_lg <- mnth %>% arrange(year, month) %>% 
  mutate(
    yoy_ch = round(((tot-lag(tot))/lag(tot))*100,2) 
  ) %>% 
  pivot_longer(
    3:4, names_to = "location", values_to = "count"
  )


mnth_2 <- mnth_lg

loc_lab <- c(res = "Resident abortions", nonres = "Nonresident abortions")

ggplot()+ theme_classic()+
  # lines for all months
  geom_line(data = mnth_lg %>% group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "gray90", size = .25)+
  geom_line(data = mnth_lg %>% group_by(year) %>% filter(year == 2022) %>% arrange(desc(count)),
            aes(month, count, group = year), color = c_2022, size = .25)+
  geom_line(data = mnth_lg %>% group_by(year) %>% filter(year == 2021) %>% arrange(desc(count)),
            aes(month, count, group = year), color = c_2021, size = .25)+
  facet_wrap(~location, ncol = 1, scales = "fixed", labeller = as_labeller(loc_lab),
             strip.position = "top")+

  t_theme()+
  theme(
  panel.grid.major.y = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(hjust = .5),
  #plot.title = element_markdown()
  )+
    labs(
      title = 
      
      
      subtitle = 
        paste0("Resident abortions ", if_else(
          abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
            abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
          "grew ", "fell "), if_else(
            abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
              abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
            mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6),
            mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6) 
          ),"% in ", if_else(
            abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
              abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
            mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(2),
            mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(2)),
          " from their <span style= 'color:",c_2021,"'>2021 </span> levels and down ",
          round(((mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(3)-
                    mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res) %>% pull(3))/
                   mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(3))*100),"
 % from their peak in ", mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(2),
          " 2022.")
    )
  
  NULL 
  
 # working version 
  
  mnth_2 <- mnth %>% group_by(month) %>% arrange(year, month) %>%
    mutate(
      res_yoy = round(((res-lag(res))/lag(res))*100,2),
      n_res_yoy = round(((nonres-lag(nonres))/lag(nonres))*100,2)
    ) %>% ungroup()
  
  
  # 2021 color 
  c_2021 <- "#1b7837"
  
  # 2022 color 
  c_2022 <- "#762a83"
  
  yr <- seq(2014, 2022, by=1)
  lc_col <- c(rep("black", 7), c_2021,  c_2022)
  
  lab_c <- data.frame(year = yr, color = lc_col)
  lab_c$month <- "December"
  
  lab_c$color[lab_c$year == 2019]
  
  ggplot()+ 
    # lines for all months
    geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
              aes(month, count, group = year), color = "gray90", size = .25)+
    geom_line(data = mnth_lg %>% group_by(year) %>% filter(year == 2022 & location == "nonres") %>% arrange(desc(count)),
              aes(month, count, group = year), color = c_2022, size = .25)+
    geom_line(data = mnth_lg %>% group_by(year) %>% filter(year == 2021 & location == "nonres") %>% arrange(desc(count)),
              aes(month, count, group = year), color = c_2021, size = .25)+
    t_theme()+
    theme(
      panel.grid.major.y = element_blank(),
    )+
    
    
    #data= mnth %>% group_by(year) %>% mutate(a_tot = sum(res)+sum(nonres)) %>% filter(month == "December"),
    # aes(12.25, tot, label = paste0(year, ":  ", prettyNum(a_tot, big.mark = ",", scientific = FALSE))),
    
    # labels for years 
    geom_text_repel(data= mnth_lg %>% filter(location == "nonres") %>% 
      group_by(year) %>% mutate(a_tot = sum(count)) %>% filter(month == "December"),
                    aes(12.25, count, label = paste0(year, ":  ", prettyNum(a_tot, big.mark = ",", scientific = FALSE))),
                    #xlim = c(2021.4),
                    point.padding = .025,
                    box.padding = .2,
                    #nudge_y = -.2, 
                    nudge_x = 1.2, # .7
                    force = 1,
                    color = lab_c$color,
                    segment.color = "gray75",
                    size = rel(2.35), 
                    fontface = "plain", 
                    direction = "y",
                    segment.size = 0.1,
                    min.segment.length = .15,
                    hjust = .4,
                    seed = 2
    )+
    labs(
      title = 
        paste0("Nonresident abortions in Indiana spiked in 2022 to an all time high of ",
               mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(4)," in ",     
               mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(2), ", ",
               formatC(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(7), 
                       format = "f", digits = 0, big.mark = ","), "% higher than July 2021."),
      subtitle = paste0(
        "For the year, nonresident abortions were up ", 
        round((mnth_lg %>% filter(year == 2022 & location == "nonres") %>% summarise(tot = sum(count))-
                 mnth_lg %>% filter(year == 2021 & location == "nonres") %>% summarise(tot = sum(count)))/
                mnth_lg %>% filter(year == 2021 & location == "nonres") %>% summarise(tot = sum(count))*100),
        "%."))
  
  
  #
  
  ####    
  
  
  
  paste0("Resident abortions ", if_else(
    abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
      abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
    "grew ", "fell "), if_else(
      abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
        abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
      mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6),
      mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6) 
    ),"% in ", if_else(
      abs(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(6)) >
        abs(mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(6)),
      mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res_yoy) %>% pull(2),
      mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res_yoy) %>% pull(2)),
    " from their <span style= 'color:",c_2021,"'>2021 </span> levels and down ",
    round(((mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(3)-
              mnth_2 %>% filter(year == 2022) %>% slice_min(order_by = res) %>% pull(3))/
             mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(3))*100),"
 % from their peak in ", mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = res) %>% pull(2),
    " 2022.")


  
# ####

  ggplot()+
    geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)) %>% 
                filter(year == 2016),
              aes(month, count, group = year), color = "gray90", size = .25)+
    geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)) %>% 
               filter(year == 2016),
              aes(month, count, group = year), color = "gray90", size = .25)+
    geom_line(data = mnth_2 %>% filter(year == 2022),
              aes(month, res, group = year),
              color = c_2022, linewidth = .8)+
    geom_line(data = mnth_2 %>% filter(year == 2021),
              aes(month, res, group = year),
              color = "gray90", linewidth = .25)+
    # geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_res = mean(res)),
    #           aes(month, mn_res, group = year),
    #           color = c_2022, linewidth = .8, linetype = "dotted")+
    geom_line(data = mnth_2 %>% filter(year == 2022),
              aes(month, nonres, group = year),
              color = c_2021, linewidth = .8)+
    geom_line(data = mnth_2 %>% filter(year == 2021),
              aes(month, nonres, group = year),
              color = "gray90", linewidth = .25)+
    # geom_line(data = mnth_2 %>% group_by(month)  %>% mutate(mn_res = mean(nonres)),
    #           aes(month, mn_res, group = year),
    #           color = c_2021, linewidth = .8, linetype = "dotted")+
   
    t_theme()+
    theme(
      panel.grid.major.y = element_blank()
    )+
    labs(
      title = 
      paste0("Nonresident abortions in Indiana spiked in 2022 to an all time high of ",
         mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(4)," in ",     
         mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(2), ", ",
         formatC(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(7), 
         format = "f", digits = 0, big.mark = ","), "% higher than July 2021.") 
    )
  
  
# previous high year was 2022, low was 2016
  
# with geom ribbon 
  
ggplot()+
  geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>% 
                mutate(ymax = mean(res)+(2*sd(res)),
                       ymin = mean(res)-(2*sd(res))),
              aes(x = month, y = mean(res), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "gray90", alpha = .45
  )+
  geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>% 
                mutate(ymax = mean(res)+(1*sd(res)),
                       ymin = mean(res)-(1*sd(res))),
              aes(x = month, y = mean(res), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "white", alpha = .55
  )+
  geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>% 
                mutate(ymax = mean(nonres)+(2*sd(nonres)),
                       ymin = mean(nonres)-(2*sd(nonres))),
              aes(x = month, y = mean(nonres), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "gray90", alpha = .45
  )+
  geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>% 
                mutate(ymax = mean(nonres)+(1*sd(nonres)),
                       ymin = mean(nonres)-(1*sd(nonres))),
              aes(x = month, y = mean(nonres), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "white", alpha = .55
  )+
  # geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray90", size = .25)+
  # geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray90", size = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = c_2022, linewidth = .8)+
  # geom_line(data = mnth_2 %>% filter(year == 2021),
  #           aes(month, res, group = year),
  #           color = c_2022, linewidth = .15, alpha = .5)+
  geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_res = mean(res)),
            aes(month, mn_res, group = year),
            color = c_2022, linewidth = .8, linetype = "dotted", alpha  = 0.03)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = c_2021, linewidth = .8)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = c_2021, linewidth = .25, alpha = .5)+
  geom_line(data = mnth_2 %>% group_by(month)  %>% mutate(mn_res = mean(nonres)),
            aes(month, mn_res, group = year),
            color = c_2021, linewidth = .8, linetype = "dotted", alpha = .03)+

  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )
  
###


# resident colors


res_c_2022 <- "#005824"
res_c_2021 <- "#238b45"
res_c_2016 <- "#41ae76"


# nonresident color 
nres_c_2022 <- "#6e016b"
nres_c_2nd <- "#88419d"
nres_c_low <- "#8c6bb1"


ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray95", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, res, group = year),
            color = res_c_2021, linewidth = .5)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, res, group = year),
            color = res_c_2016, linewidth = .5, alpha = .3)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, nonres, group = year),
            color = nres_c_2021, linewidth = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, nonres, group = year),
            color = nres_c_2016, linewidth = .25, alpha = .3)+
    scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  geom_textbox(aes( x = 12.45, 
                    y = 725),
               label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  geom_textbox(aes( x = 12.15, 
                    y = 725),
               label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
           y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
  label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                 ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
  color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  # 2021 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # 2016 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # minimum total nonresident abortions
  geom_textbox(
    aes( x = 12.15,
         y = mnth_2 %>% 
           filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
                    arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
                    month == "December") %>% pull(4)),
        label = paste0(
        mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
        slice_min(order_by = tot) %>% pull(1), 
        ":     ",
        mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
        group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
        pull(1)) %>% pull(2)),
      label.colour = NA, fill = panel_c, size = rel(2.45),
     color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # 2018 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
             slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
             slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )

#
100*((1827-384)/384)









paste0("<span style= 'color:",nres_c_2021,"'>Nonresident abortions </span>in Indiana spiked in 2022 to an all time high of ",
       mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(4)," in ",     
       mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(2), ", ",
       formatC(mnth_2 %>% filter(year == 2022) %>% slice_max(order_by = n_res_yoy) %>% pull(7), 
               format = "f", digits = 0, big.mark = ","), "% higher than July 2021.") 
# 
paste0("In ", mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
         slice(1) %>% pull(1),
       " <span style= 'color:",nres_c_2021,"'>Nonresident </span>abortions were ", round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
         slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
  slice(2) %>% pull(2))*100),
  "% higer than ",
  mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
    slice(2) %>% pull(1),"the second highest year.")

mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(2)



100*((1827-455)/455)



  
# unused elements
# geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>% 
#               mutate(ymax = mean(res)+(2*sd(res)),
#                      ymin = mean(res)-(2*sd(res))),
#             aes(x = month, y = mean(res), group=1,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = "gray90", alpha = .45
# )+
# geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>% 
#               mutate(ymax = mean(res)+(1*sd(res)),
#                      ymin = mean(res)-(1*sd(res))),
#             aes(x = month, y = mean(res), group=1,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = "white", alpha = .55
# )+
# geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>% 
#               mutate(ymax = mean(nonres)+(2*sd(nonres)),
#                      ymin = mean(nonres)-(2*sd(nonres))),
#             aes(x = month, y = mean(nonres), group=1,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = "gray90", alpha = .45
# )+
# geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>% 
#               mutate(ymax = mean(nonres)+(1*sd(nonres)),
#                      ymin = mean(nonres)-(1*sd(nonres))),
#             aes(x = month, y = mean(nonres), group=1,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = "white", alpha = .55
#)+
# geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
#           aes(month, count, group = year), color = "gray92", size = .25)+
# geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
#           aes(month, count, group = year), color = "gray92", size = .25)+
  




# Working version 07042023 12:14 pm

ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#8c6bb1", size = .25, alpha = .2)+
  geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#41ae76", size = .25, alpha = .2)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% filter(year != 2022) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = nres_c_2022, linewidth = .25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>%  filter(year != 2022) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = res_c_2022, linewidth = .25)+  
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, mean(res)),
            color = res_c_2021, linewidth = .5)+
  
  # geom_line(data = mnth_2 %>% filter(year == 2021),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2021, linewidth = .25)+
  # geom_line(data = mnth_2 %>% filter(year == 2016),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2016, linewidth = .25, alpha = .3)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  # geom_textbox(aes( x = 12.45, 
  #                   y = 725),
  #              label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  # geom_textbox(aes( x = 12.15, 
  #                   y = 725),
  #              label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
geom_textbox(aes( x = 12.15, 
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
             label = "2022 resident abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
             color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # # 2021 resident abortions
  # geom_textbox(aes( x = 12.15, 
  #                   y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
  #              label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
  #                                                   %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
  #                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
  #              label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # # 2016 resident abortions
  # geom_textbox(aes( x = 12.15, 
#                   y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
#              label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
#                                                   %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45), 
#              color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+

  # average nonresident abortions
  geom_textbox(aes( x = 12.15,
                  y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                               year == 2022) %>% pull(8)),
             label = "Average nonresident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
             color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average resident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)) %>% filter(month == "December" & 
                                                                                             year == 2022) %>% pull(8)),
               label = "Average resident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = res_c_2022, hjust = 0, vjust = .45,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = "2022 nonresident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # # minimum total nonresident abortions
  # geom_textbox(
  #   aes( x = 12.15,
  #        y = mnth_2 %>% 
  #          filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  #                   arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
  #                   month == "December") %>% pull(4)),
  #   label = paste0(
  #     mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
  #       slice_min(order_by = tot) %>% pull(1), 
  #     ":     ",
#     mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
#                                                                         group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
#                                                                         pull(1)) %>% pull(2)),
#   label.colour = NA, fill = panel_c, size = rel(2.45),
#   color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
# # 2018 nonresident abortions
# geom_textbox(aes( x = 12.15,
#                   y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
#              label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
#                                                   %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45),
#              color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+


t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )




# 070423 3 pm


ggplot()+
  # major y axis grid lines
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  # yearly nonres lines
  geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#8c6bb1", size = .25, alpha = .2)+
  # yearly resident lines
  geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "#41ae76", size = .25, alpha = .2)+
  # 2022 resident line
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = .75)+
  # 2022 nonnresident line
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
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 1.5)))+
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
               label = "2022 resident abortions", label.colour = NA, fill = panel_c, size = rel(3), 
               color = res_c_2022, hjust = 0, vjust = .8,  box.color = NA, width = unit(1.25, "inch"))+
  
  # average nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                                 year == 2022) %>% pull(8)),
               label = "Average nonresident abortions 2014:2021", label.colour = NA, fill = panel_c, size = rel(2.35),
               color = "grey30", hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average resident abortions
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
  # label for 2022 nonresident abortions
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
  # label for 2022 resident October
  geom_textbox(aes( x = 10.25,
                    y = mnth_2 %>% filter(year == 2022 & month == "October") %>% arrange(res) %>% pull(3)),
               label = paste0("Resident abortions reach an all-time low in October 2022, ",
                              round(((mnth$nonres[mnth$year == 2022 & mnth$month == "October"]-
                                  mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "October"])/
                                 mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "October"])*100),
                              "% below average."),
               label.colour = NA, fill = panel_c, size = rel(2.85),
               color = res_c_2022, hjust = 0, vjust = .85,  box.color = NA, width = unit(1.25, "inch"))+
  geom_text_repel(data= mnth %>% filter (month == "January"),
                  aes(.95, res, label = year),
                  xlim = 0.04,
                  point.padding = .015,
                  box.padding = .2,
                  nudge_y = 2, 
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
  geom_textbox(aes(x = .03,
                   y = 110),
               label = "Each line represents a year between 2014 to 2022.", label.colour = NA, fill = panel_c, size = rel(2.15),
               color = "gray60", hjust = 0, vjust = .55,  box.color = NA, width = unit(.65, "inch"))+
t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    
  )+
  
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
      summarise(mean(tot)))/
      mnth %>% filter(year != 2022) %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
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
        
ggsave("plots/res_nonres.svg", width = 3200, height = 1700,
           units = "px")
        

# old title 
# paste0("In ", 
#        mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
#          slice(1) %>% pull(1),
#        ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
#        round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
#                 slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
#                 slice(2) %>% pull(2))*100),
#        "% higer than ",
#        mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
#          slice(2) %>% pull(1),
#        ", the previous nine-year high.")

mnth_pc <- mnth %>% filter(year != 2022) %>% group_by(month) %>% mutate(
  mn_res = round(mean(res)),
  mn_nres = round(mean(nonres)),
  pct_d_res  = round(100*((res-mn_res)/mn_res),1),
  pct_d_nres = round(100*((nonres-mn_nres)/mn_nres),1)
  ) 


  formatC( ((mnth$nonres[mnth$year == 2022 & mnth$month == "July"]-
             mnth_pc$pct_d_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])/
            mnth_pc$pct_d_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])*100,
        format = "f", digits = 0, big.mark = ",")

((mnth$nonres[mnth$year == 2022 & mnth$month == "July"]-
mnth_pc$pct_d_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])/
  mnth_pc$pct_d_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])*100


((mnth$nonres[mnth$year == 2022 & mnth$month == "July"]-
    mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])/
    mnth_pc$mn_nres[mnth_pc$year == 2021 & mnth_pc$month == "July"])*100


round(((mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2022) %>% pull(2)-
mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2021) %>% pull(2))/
mnth %>% group_by(year) %>% summarise(sum(res)) %>% filter(year == 2021) %>% pull(2))*100)


round(((mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2022) %>% pull(2)-
        mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2021) %>% pull(2))/
        mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2021) %>% pull(2))*100)


round(((mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2022) %>% pull(2)-
        mnth %>% group_by(year) %>% summarise(mean(nonres)) %>% filter(year == 2021) %>% pull(2))/
        mnth %>% group_by(year) %>% summarise(mean(nonres)) %>% filter(year == 2021) %>% pull(2))*100)


round(((mnth %>% group_by(year) %>% summarise(sum(nonres)) %>% filter(year == 2022) %>% pull(2)-
mnth %>% filter(year != 2022) %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  summarise(mean(tot)))/
  mnth %>% filter(year != 2022) %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  summarise(mean(tot)))*100)
  

100*((1827-540)/540)
100*((7702-7949)/7949)
#

older variations
p1 <- ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray95", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, res, group = year),
            color = res_c_2021, linewidth = .5)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, res, group = year),
            color = res_c_2016, linewidth = .5, alpha = .3)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, nonres, group = year),
            color = nres_c_2021, linewidth = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, nonres, group = year),
            color = nres_c_2016, linewidth = .25, alpha = .3)+
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  geom_textbox(aes( x = 12.45, 
                    y = 725),
               label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  geom_textbox(aes( x = 12.15, 
                    y = 725),
               label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  # 2021 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # 2016 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # minimum total nonresident abortions
  geom_textbox(
    aes( x = 12.15,
         y = mnth_2 %>% 
           filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
                    arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
                    month == "December") %>% pull(4)),
    label = paste0(
      mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
        slice_min(order_by = tot) %>% pull(1), 
      ":     ",
      mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
                                                                          group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
                                                                          pull(1)) %>% pull(2)),
    label.colour = NA, fill = panel_c, size = rel(2.45),
    color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # 2018 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )


#

p2 <- ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray95", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray95", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>%
                mutate(ymax = mean(res)+(2*sd(res)),
                       ymin = mean(res)-(2*sd(res))),
              aes(x = month, y = mean(res), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "gray90", alpha = .45
  )+
  geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>%
                mutate(ymax = mean(res)+(1*sd(res)),
                       ymin = mean(res)-(1*sd(res))),
              aes(x = month, y = mean(res), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "white", alpha = .55
  )+
  geom_ribbon(data = mnth  %>% group_by(month) %>% filter(year != 2022) %>%
                mutate(ymax = mean(nonres)+(2*sd(nonres)),
                       ymin = mean(nonres)-(2*sd(nonres))),
              aes(x = month, y = mean(nonres), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "gray90", alpha = .45
  )+
  geom_ribbon(data = mnth %>% group_by(month) %>% filter(year != 2022) %>%
                mutate(ymax = mean(nonres)+(1*sd(nonres)),
                       ymin = mean(nonres)-(1*sd(nonres))),
              aes(x = month, y = mean(nonres), group=1,
                  ymin = ymin,
                  ymax = ymax),
              fill = "white", alpha = .55
  )+
  geom_line(data = mnth_2 %>% filter(year != 2022) %>% group_by(month) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = nres_c_2022, linewidth = .25)+
  
  geom_line(data = mnth_2 %>% filter(year != 2022) %>% group_by(month) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = res_c_2022, linewidth = .25)+  
  
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  geom_textbox(aes( x = 12.45, 
                    y = 725),
               label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  geom_textbox(aes( x = 12.15, 
                    y = 725),
               label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )

p2

###




p3 <-  
  ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, res, group = year),
            color = res_c_2021, linewidth = .5)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, res, group = year),
            color = res_c_2016, linewidth = .5, alpha = .3)+
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, nonres, group = year),
            color = nres_c_2021, linewidth = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2016),
            aes(month, nonres, group = year),
            color = nres_c_2016, linewidth = .25, alpha = .3)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  geom_textbox(aes( x = 12.45, 
                    y = 725),
               label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  geom_textbox(aes( x = 12.15, 
                    y = 725),
               label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # 2021 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # 2016 resident abortions
  geom_textbox(aes( x = 12.15, 
                    y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
               label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                    %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45), 
               color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # minimum total nonresident abortions
  geom_textbox(
    aes( x = 12.15,
         y = mnth_2 %>% 
           filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
                    arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
                    month == "December") %>% pull(4)),
    label = paste0(
      mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
        slice_min(order_by = tot) %>% pull(1), 
      ":     ",
      mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
                                                                          group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
                                                                          pull(1)) %>% pull(2)),
    label.colour = NA, fill = panel_c, size = rel(2.45),
    color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # 2018 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  
  t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )


#

p4 <-  
  ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
            aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = nres_c_2022, linewidth = .25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = res_c_2022, linewidth = .25)+  
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, mean(res)),
            color = res_c_2021, linewidth = .5)+
  
  # geom_line(data = mnth_2 %>% filter(year == 2021),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2021, linewidth = .25)+
  # geom_line(data = mnth_2 %>% filter(year == 2016),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2016, linewidth = .25, alpha = .3)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  # geom_textbox(aes( x = 12.45, 
  #                   y = 725),
  #              label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # label header
  # geom_textbox(aes( x = 12.15, 
  #                   y = 725),
  #              label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
geom_textbox(aes( x = 12.15, 
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
             label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                  %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                            ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
             color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # # 2021 resident abortions
  # geom_textbox(aes( x = 12.15, 
  #                   y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
  #              label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
  #                                                   %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
  #                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
  #              label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # # 2016 resident abortions
  # geom_textbox(aes( x = 12.15, 
#                   y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
#              label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
#                                                   %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45), 
#              color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+

# average nonresident abortions
geom_textbox(aes( x = 12.15,
                  y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                               year == 2022) %>% pull(8)),
             label = "Average nonresident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
             color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average resident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)) %>% filter(month == "December" & 
                                                                                             year == 2022) %>% pull(8)),
               label = "Average resident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = res_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  
  # label for 2022 nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
               label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                    %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                              ""), label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # # minimum total nonresident abortions
  # geom_textbox(
  #   aes( x = 12.15,
  #        y = mnth_2 %>% 
  #          filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  #                   arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
  #                   month == "December") %>% pull(4)),
  #   label = paste0(
  #     mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
  #       slice_min(order_by = tot) %>% pull(1), 
  #     ":     ",
#     mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
#                                                                         group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
#                                                                         pull(1)) %>% pull(2)),
#   label.colour = NA, fill = panel_c, size = rel(2.45),
#   color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
# # 2018 nonresident abortions
# geom_textbox(aes( x = 12.15,
#                   y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
#              label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
#                                                   %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45),
#              color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+


t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )

#

p5 <- 
  ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  # geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray92", size = .25)+
  # geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = nres_c_2022, linewidth = .25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = res_c_2022, linewidth = .25)+  
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, mean(res)),
            color = res_c_2021, linewidth = .5)+
  
  # geom_line(data = mnth_2 %>% filter(year == 2021),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2021, linewidth = .25)+
  # geom_line(data = mnth_2 %>% filter(year == 2016),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2016, linewidth = .25, alpha = .3)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  # geom_textbox(aes( x = 12.45, 
  #                   y = 725),
  #              label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # # label header
  # geom_textbox(aes( x = 12.15, 
  #                   y = 725),
  #              label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
geom_textbox(aes( x = 12.15, 
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
             label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                  %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                            ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
             color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # # 2021 resident abortions
  # geom_textbox(aes( x = 12.15, 
  #                   y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
  #              label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
  #                                                   %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
  #                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
  #              label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # # 2016 resident abortions
  # geom_textbox(aes( x = 12.15, 
#                   y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
#              label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
#                                                   %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45), 
#              color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+

# label for 2022 nonresident abortions
geom_textbox(aes( x = 12.15,
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
             label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                  %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                            ""), label.colour = NA, fill = panel_c, size = rel(2.45),
             color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                                 year == 2022) %>% pull(8)),
               label = "Average nonresident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average resident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)) %>% filter(month == "December" & 
                                                                                             year == 2022) %>% pull(8)),
               label = "Average resident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = res_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # # minimum total nonresident abortions
  # geom_textbox(
  #   aes( x = 12.15,
  #        y = mnth_2 %>% 
  #          filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  #                   arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
  #                   month == "December") %>% pull(4)),
  #   label = paste0(
  #     mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
  #       slice_min(order_by = tot) %>% pull(1), 
  #     ":     ",
#     mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
#                                                                         group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
#                                                                         pull(1)) %>% pull(2)),
#   label.colour = NA, fill = panel_c, size = rel(2.45),
#   color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
# # 2018 nonresident abortions
# geom_textbox(aes( x = 12.15,
#                   y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
#              label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
#                                                   %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45),
#              color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+


t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )

#
p6 <- ggplot()+
  geom_segment(aes(x = 0, xend = 12, y = 0, yend = 0), color     = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 200, yend = 200), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 400, yend = 400), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 600, yend = 600), color = "gray90", size = .25)+
  geom_segment(aes(x = 0, xend = 12, y = 800, yend = 800), color = "gray90", size = .25)+
  scale_y_continuous(limits  = c(0, max(mnth$res)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = c("0", "200", "400", "600", "800"))+
  
  # geom_line(data = mnth_lg %>% filter(location == "nonres") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray92", size = .25)+
  # geom_line(data = mnth_lg %>% filter(location == "res") %>%  group_by(year) %>%  arrange(desc(count)),
  #           aes(month, count, group = year), color = "gray92", size = .25)+
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, res, group = year),
            color = res_c_2022, linewidth = 1)+
  
  geom_line(data = mnth_2 %>% filter(year == 2022),
            aes(month, nonres, group = year),
            color = nres_c_2022, linewidth = 1.25)+
  
  geom_line(data = mnth_2 %>% group_by(month) %>% filter(year != 2022) %>% mutate(mn_nr = mean(nonres)),
            aes(month, mn_nr, group = year),
            color = nres_c_2022, linewidth = .25)+
  
  geom_line(data = mnth_2 %>% filter(year != 2022) %>% group_by(month) %>% mutate(mn_r = mean(res)),
            aes(month, mn_r, group = year),
            color = res_c_2022, linewidth = .25)+  
  
  geom_line(data = mnth_2 %>% filter(year == 2021),
            aes(month, mean(res)),
            color = res_c_2021, linewidth = .5)+
  
  # geom_line(data = mnth_2 %>% filter(year == 2021),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2021, linewidth = .25)+
  # geom_line(data = mnth_2 %>% filter(year == 2016),
  #           aes(month, nonres, group = year),
  #           color = nres_c_2016, linewidth = .25, alpha = .3)+
  
  scale_x_discrete(expand = expansion(mult = 0, add = c(0, 2)))+
  # label header
  # geom_textbox(aes( x = 12.45, 
  #                   y = 725),
  #              label = "Total abortions", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(1.25, "inch"))+
  # # label header
  # geom_textbox(aes( x = 12.15, 
  #                   y = 725),
  #              label = "Year", label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = "gray30", hjust = 0, vjust = 1.65,  box.color = NA, width = unit(.25, "inch"))+
  # label for 2022 resident abortions
geom_textbox(aes( x = 12.15, 
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(res) %>% pull(3)),
             label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
                                                  %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                            ""), label.colour = NA, fill = panel_c, size = rel(2.45), 
             color = res_c_2022, hjust = 0, vjust = .65,  box.color = NA, width = unit(1.25, "inch"))+
  
  # # 2021 resident abortions
  # geom_textbox(aes( x = 12.15, 
  #                   y = mnth_2 %>% filter(year == 2021 & month == "December") %>% arrange(res) %>% pull(3)),
  #              label = paste0("2021:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
  #                                                   %>% filter(year == 2021) %>% pull(2), format = "f", digits = 0, big.mark = ","),
  #                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
  #              label.colour = NA, fill = panel_c, size = rel(2.45), 
  #              color = res_c_2021, hjust = 0, vjust = .5,  box.color = NA, width = unit(1.25, "inch"))+
  # # 2016 resident abortions
  # geom_textbox(aes( x = 12.15, 
#                   y = mnth_2 %>% filter(year == 2016 & month == "December") %>% arrange(res) %>% pull(3)),
#              label = paste0("2016:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(res)) 
#                                                   %>% filter(year == 2016) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45), 
#              color = res_c_2016, hjust = 0, vjust = .15,  box.color = NA, width = unit(1.25, "inch"))+

# label for 2022 nonresident abortions
geom_textbox(aes( x = 12.15,
                  y = mnth_2 %>% filter(year == 2022 & month == "December") %>% arrange(nonres) %>% pull(4)),
             label = paste0("2022:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
                                                  %>% filter(year == 2022) %>% pull(2), format = "f", digits = 0, big.mark = ","),
                            ""), label.colour = NA, fill = panel_c, size = rel(2.45),
             color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average nonresident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_nr = mean(nonres)) %>% filter(month == "December" & 
                                                                                                 year == 2022) %>% pull(8)),
               label = "Average nonresident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = nres_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # average resident abortions
  geom_textbox(aes( x = 12.15,
                    y = mnth_2 %>% group_by(month) %>% mutate(mn_r = mean(res)) %>% filter(month == "December" & 
                                                                                             year == 2022) %>% pull(8)),
               label = "Average resident abortions", label.colour = NA, fill = panel_c, size = rel(2.45),
               color = res_c_2022, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
  # # # minimum total nonresident abortions
  # geom_textbox(
  #   aes( x = 12.15,
  #        y = mnth_2 %>% 
  #          filter(year == mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% 
  #                   arrange(desc(tot)) %>% slice_min(order_by = tot) %>% pull(1) & 
  #                   month == "December") %>% pull(4)),
  #   label = paste0(
  #     mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
  #       slice_min(order_by = tot) %>% pull(1), 
  #     ":     ",
#     mnth %>% group_by(year) %>% summarise(tot=sum(nonres)) %>% filter(year == mnth %>% 
#                                                                         group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% slice_min(order_by = tot) %>% 
#                                                                         pull(1)) %>% pull(2)),
#   label.colour = NA, fill = panel_c, size = rel(2.45),
#   color = nres_c_low, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+
# # 2018 nonresident abortions
# geom_textbox(aes( x = 12.15,
#                   y = mnth_2 %>% filter(year == 2018 & month == "December") %>% arrange(nonres) %>% pull(4)),
#              label = paste0("2018:     ", formatC(mnth %>% group_by(year) %>% summarise(tot=sum(nonres))
#                                                   %>% filter(year == 2018) %>% pull(2), format = "f", digits = 0, big.mark = ","),
#                             ""), label.colour = NA, fill = panel_c, size = rel(2.45),
#              label.colour = NA, fill = panel_c, size = rel(2.45),
#              color = nres_c_2nd, hjust = 0, vjust = .55,  box.color = NA, width = unit(1.25, "inch"))+


t_theme()+
  theme(
    panel.grid.major.y = element_blank()
  )+
  
  labs(
    title = 
      paste0("In ", 
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(1) %>% pull(1),
             ", total <span style= 'color:",nres_c_2021,"'>nonresident </span>abortions were ", 
             round((mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(1) %>% pull(2)/mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
                      slice(2) %>% pull(2))*100),
             "% higer than ",
             mnth %>% group_by(year) %>% summarise(tot = sum(nonres)) %>% arrange(desc(tot)) %>% 
               slice(2) %>% pull(1),
             ", the previous nine-year high.")
  )

# bar chart for nonresident states ####

import data
nres <- read_csv("exported_data/nonresident_ab_19_22.csv")

# set colors 

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



# on reordering variables within facets  https://juliasilge.com/blog/reorder-within/
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
# adjust to add locator map of states

# using tigris package

# read in provider location data   
prov <- read_csv("Exported_Data/prov_ll.csv") %>% 
  filter(year %in% 2019:2021)

# import states geometry  ####
states <- states(cb=T)

states <- states %>% filter(NAME == "Illinois" | NAME == "Michigan" | NAME == "Kentucky" |
                              NAME == "Tennessee" | NAME == "Indiana" | NAME == "Ohio")

# add colors to match bar chart #### 

states$color[states$NAME == "Kentucky"] <- ky_c 
states$color[states$NAME == "Ohio"] <- oh_c
states$color[states$NAME == "Tennessee"] <- tn_c
states$color[states$NAME == "Illinois" ] <- "gray"
states$color[states$NAME == "Michigan"] <- "gray"


ggplot()+
  geom_sf(data = states,
          fill = states$color)+
  
  theme_void()+
  geom_point(data=prov %>% distinct(facility, .keep_all = T),
             # filter(!str_detect(facility, "Hospital") == TRUE,
             #        county == "Marion"),
             aes(x=lon, y=lat,
                 size = 2),
             color = "darkgreen",
             fill  = "darkgreen",
             shape = 21,
             alpha = .3,
             show.legend = F)+
  t_theme()+
  theme(
    axis.text = element_blank()
  )
#### # combo bar chart inset map

# bar chart:
pt <- nres %>% group_by(state) %>% arrange(desc(abortions)) %>% ungroup() %>% 
  # mutate(
  #   year = as.factor(year),
  #   state2 = tidytext::reorder_within(state, by = rev(abortions), within = year)) %>%
  # added state2 as additional column to retain ability to scale_fill_manual via state column
  ggplot(aes(year, abortions, fill = state))+
  geom_col(show.legend = F)+
  scale_fill_manual(values = s_cols)+
  facet_grid(~state, scales="free_y", switch = "x")+
  #coord_flip()+
  #tidytext::scale_x_reordered()+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    
    plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                           lineheight = 1,margin = margin(.25, 0, .4, 0, unit = "cm"),),
  )

# map
p1 <- ggplot()+
  geom_sf(data = states,
          fill = states$color)+
  theme(
    axis.text = element_blank(),
  )+
  t_theme()

# inset text 

alt_label <- "<span style = 'color:#fc8d59;'> Kentucky</span> and
      <span style = 'color:#82b6d6;'>Tennessee</span> enacted near-total abortion
      bans after the Dobbs decision in 2022. Like Indiana, <span style = 'color:#376bae;'>Ohio's
      </span>ban is currently blocked while litigation proceeds. Illinois and Michigan
      retain abortion rights."

# inset text object
text1 <- ggplot(data = tibble(x = 0, y = 1, label = alt_label)) +
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
                   left = 0.6, 
                   bottom = 0.3, 
                   right = .95, 
                   top = 1,
                   align_to = "plot")+
  inset_element(text1, 
                left = 0.31, 
                bottom = 0.45, 
                right = .7, 
                top = .80,
                align_to = "plot")+
  
  plot_annotation(
    theme = t_theme(),
    title =
      "Indiana began reporting non-resident abortion totals for individual
      states starting in 2019.<span style ='color:#fc8d59;'> Kentucky</span>
      residents received the most while <span
      style ='color:#82b6d6;'>Tennessee</span>, which had the second-highest totals
      in 2019, was the lowest in 2021.")