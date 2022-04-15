#INSTALL LIBRARIES (not needed if already installed)----
install.packages("tidyverse")
install.packages("tidytext")
install.packages("sysfonts")
install.packages("showtext")
install.packages("viridis")
install.packages("ggridges")

#IMPORT LIBRARIES ----
library(tidyverse) 
library(tidytext)
library(sysfonts)
library(showtext)
library(viridis)
library(ggridges)

#IMPORT & PREVIEW DATA ----
#Data about Olympic Athletes from TidyTuesday - with read_csv you can read files from your local directory or from online by specifying the file path or url
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

#function View with dataframe name or we can click on dataframe in side console to preview
View(df)

#SET UP  & LOAD FONTS ----
font_add_google("Chivo", "chivo")
showtext_auto()

#INTRO TO DPLYR- Transforming Data ----

#dplyr cheatsheet pdf: https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf
#filtering rows - use dplyr function, filter()
df%>%filter(sport=="Swimming")

#filtering with multiple arguments (use & for and), including variable in a list e.g. medals is either Gold, Silver, or Bronze
df%>%filter(sport=="Swimming" & year>1948 & medal %in% c('Gold','Silver','Bronze'))

#adding new rows with dplyr mutate()
df%>%mutate(athlete_dob = year - age)

#aggregate data with dplyr group_by() and summarise(). many different agg functions with summarise()!
df%>%group_by(sex)%>%summarise(athletes = n_distinct(id), median_age = median(age, na.rm=TRUE))

#putting it all together with %>%
df%>%
  filter(sport=="Swimming")%>%
  mutate(athlete_dob = year-age)%>%
  group_by(athlete_dob)%>%
  summarise(athletes= n_distinct(id))%>%
  arrange(-athletes)


#PLOT 1 Bar Plot Olympic Swimming ----
#What are the top teams overall for each swimming style based on # of medals?
swimming <- df%>%
  filter(sport == 'Swimming' & year>=1948 & medal %in% c('Gold','Silver','Bronze') & !grepl("Open Water",event))%>%
  mutate(
    #Swimming Event contains "Swimming Men's" and "Swimming Women's", strip this part of text with stringr str_replace
    event = str_replace(event, "Swimming Men's |Swimming Women's ", ""), 
    #R has a SQL like function, case_when - let's use this to set "East Germany" and "West Germany" as "Germany"
    team = case_when(team %in% c('East Germany','West Germany') ~ 'Germany', TRUE ~ team)
    )%>%
  #split column "event" into two new columns - metres and style - based on delimiter. e.g. "400 metres Freestyle"
  separate(event, into = c("metres","style"), sep = " metres ")%>%
  #aggregate medal count by team and style
  group_by(team, style)%>%
  summarise(medals = n())%>%
  arrange(-medals)

#preview data
swimming

#narrow it down to top teams by style
swimming_top<-swimming%>%
  #remove swimming events that are relays with grepl (exclude anything that contains "Relay)
  filter(!grepl("Relay",style))%>%
  #filter to get top teams by swimming style using group_by & slice_max
  group_by(style)%>%
  slice_max(order_by=medals, n=8)

#start off small - let's look at Freestyle data only for our first plot
freestyle_top<-swimming_top%>%filter(style=="Freestyle")


#our first ggplot 
ggplot(data=freestyle_top, aes(x=medals,y=team))+
  geom_bar(stat="identity")

#sorting bars with reorder(), layering with new geom to add text

bar<-ggplot(data=freestyle_top, aes(y=reorder(team,medals), x=medals))+
  geom_bar(stat="identity", fill="#4a7fb0")+
  geom_text(aes(label=medals), color="white", hjust=1.5, size=3)

bar

#adding our labels with labs()

bar+
  labs(title="Freestyle Swimming",
       subtitle="Something about freestyle swimming",
       y = "Team", x="Medals")

bar_2<-bar+
  labs(title="Olympic Freestyle Swimming - Top Teams",
       subtitle="Data from 1948 to 2016",
       x="Medals",
       y="Team",
       caption="Data from www.sports.reference.com | Graphic by Tanya Shapiro")

bar_2

#adding a custom theme with theme()
bar_3<-bar_2+
  theme(text = element_text(family="chivo"),
        plot.title=element_text(face="bold"))

bar_3

#Plotting Multiple Bar Charts in One - facet_wrap()
ggplot(data=swimming_top)+
  geom_bar(aes(y=team, x=medals, fill=style), stat="identity")+
  geom_text(aes(y=team, x=medals, label=medals), hjust=1.4, size=3, color="white")+
  facet_wrap(~style, scales = "free_y")

#Reordering The Data for Facets - use tidytext reorder_within(). great blog post by Julia Silge: https://juliasilge.com/blog/reorder-within/
swimming_ordered<-swimming_top%>%
  mutate(team = reorder_within(team, medals, style))

#Facets Ordered Data
bar_facet<-ggplot(data=swimming_ordered)+
  geom_bar(aes(y=team, x=medals, fill=style), stat="identity")+
  geom_text(aes(y=team, x=medals, label=medals), hjust=1.4, size=3, color="white")+
  facet_wrap(~toupper(style), scales="free")+
  scale_y_reordered()

bar_facet

#Customizing Facets - setting colors, adding labels, and theme
bar_facet_2<-bar_facet+
  scale_fill_manual(values = c("#f46036","#E6AC00","#5b85aa","#414770","#372248"))+
  labs(title="OLYMPIC SWIMMERS - TEAMS WITH MOST MEDALS (1952-2016)", 
       subtitle= "By Swimming Style. Does not include Relays.",
       caption = "Data from www.sports-reference.com | Graphic by insert_name_here",
       y = "", x="Total Medals")+
  theme(text = element_text(family="chivo"),
        plot.title=element_text(face="bold"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey20"),
        strip.text = element_text(color="white"),
        legend.position = "none")

bar_facet_2
#saving plot
ggsave(plot = bar_facet_2, filename = "top_swimming.jpeg", height=6, width=10)

#PLOT 2 Distribution - Density Plots ----

#What does the weight distribution look like for athletes competing in different sports?

female_athletes<-df%>%
  filter(sex == "F" & season == "Summer" )%>%
  group_by(id, name, sport)%>%
  #cheat sheet for dplyr aggregation functions: link
  summarise(
            weight_kg = median(weight, na.rm=TRUE),
            height_cm = median(height, na.rm=TRUE)
  )%>%
  #we can use mutate to add metric conversions
  mutate(
    weight_lb = round(weight_kg * 2.20462,2),
    height_in = round(height_cm * 0.393701, 2)
  )

#subset data to look at specific sports - first create a list of specific sports
sports<-c("Basketball","Judo", "Weightlifting", "Volleyball","Rowing","Swimming","Diving","Gymnastics")

#filter data with dplyr
fem_ath_sub <- female_athletes%>%filter(sport %in% sports)

#first distribution plot with ggridges - extension of ggplot! more info: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
ggplot(data=fem_ath_sub, aes(y=sport, x= weight_lb, fill=stat(x)))+
  geom_density_ridges_gradient()

ggplot(data = fem_ath_sub, aes(y=sport, x=weight_lb, fill=stat(x)))+
  geom_density_ridges_gradient()

#adjust geom_density parameters, add lines for medians, reduce size to remove overlap
density <- ggplot(data = fem_ath_sub, aes(y=sport, x=weight_lb, fill=stat(x)))+
  geom_density_ridges_gradient(scale = 0.8, quantile_lines = TRUE, quantiles=2, show.legend = FALSE)

density

#what if we want to reorder y axis based on medians? answer: reshape our data

#get median per sport with dplyr aggregation
sport_median<-fem_ath_sub%>%
  group_by(sport)%>%
  summarise(median_weight = median(weight_lb, na.rm=TRUE))

sport_median

#append dataset to original dataset using dplyr join
fem_ath_ord<-fem_ath_sub%>%left_join(sport_median, by="sport")

fem_ath_ord

density_2 <- ggplot(data = fem_ath_ord, aes(y=reorder(sport,median_weight), x=weight_lb, fill=stat(x)))+
  geom_density_ridges_gradient(scale = 0.8, quantile_lines = TRUE, quantiles=2, show.legend = FALSE)

density_2

#adjusting x axis and color with scale_
density_3<-density_2+
  scale_x_continuous(limits = c(50,225))+
  scale_fill_viridis_c()

density_3

sum(df$weight, na.rm=TRUE)

#overlay overall median line with geom_vline()
density_4<-density_3+
  geom_vline(xintercept = median(fem_ath_ord$weight_lb, na.rm=TRUE), linetype="dashed")

density_4

#beautifying it with new labels and custom theme
density_5<-density_4+
  labs(title="Weight Distribution of Female Olympic Athletes",
       subtitle="Data for Summer Olympics only between 1986 and 2016",
       y="Sport",
       x="Weight (lbs)",
       caption = "Data from www.sports-refrence.com | Chart by [insert_your_name]"
  )+
  theme_minimal()+
  theme(text=element_text(family="chivo"),
        axis.title=element_text(face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x= element_blank(),
        plot.title=element_text(face="bold"))

density_5


#final step - adding annotaations and arrows ----

#add plot annotations with custom df for text & arrows. \n creates a line break for text

data.frame(
  first_name = c("Ted", "Roy"),
  last_name = c("Lasso", "Kent")
)

notes <- data.frame(
  x = c(102,78), 
  y = c(6.45, 2.5),
  text = c(paste("Overall Median \n", median(fem_ath_ord$weight_lb, na.rm=TRUE)),
           "Median \n per sport")
)

#for arrows we need start and stop point for both x and y coordinates. discrete y can be called as numeric (1, 2, etc)
arrows<-data.frame(
  xstart = c(90, 90, 118),
  xend = c(115, 130, 137),
  ystart = c(2.5, 2.55, 6.5),
  yend = c(2.4, 3.4,6.4)
)

#two ways we can annotate - geom_text or annotate() add curved arrows with geom_curve()
density_5+
  geom_text(data=notes, inherit.aes=FALSE, mapping=aes(x=x,y=y,label=text), size=3.5, family="chivo")+
  annotate("text", x= 200, y=4.5, size=3, family="chivo",
           label='Judo & Weightlifting have \n different weight classes')


density_6<-density_5+
  geom_text(data=notes, inherit.aes=FALSE, mapping=aes(x=x,y=y,label=text), size=3.5, family="chivo")+
  annotate("text", x = 200, y = 4.5, size=3, family="chivo", 
           label ='Judo & Weightlifting have \n different weight classes')+
  geom_curve(data = arrows, aes(x = xstart, y = ystart, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "black", curvature =-0.18)

density_6

#saving our plot
ggsave(plot=density_6, filename="weight_distribution.jpeg", width=10, height=9)
