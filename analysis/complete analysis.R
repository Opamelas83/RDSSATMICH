library(readr); library(tidyverse); library(ggplot2); library(dplyr); library(lemon); library(gridExtra); library(gridtext)
#Combined_Data_file_Day_7_14_ <- read_csv("Combined Data file_Day_7_14_.csv")
METABOLITES_7D_up <- read_csv("METABOLITES_7D_up.csv")
METABOLITES_7D_down <- read_csv("METABOLITES_7D_down.csv")
METABOLITES_14D_up <- read_csv("METABOLITES_14D_up.csv")
METABOLITES_14D_down <- read_csv("METABOLITES_14D_down.csv")
METABOLITES_7D_up <- METABOLITES_7D_up %>% mutate(Position = "7D_up")
METABOLITES_7D_down <- METABOLITES_7D_down %>% mutate(Position = "7D_down")
METABOLITES_14D_up <- METABOLITES_14D_up %>% mutate(Position = "14D_up")
METABOLITES_14D_down <- METABOLITES_14D_down %>% mutate(Position = "14D_down")
#METABOLITES_14D_down <- METABOLITES_14D_down %>% mutate(Position = "14D_down")
#METABOLITES_14D <- bind_rows(METABOLITES_14D_up, METABOLITES_14D_down)
METABOLITES <- bind_rows(METABOLITES_7D_up, METABOLITES_7D_down, METABOLITES_14D_up, METABOLITES_14D_down)
write_csv(METABOLITES, "METABOLITES_sum.csv", append = FALSE)
#Uniformise the names of `Super class` on the csv file
METABOLITES <- read_csv("METABOLITES_sum.csv")
#add total number of metabolite
#prepare data fo pie chart
# "CBE5"  "TVu87" "MSS"   "TVu28" Cultivar list
Piedon7DdownCBE5 <- METABOLITES %>% #METABOLITES_14D_down
  group_by(Cultivar, Position, `Super class`) %>% #Position,
  summarise(N_metabolite = length(`Metabolite name`)) %>%
  mutate(Frequence = round(N_metabolite/sum(N_metabolite)*100, digits =1)) %>%
        # ypos = cumsum(Frequence)- 0.5*Frequence) %>%
  filter(Position %in% "7D_down") %>%
  filter(Cultivar %in% "CBE5")

#To modify the pie donut name
#mutate(MSS_T14 = case_when(Position == "14D_up" ~ "upregulated",
 #                        Position == "14D_down" ~ "downregulated"),
  #     SuperClass = `Super class`)
write_csv(Piedon14D, "Piedon14D.csv", append = FALSE)
#It seems like I don't really need the "Frequence" column unless you want to modify thepie name
#library(webr)
#labs <- paste0(Piedon14D$Type, " (", Piedon14D$Frequence, "%)")
#jpeg(filename = "CBE5_T14.jpeg")
#PieDonut(Piedon14D, aes(MSS_T14, SuperClass, count=N_metabolite),
 #        pieLabelSize = 0,
         #r=0.2,r1=1,r2=1.3,
  #       donutLabelSize = 0,
         #labelposition=0.01,
   #      showPieName = FALSE,
    #     showDonutName = FALSE)#r0 = 0.3, r1 = 1

#dev.off()

# Make the plot
## 7D_up
sevDup <- ggplot(Piedon7Dup, aes(x="",  y=Frequence, fill = `Super class`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = paste0(Frequence,"%"))) + #size=5
  scale_fill_brewer(palette="Set1") +
    ggtitle("7D_up")
## 7D_down

sevDdown <- ggplot(Piedon7Ddown, aes(x="",  y=Frequence, fill = `Super class`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = paste0(Frequence,"%"))) + #size=5
  scale_fill_brewer(palette="Set1") +
  ggtitle("7D_down")

## 14D_up
forDup <- ggplot(Piedon14Dup, aes(x="",  y=Frequence, fill = `Super class`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = paste0(Frequence,"%"))) + #size=5
  scale_fill_brewer(palette="Set1") +
  ggtitle("14D_up")

## 14D_down
forDdown <- ggplot(Piedon14Ddown, aes(x="",  y=Frequence, fill = `Super class`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste0(Frequence,"%"))) + #size=5
  scale_fill_brewer(palette="Set1") +
  ggtitle("14D_down")

FforDdown <- plot_ly(labels = ~`Super class`, values = ~Frequence) %>%
  add_pie(data = Piedon7DdownMSS, name = "DF3", domain = list(row = 0, column = 0)) %>%
  add_pie(data = Piedon7DdownCBE5, name = "DF4", domain = list(row = 0, column = 1)) %>%
  add_pie(data = Piedon7DdownTVu28, name = "DF1", domain = list(row = 1, column = 0)) %>%
  add_pie(data = Piedon7DdownTVu87, name = "DF2", domain = list(row = 1, column = 1)) %>%
  layout(title = "7D_down", showlegend = T,
         grid=list(rows=2, columns=2),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(title=list(text="Super class"), y = 0.5),
         annotations = list(x = c(.08, .62, .08, .62),
                            y = c(.98, .98, .45, .45),
                            text = c("MSS","CBE5","TVu28","TVu87"),
                            xref = "papper",
                            yref = "papper",
                            showarrow = F
         )
  )

UNO <- FforDdown %>% layout(title = '14D_down')#,
                #legend=list(title=list(text='<b> Super class </b>')))

FforDup <- plot_ly(Piedon14Dup, labels = ~`Super class`, values = ~Frequence, legendgroup = ~`Super class`)
DOS <- FforDup %>% layout(title = '14D_up')#,
                           # legend=list(title=list(text='<b> Super class </b>')))

FsevDdown <- plot_ly(Piedon7Ddown, labels = ~`Super class`, values = ~Frequence, type = "pie", legendgroup = ~`Super class`,
                     showlegend = F)
TRES <- FsevDdown %>% layout(title = '7D_down')#,
                            #legend=list(title=list(text='<b> Super class </b>')))
FsevDup <- plot_ly(Piedon7Dup, labels = ~`Super class`, values = ~Frequence, type = "pie", legendgroup = ~`Super class`,
                   showlegend = F)
QUATRO <- FforDup %>% layout(title = '7D_up', xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F))#,
                            #legend=list(title=list(text='<b> Super class </b>')))

                          rr <-  UNO %>% add_pie(DOS)
fig <- plt.subplots(UNO, DOS, TRES, QUATRO)



# xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      #yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
 fig %>% layout(legend=list(title=list(text='<b> Super class </b>')))

nt <- theme(legend.position='none')
  p = list(sevDup+nt, sevDdown+nt, forDup+nt, forDdown+nt) %>% map(~.x + labs(x=NULL, y=NULL))
  #jpeg(filename = "Result/Rcomparison.jpeg")
  grid_arrange_shared_legend(UNO, DOS, TRES, QUATRO, ncol = 2, nrow = 2)
