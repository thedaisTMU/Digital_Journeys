library(BFTheme)
library(ggplot2)
library(data.table)

#########################################################
#Load in graph spread sheets
graph.data <- fread("Graphs/Graph_spreadsheet.csv")



#################################
#Figure 1 - most complex

library(maptools)
library(BFTheme)
library(ggplot2)
library(data.table)
library(ggthemes)
library(showtext)


#Data on indigenous land
indigenous.land <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/Indigenous_land.kml")
indigenous.land <- lapply(indigenous.land,as.data.table)
indigenous.land.dt <- rbindlist(indigenous.land,idcol=TRUE)
names(indigenous.land.dt) <- c("id","long","lat","height")
#Data on high speed coverage 75100%
high.speed75100 <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/5010coverage_75100.kml")
high.speed75100 <- lapply(high.speed75100,as.data.table)
high.speed75100.dt <- rbindlist(high.speed75100,idcol=TRUE)
names(high.speed75100.dt) <- c("id","long","lat","height")
#Data on high speed coverage 5075%
high.speed5075 <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/5010Coverage_5075.kml")
high.speed5075 <- lapply(high.speed5075,as.data.table)
high.speed5075.dt <- rbindlist(high.speed5075,idcol=TRUE)
names(high.speed5075.dt) <- c("id","long","lat","height")
#Data on high speed coverage 2550%
high.speed2550 <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/5010_Coverage2550.kml")
high.speed2550 <- lapply(high.speed2550,as.data.table)
high.speed2550.dt <- rbindlist(high.speed2550,idcol=TRUE)
names(high.speed2550.dt) <- c("id","long","lat","height")

#Data on high speed coverage 025%
high.speed025 <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/5010_Coverage025.kml")
high.speed025 <- lapply(high.speed025,as.data.table)
high.speed025.dt <- rbindlist(high.speed025,idcol=TRUE)
names(high.speed025.dt) <- c("id","long","lat","height")

#Data on low speed coverage 75100%
low.speed75100 <- getKMLcoordinates(kmlfile="Graphs/Graphs_Data/Figure_1/51_Coverage75100.kml")
low.speed75100 <- lapply(low.speed75100,as.data.table)
low.speed75100.dt <- rbindlist(low.speed75100,idcol=TRUE)
names(low.speed75100.dt) <- c("id","long","lat","height")

########Actual plot

figure.1 <- ggplot(data=indigenous.land.dt) + #Set up base plot - only choose shapes within province chosen
  brookfield.base.theme() + #Base theme
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_polygon(data=areas.erDF,aes(x=long,y=lat,group=group),fill=NA,colour=set.colours(1,categorical.choice="grey"),size=0.03) +
  geom_polygon(data=indigenous.land.dt,aes(x=long,
                                           y=lat,
                                           group=id,fill="Aboriginal land in Canada"),colour=NA,alpha=0.6) +
  geom_polygon(data=high.speed025.dt,aes(x=long,
                                         y=lat,
                                         group=id,fill="0-25% Coverage of High speed internet"),colour=NA,alpha=0.4) +
  geom_polygon(data=high.speed2550.dt,aes(x=long,
                                          y=lat,
                                          group=id,fill="25-50% Coverage of High speed internet"),colour=NA,alpha=0.4) + 
  geom_polygon(data=high.speed5075.dt,aes(x=long,
                                          y=lat,
                                          group=id,fill="50-75% Coverage of High speed internet"),colour=NA,alpha=0.4) + 
  geom_polygon(data=high.speed75100.dt,aes(x=long,
                                           y=lat,
                                           group=id,fill="75-100% Coverage of High speed internet"),colour=NA,alpha=0.4) +
  
  
  coord_map("lambert",parameters=c(49,77),expand=FALSE) +
  scale_fill_manual(breaks = c("Aboriginal land in Canada","75-100% Coverage of High speed internet",
                               "50-75% Coverage of High speed internet",
                               "25-50% Coverage of High speed internet",
                               "0-25% Coverage of High speed internet"),
                    values = c("Aboriginal land in Canada" = "#eb0072",
                               "75-100% Coverage of High speed internet" = "#002d72",
                               "50-75% Coverage of High speed internet" = "#ffa300",
                               "25-50% Coverage of High speed internet" = "#00a9ef",
                               "0-25% Coverage of High speed internet" = "#6bbfae")) +
  labs(title = "Figure 1",subtitle = graph.data[Figure_number=="Figure 1",Figure_title], caption = graph.data[Figure_number=="Figure 1",Caption],
       fill = "Legend") +
  guides(fill=guide_legend(ncol=3))








#################################
#Figure 2 onwards


figure_2_data <- fread("Graphs/Graphs_Data/Figure_2.csv")
figure.2 <- plot.line.bf(figure_2_data,"date", "ratio", group.by = "indg2",
                         colours = c("#eb0072", "#000000"),
                         plot.fig.num = "Figure 2",
                         plot.title= graph.data[Figure_number=="Figure 2",Figure_title],
                         x.axis= graph.data[Figure_number=="Figure 2",X_Axis],
                         y.axis= graph.data[Figure_number=="Figure 2",Y_Axis],
                         caption = graph.data[Figure_number=="Figure 2",Caption],
                         unit.y = graph.data[Figure_number=="Figure 2",Y_Axis_Ticks],
                         smooth = TRUE,
                         export = TRUE,
                         export.name = "F2")


figure_3_data <- fread("Graphs/Graphs_Data/Figure_3.csv")
figure.3 <- plot.line.bf(figure_3_data, "date", "index", group.by = "indg2",
                         colours = c("#eb0072", "#000000"),
                         plot.fig.num = "Figure 3",
                         plot.title= graph.data[Figure_number=="Figure 3",Figure_title],
                         x.axis= graph.data[Figure_number=="Figure 3",X_Axis],
                         y.axis= graph.data[Figure_number=="Figure 3",Y_Axis],
                         caption = graph.data[Figure_number=="Figure 3",Caption],
                         smooth = TRUE,
                         export = TRUE,
                         export.name = "F3")

figure_4_data <- fread("Graphs/Graphs_Data/Figure_4.csv")
figure.4 <- plot.column.bf(figure_4_data,"total11ratio","sex2",
                           colours = c("#eb0072","#000000"),
                           plot.fig.num = "Figure 4",
                           caption = graph.data[Figure_number=="Figure 4",Caption],
                           plot.title = graph.data[Figure_number=="Figure 4",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 4",Y_Axis_Ticks],
                           export = TRUE,
                           export.name = "F4")

figure_5_data <- fread("Graphs/Graphs_Data/Figure_5.csv")
figure.5 <- plot.column.bf(figure_5_data,"edusexratio","sex2", group.by = "edu2",
                           y.axis = graph.data[Figure_number=="Figure 5",Y_Axis],
                           colours = c( "#002d72","#5bc2f4","#EB0C72","#ffa300","#DDA5C0"),
                           plot.fig.num = "Figure 5",
                           caption = graph.data[Figure_number=="Figure 5",Caption],
                           plot.title = graph.data[Figure_number=="Figure 5",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 5",Y_Axis_Ticks],
                           stacked = TRUE,
                           order.bar = "ascending",
                           export = TRUE,
                           export.name = "F5")

figure_6_data <- fread("Graphs/Graphs_Data/Figure_6.csv")
figure.6 <- plot.column.bf(figure_6_data,"incsexratio","income2",group.by = "sex2",
                           colours = c("#eb0072","#000000"),
                           plot.fig.num = "Figure 6",
                           y.axis = graph.data[Figure_number=="Figure 6",Y_Axis],
                           caption = graph.data[Figure_number=="Figure 6",Caption],
                           plot.title = graph.data[Figure_number=="Figure 6",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 6",Y_Axis_Ticks],
                           order.bar= "ascending",
                           export = TRUE,
                           export.name = "F6")

figure_7_data <- fread("Graphs/Graphs_Data/Figure_7.csv")
figure.7 <- plot.column.bf(figure_7_data, "share", "indg2",
                           colours = c("#eb0072", "#000000"),
                           plot.fig.num = "Figure 7",
                           y.axis = graph.data[Figure_number=="Figure 7",Y_Axis],
                           caption = graph.data[Figure_number=="Figure 7",Caption],
                           plot.title = graph.data[Figure_number=="Figure 7",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 7",Y_Axis_Ticks],
                           export = TRUE,
                           export.name = "F7")

figure_8_data <- fread("Graphs/Graphs_Data/Figure_8.csv")
figure.8 <- plot.column.bf(figure_8_data,  "sexratio","gender2",
                           colours=c("#000000","#eb0072"),
                           plot.fig.num = "Figure 8",
                           y.axis = graph.data[Figure_number=="Figure 8",Y_Axis],
                           caption = graph.data[Figure_number=="Figure 8",Caption],
                           plot.title = graph.data[Figure_number=="Figure 8",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 8",Y_Axis_Ticks],
                           export = TRUE,
                           export.name = "F8")

figure_9_data <- fread("Graphs/Graphs_Data/Figure_9.csv")
figure.9 <- plot.column.bf(figure_9_data,"incratio2","labels_quintinc", group.by = "indg2",
                           stacked = FALSE,
                           colours = c( "#eb0072", "#000000"),
                           plot.fig.num = "Figure 9",
                           y.axis = graph.data[Figure_number=="Figure 9",Y_Axis],
                           caption = graph.data[Figure_number=="Figure 9",Caption],
                           plot.title = graph.data[Figure_number=="Figure 9",Figure_title],
                           label.unit = graph.data[Figure_number=="Figure 9",Y_Axis_Ticks],
                           export = TRUE,
                           export.name = "F9")

figure_10_data <- fread("Graphs/Graphs_Data/Figure_10.csv")
figure.10 <- plot.column.bf(figure_10_data, "indgratio", "edu2",group.by="indg2", order= "ascending",
                            colours = c( "#eb0072", "#000000"),
                            plot.fig.num = "Figure 10",
                            y.axis = graph.data[Figure_number=="Figure 10",Y_Axis],
                            caption = graph.data[Figure_number=="Figure 10",Caption],
                            plot.title = graph.data[Figure_number=="Figure 10",Figure_title],
                            label.unit = graph.data[Figure_number=="Figure 10",Y_Axis_Ticks],
                            export = TRUE,
                            export.name = "F10")

figure_11_data <- fread("Graphs/Graphs_Data/Figure_11.csv")
figure.11 <- plot.line.bf(figure_11_data, "date2", "value", group.by= "skill",
                          plot.title = graph.data[Figure_number=="Figure 11",Figure_title],
                          caption = graph.data[Figure_number=="Figure 11",Caption],
                          plot.fig.num = "Figure 11",
                          colours = c("#002d72","#5bc2f4","#EB0C72","#ffa300","#DDA5C0","#792082","#009a44"),
                          export = TRUE,
                          export.name = "F11")



figure_12_data <- fread("Graphs/Graphs_Data/Figure_12.csv")
figure.12 <- plot.column.bf(figure_12_data, "occtotal","Occupation",
                            plot.title = graph.data[Figure_number=="Figure 12",Figure_title],
                            colours = c("#EB0C72"),
                            caption = graph.data[Figure_number=="Figure 12",Caption],
                            order.bar = "ascending",
                            export = TRUE,
                            export.name = "F12")





