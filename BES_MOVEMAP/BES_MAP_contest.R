#Script for plotting the autumn migration routes of the Finnish population of the European honey buzzard
#Elham Nourani, PhD. enourani@ab.mpg.de
#Jan. 13. 2021.

#load libraries
library(sp)
library(lubridate)
library(scales) 
library(TeachingDemos) 
library(png)

setwd("BES_MOVEMAP")

#------------- STEP 1: Read in the compnents of the plot ----

#flyway map (spatial polygon called flyway)
load("flyway_layer.RData")

#migratory trajectories (list of lines called Lines_ls)
load("tracks_ls.RData")

#migratory track data (data frame called tracks_df)
load("tracks_df.RData")

#separate the data for adults and juveniles
adlt <- tracks_df[tracks_df$age == "adult", ]
juv <- tracks_df[tracks_df$age != "adult", ]

# twitter logo
twtr <- readPNG("twitter-logo.png")

#------------- STEP 2: Write a function for the subplot ----

ind_var_plot <- function(x){
    plot(0, xlim = c(228, 270), ylim = c(-6,40), labels = F, tck = 0, ann = F)
    points(first_sea_long ~ first_obs_yday, data = tracks_df[tracks_df$age == "adult",], col = alpha("paleturquoise3",0.7), 
           pch = 20, cex = 2)
    points(first_sea_long ~ first_obs_yday, data = tracks_df[tracks_df$age != "adult",], col = alpha("rosybrown2",0.7), 
           pch = 20, cex = 2)
    abline(h = -5.6, lty = 2, lwd = 0.7, col = "grey20")
    text(x = 263.7, y = -4, "Gibraltar", col = "grey20", cex = 0.7, font = 3)
    abline(h = 36, lty = 2, lwd = 0.7, col = "grey20")
    text(x = 262, y = 37.8, "Iskenderun", col = "grey20", cex = 0.7, font = 3)
    axis(side = 1, at = c(230, 240, 250, 260,270), line = 0, labels = c(230, 240,250, 260,270), 
         tick = T , col.ticks = 1, col = NA, lty = NULL, tck = -.015)
    axis(side = 2, at = c(0,10,20, 30, 40), line = 0, labels = c(0,10,20, 30, 40),
         tick = T , col.ticks = 1, col = NA, lty = NULL, tck = -.015, 
         las = 2)
    
    mtext("Longitude at sea", 2, line = 1.2 ,las = 0, cex = 1.4, font = 3)
    mtext("Migration onset (Day of year)", 1, line = 1.2, cex = 1.4, font = 3)
    mtext("Individual variation in", 3, line = 1.1, cex = 1.5, font = 4)
    mtext("routes and timing", 3, line = 0.2, cex = 1.5, font = 4)
}


#------------- Put it all together ----

pdf("EHB_map.pdf", height = 15, width = 12)

par(mfrow=c(1,1), bty="n", 
    cex.axis= 0.75, 
    font.axis= 3, 
    mar= c(0,0,0,0), 
    oma= c(0,0,1,0)
)

#Plot the flyway as background
plot(flyway, col="grey30", border= F, ylim = c(-20,68)) 

#add latitude lines
lines(x = c(-20,45.9), y = c(0,0),lty = 2,lwd = 1, col = "grey50")
lines(x = c(-20,45.9), y = c(30,30),lty = 2,lwd = 1, col = "grey50")
lines(x = c(-20,45.9), y = c(60,60),lty = 2,lwd = 1, col = "grey50")
text(x = -17, y = 30.7, "30° N", col = "grey50", cex = 1.1, font = 3)
text(x = -17, y = 60.7, "60° N", col = "grey50", cex = 1.1, font = 3)

#add trajectories
lapply(Lines_ls[names(Lines_ls) %in% adlt$track],lines,lty= 1, lwd =  2, col="#88CCEE")
lapply(Lines_ls[names(Lines_ls) %in% juv$track],lines,lty= 1,lwd = 2, col = "#FFC20A")

#add a box for subplot
rect(xleft = 16,
     xright = 45.4,
     ybottom = -19.5,
     ytop = 13.2,
     col = alpha("white", 0.9),
     border = NA)

#add the subplot
subplot(ind_var_plot(), x = 33, y = -3, size = c(3.1,3.4),  #-23 was -19
        pars = list(mar=c(0,0,0.6,0),cex = 1.8, bty = "l", mgp = c(0,0.3,0),tck = 0.015))

#add title
text(x = 12, y = 69, "Autumn Migration of the European Honey Buzzard", cex = 2, font = 4)

#add legend
legend(x = -21, y = 5, legend=c("Juveniles (n = 22)","Adults (n = 31)"), col=c("#FFC20A","#88CCEE"), #coords indicate top-left
       pch = 20, cex = 1.3, bg = "white", bty = "n")

#add plot description
textbox(x = c(-20,8), y = -0.8, c("European honey buzzards (Pernis apivorus) breed in northern Europe and winter in sub-Saharan Africa.", 
                                  "This map shows the GPS tracking data for autumn migration of first-year juveniles (yellow) and adults of unknown age (blue).",
                                  "There is pronounced age-specific differences, both in the migratory routes and migration onset (subplot).",
                                  "For example, some adults migrate through bottlenecks (e.g. the Strait of Gibraltar and Iskenderun Bay) or islands to minimize sea-crossing,",
                                  "while juveniles cross the sea whenever they encounter it.", 
                                  "To learn more, see Nourani et al. 2020 (Biol Lett) and Vansteelant et. al. 2017 (Proc B)."), 
        justify = "l", box = F, cex = 1.041, font = 3)

#add twitter details
text(x = -16.5, y = -18.5, "@elham_nourani", cex = 1.2, font = 3, adj = 0)
rasterImage(twtr, xleft = -20, xright = -17, ybottom = -19.5, ytop = -17) 

dev.off()
