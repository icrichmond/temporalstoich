# Author: Isabella Richmond
# Last edited: April 2, 2020
# Source codes: Stoich_TemporalModels_2step.R and Stoich_GzLMTemporalModels_2step.R 

# This code was for the visualization of our data and temporal stoichiometry models.
# A large portion of boxplot code provided by Juliana Balluffi-Fry and Matteo Rizzuto
# Run codes in source scripts to load in the data and models 
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, site, moisture, weather)

### Visualize the models ###
# going to make boxplots for the 
# manually setting up colours 
cols<-c("2016"= rgb(26,153,136, maxColorValue = 255), "2017"= rgb(235,86,0, maxColorValue = 255))

abba.c.box <- ggplot(data=ABBA, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  geom_jitter(cex=1.5, col="#989898")+
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Balsam Fir")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

acru.c.box<- ggplot(data=ACRU, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  geom_jitter(cex=1.5, col="#989898")+
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Red Maple")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

bepa.c.box<- ggplot(data=BEPA, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  geom_jitter(cex=1.5, col="#989898")+
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("White Birch")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

vaan.c.box<- ggplot(data=VAAN, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  geom_jitter(cex=1.5, col="#989898")+
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Lowland Blueberry")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
png("graphics/StoichModels_2Step/Boxplots/RawCbox.png")
(abba.c.box | acru.c.box) /
  (bepa.c.box | vaan.c.box)
dev.off()