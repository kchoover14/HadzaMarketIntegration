library(ggplot2); library(ggpubr); library(cowplot)
library(car)

#read file
hadza= read.csv("3-data-hadza-final.csv", stringsAsFactors = TRUE)

#plots
p1 <- ggplot(hadza, aes(x = Sex, y = hypoplasia.frequency,fill=Sex))+
geom_boxplot(show.legend=FALSE)+
  theme(axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"))+
    stat_compare_means(method = "t.test", label.x = 1.25, label.y = 1.5, size=4)+
  ylab("Hypoplasia Frequency")+
  xlab("")+
  scale_color_viridis_d(begin= 0.15, end=0.65)+
  scale_fill_viridis_d(begin= 0.15, end=0.65)+
  theme_classic2()

p2=ggplot(hadza, aes(x = yob, y = hypoplasia.frequency,fill= Sex, colour=Sex, shape=Sex)) +
    stat_smooth(method = "lm") +
    geom_point(size=2) +
    xlab("Year of Birth") + ylab("LEH Frequency") +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             method = "pearson",
             p.accuracy = 0.001, r.accuracy = 0.01,
             label.x = c(1930,1930), label.y = c(3, 3.2), size = 4) +
    stat_regline_equation(label.x = c(1950,1950), label.y = c(3, 3.2), size = 4) +
    scale_x_continuous(breaks=seq(1925,2000,5))+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(color="black", size=12),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.y=element_blank())+
    scale_color_viridis_d(begin= 0.15, end=0.65)+
    scale_fill_viridis_d(begin= 0.15, end=0.65) +
    theme_classic2()


tiff("f2-LEH frequency.tiff", width = 12, height = 4, units = "in", res = 300, type="cairo")
plot_grid(p1, p2, nrow = 1, labels = c("A", "B"),rel_widths = c(1, 2))
dev.off()
