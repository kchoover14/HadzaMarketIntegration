library(tidyverse); library(psych); library(car)
library(ggplot2); library(ggpubr); library(gridBase); library(grid)

#read final data
hadza <- read.csv("3-data-hadza-final.csv", stringsAsFactors = TRUE)

#descriptives by sample
descriptives.sample <- describe(hadza, na.rm = TRUE)
write.csv(descriptives.sample, "results-descriptives.sample.csv", quote=FALSE, row.names = FALSE, na="")

#descriptives by sex
descriptives.sex <- describeBy(hadza, group="Sex", na.rm = TRUE, mat=TRUE)
write.csv(descriptives.sex, "results-descriptives.sex.csv", quote=FALSE, row.names = FALSE, na="")

#descriptives by cohort
descriptives.cohort <- describeBy(hadza, group="BirthCohort", na.rm = TRUE, mat=TRUE)
write.csv(descriptives.cohort, "results-descriptives.cohort.csv", quote=FALSE, row.names = FALSE, na="")

#descriptives by sex and cohort
descriptives.sexcohort <- describeBy(hadza, group= c("Sex", "BirthCohort"), na.rm = TRUE, mat=TRUE)
write.csv(descriptives.sexcohort, "results-descriptives.sexcohort.csv", quote=FALSE, row.names = FALSE, na="")

#tidy
rm(descriptives.cohort, descriptives.sample, descriptives.sex, descriptives.sexcohort)

#Data Distribution Plot
p <- ggplot(hadza, aes(hypoplasia.frequency, BirthCohort, color = Sex, shape=Sex)) +
    geom_jitter(width = .3, height = .3) +
    labs(title = "", x="Hypoplasia Frequency", y="Before 1975 or 1975+") +
    theme_classic2() +
    theme(legend.position = "top", panel.border = element_rect(color = "black",fill = NA,size = 1))+
    theme(legend.title = element_blank(),
          legend.text = element_text(color="black", size=12),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.y=element_blank())+
    scale_colour_viridis_d(begin= 0.15, end=0.65)


#Panel plot
windows(width=5, height=2.5, xpinch = 300, ypinch = 300)
par(mfrow=c(1,2))
qqp(hadza$hypoplasia.frequency, "norm", grid=FALSE, ylab=paste("Hypoplasia Frequency"),
    xlab=paste("Normal Distribution Quantiles"), envelope=c(style="lines"), cex=1.5,
    pch=16, col= "darkslateblue", col.lines="black", id=FALSE)
plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <-plotViewport(c(0,0,0,0))
print(p,vp = vp1)

rm(hadza)
