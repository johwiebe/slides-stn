library(ggplot2)
library(tikzDevice)
library(tidyr)
library(scales)

instance = "biondi"
dir = "/home/johannes/homedir/STN/STN-Scheduler/results"
rdir = "/home/johannes/OneDrive/Presentations/esa"
setwd(paste0(dir, "/", instance))

icdarkgreen = rgb(102/255, 164/255, 10/255)
icgreen = rgb(206/255, 226/255, 177/255)
iclightgreen = rgb(225/255, 237/255, 206/266) 
icdarkblue = rgb(0, 52/255, 92/255)
icblue = rgb(173/255, 190/255, 203/255)
iclightblue = rgb(225/255, 229/255, 236/255)
mbg = rgb(250/255, 250/255, 250/255)
icdarkorange = rgb(210/255, 64/255, 0)

p_det_high = read.csv("det/high_pfail.csv")
p_det_avg = read.csv("det/avg_pfail.csv")
p_det_low = read.csv("det/low_pfail.csv")
p_det_high$scenario = "high"
p_det_avg$scenario = "avg"
p_det_low$scenario = "low"
p_det = rbind(p_det_avg, p_det_low, p_det_high)

p_mc_high = read.csv("mc/high_pfail.csv")
p_mc_avg = read.csv("mc/avg_pfail.csv")
p_mc_low = read.csv("mc/low_pfail.csv")
p_mc_high$scenario = "high"
p_mc_avg$scenario = "avg"
p_mc_low$scenario = "low"
p_mc = rbind(p_mc_avg, p_mc_low, p_mc_high)

res_det_high = read.csv("det/high_results.csv")
res_det_avg = read.csv("det/avg_results.csv")
res_det_low = read.csv("det/low_results.csv")
res_det_high$scenario = "high"
res_det_avg$scenario = "avg"
res_det_low$scenario = "low"
res_det = rbind(res_det_avg, res_det_high, res_det_low)


res_mc_high = read.csv("mc/high_mcre2_results.csv")
res_mc_avg = read.csv("mc/avg_mcre2_results.csv")
res_mc_low = read.csv("mc/low_mcre2_results.csv")
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc$bound = "MC"

res_mc_high = read.csv("mc/high_freq_results.csv")
res_mc_high$index = NULL
res_mc_avg = read.csv("mc/avg_freq_results.csv")
res_mc_low = read.csv("mc/low_freq_results.csv")
res_mc_low$index = NULL
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc2 = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc2$bound = "freq"
res_mc$index <- NULL

res_mc <- rbind(res_mc, res_mc2)


## PAPER ##
dfl <- gather(res_mc, Unit, p, c(Heater, Reactor_1, Reactor_2, Still))
dfl_det <- gather(res_det, Unit, p, c(Pheater, Preactor1, Preactor2, Pstill))
map <- setNames(c("Heater", "Reactor 1", "Reactor 2", "Still"), c("Pheater", "Preactor1", "Preactor2", "Pstill"))
dfl_det$Unit[] <- map[dfl_det$Unit]
map <- setNames(c("Heater", "Reactor 1", "Reactor 2", "Still"), c("Heater", "Reactor_1", "Reactor_2", "Still"))
dfl$Unit[] <- map[dfl$Unit]
# Apriori
eps <- c(Inf, read.csv("../eps.csv")$X0)
apriori <- read.csv("../apriori.csv")
apriori$Heater <- exp(-1/2*eps^2/(3*0.27^2))
apriori["Reactor 1"] <- exp(-1/2*eps^2/(9*0.27^2))
apriori["Reactor 2"] <- exp(-1/2*eps^2/(9*0.27^2))
apriori$Still <- exp(-1/2*eps^2/(3*0.27^2))
dfa <- gather(apriori, Unit, p, c("Heater", "Reactor 1", "Reactor 2", "Still"))

options(tikzDefaultEngine = 'luatex')
tikz(paste0(rdir, '/biondi-p-vs-alpha.tex'),width=4.27,height=3,pointsize=8)
a = 0.55
cols = hue_pal()(3)
gg <- ggplot(dfl, aes(alpha, p, color=scenario)) +
  theme_classic()+
  theme(text = element_text(color=icdarkblue)) +
  theme(plot.background = element_rect(fill=iclightblue)) +
  theme(panel.background = element_rect(fill=mbg)) +
  theme(axis.text = element_text(color=icdarkblue)) +
  guides(color = guide_legend(title.hjust = 1.0),
         linetype = guide_legend(title.hjust = 1.0)) +
  scale_color_manual(values=cols,
                    labels=c("{\\scriptsize avg\\,}", "{\\scriptsize high\\,}", "{\\scriptsize low\\,}")) +
  scale_linetype_manual(values=c(1, 2),
                     labels=c("{\\scriptsize freq\\,}", "{\\scriptsize MC\\qquad}")) +
  theme(legend.position = "top", legend.margin=margin(t=-0.1, b=-0.2, unit="cm")) +
  coord_fixed(ratio=1/300, ylim = c(0, 100), xlim =c(0,0.5)) +
  labs(color = "{\\scriptsize Scenario}", linetype="{\\scriptsize Bound}", x="$\\alpha$", y="$p^f_j$ [\\%]") +
  geom_line(aes(linetype=bound), size=0.5) + 
  geom_point(data=dfl_det, alpha=a, size=0.5) +
  geom_line(data=dfa, aes(alpha, 100*p, color=NULL), linetype=3)
gg + facet_wrap(~Unit, 2)
dev.off()
tikz(paste0(rdir, '/biondi-r1-p-vs-alpha.tex'),width=4.27,height=3,pointsize=8)
a = 0.85
cols = hue_pal()(3)
cols = c(icdarkgreen, icdarkblue, icdarkorange)
gg <- ggplot(dfl[dfl$Unit == "Reactor 1",], aes(alpha, p, color=scenario)) +
  theme_classic()+
  theme(text = element_text(color=icdarkblue)) +
  theme(plot.background = element_rect(fill=iclightblue)) +
  theme(panel.background = element_rect(fill=mbg)) +
  theme(axis.text = element_text(color=icdarkblue)) +
  #guides(color = guide_legend(title.hjust = 1.0),
  #       linetype = guide_legend(title.hjust = 1.0)) +
  scale_color_manual(values=cols,
                    labels=c("{\\scriptsize avg\\,}", "{\\scriptsize high\\,}", "{\\scriptsize low\\,}")) +
  scale_linetype_manual(values=c(1, 2),
                     labels=c("{\\scriptsize freq\\,}", "{\\scriptsize MC\\qquad}")) +
  theme(legend.position = "top", legend.margin=margin(t=-0.05, b=-0.01, unit="cm")) +
  #coord_fixed(ratio=1/300, ylim = c(0, 100), xlim =c(0,0.5)) +
  labs(color = "{\\scriptsize Scenario}", linetype="{\\scriptsize Bound}", x="$\\alpha$", y="$p^f_j$ [\\%]") +
  geom_line(aes(linetype=bound), size=0.5) + 
  geom_point(data=dfl_det, alpha=a, size=0.5) +
  geom_line(data=dfa[dfa$Unit == "Reactor 1",], aes(alpha, 100*p, color=NULL), linetype=3)
gg
dev.off()
## PAPER ##

# Plot rob vs det
res_rob <- read.csv("rob/avg_results.csv")
res_det <- read.csv("det/avg41_results.csv")
res_rob <- res_rob[,c("Cost", "gapmean", "gapmax", "gapmin", "slack")]
res_rob$mode <- "robust"
res_det <- res_det[,c("Cost", "gapmean", "gapmax", "gapmin", "slack")]
res_det$mode <- "deterministic"
res_comp <- rbind(res_rob, res_det)

tikz('/home/jw3617/homedir/STN/STN-Scheduler/results/biondi-det-vs-rob-cost.tex',width=8,height=6)
ggplot() +
  labs(y="Cost", x="") +
  geom_boxplot(data=res_comp, aes(x=mode, y=Cost, color=mode))
dev.off()

tikz('/home/jw3617/homedir/STN/STN-Scheduler/results/biondi_det-vs-rob-maxgap.tex',width=8,height=6)
ggplot(res_comp, aes(x=mode, y=Cost, color=mode)) +
  theme(text = element_text(size=10, color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  labs(y="Maximum MIP gap [\\%]", x="") +
  geom_boxplot()
dev.off()

tikz('/home/jw3617/homedir/STN/STN-Scheduler/results/biondi_det-vs-rob-avggap.tex',width=8,height=6)
ggplot(res_comp, aes(x=mode, y=gapmean, color=mode)) +
  theme(text = element_text(size=10, color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  labs(y="Average MIP gap [\\%]", x="") +
  geom_boxplot()
dev.off()

tikz(paste0(rdir,'/biondi-det-vs-rob.tex'),width=4.27,height=3)
gg <- ggplot(res_comp)+
  theme_classic()+
  theme(text = element_text(color=icdarkblue)) +
  theme(plot.background = element_rect(fill=iclightblue)) +
  theme(panel.background = element_rect(fill=mbg)) +
  theme(axis.text = element_text(color=icdarkblue))
cost <- gg +
  labs(y="cost [$10^3$]", x="") +
  geom_boxplot(aes(x=mode, y=Cost/1000), color=icdarkblue)
gap <- gg + 
  labs(y="Average MIP gap [\\%]", x="") +
  geom_boxplot(aes(x=mode, y=gapmean), color=icdarkblue)
multiplot(cost, gap, cols=2)
dev.off()

tikz(paste0(rdir, '/biondi-price-of-rob.tex'),width=4.27,height=3)
res_price <- read.csv("1period_results.csv")
gg <- ggplot(res_price) +
  theme_classic()+
  theme(text = element_text(color=icdarkblue)) +
  theme(plot.background = element_rect(fill=iclightblue)) +
  theme(panel.background = element_rect(fill=mbg)) +
  theme(axis.text = element_text(color=icdarkblue))
pcost <- gg +
  labs(y="$p^f_{j} [\\%]$", x="cost [$10^3$]") +
  geom_point(aes(Cost/2/1000, Reactor_1), color=icdarkblue)
palpha <- gg +
  labs(y="$p^f_{j} [\\%]$", x="$\\alpha$") +
  geom_point(aes(alpha, Reactor_1), color=icdarkblue)
multiplot(palpha, pcost, cols=2, ident=TRUE)
dev.off()

# Plot N=100 vs N=1000
approach = "mcre2"
res_mc_high = read.csv(paste0("mc/high_", approach ,"_results.csv"))
#res_mc_high$index = NULL
res_mc_avg = read.csv(paste0("mc/avg_", approach, "_results.csv"))
res_mc_low = read.csv(paste0("mc/low_", approach, "_results.csv"))
#res_mc_low$index = NULL
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc$N = "100"
approach = "mc"
res_mc_high = read.csv(paste0("mc/high_", approach, "1000_results.csv"))
res_mc_high$index = NULL
res_mc_avg = read.csv(paste0("mc/avg_", approach, "1000_results.csv"))
res_mc_low = read.csv(paste0("mc/low_", approach, "1000_results.csv"))
res_mc_low$index = NULL
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc2 = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc2$N = "1000"
res_mc$index <- NULL
res_mc2$group <- paste0(res_mc2$scenario, res_mc2$alpha)
res_mc$group <- paste0(res_mc$scenario, res_mc$alpha)
res_mc2$Reactor_1_100 <- res_mc$Reactor_1
res_mc2$Reactor_1 <- apply(res_mc2[,c("Reactor_1", "Reactor_1_100")], 1, max)
res_mc2$Reactor_1_100 <- NULL
res_mc = rbind(res_mc, res_mc2)
res_mc$bound <- "MC"
res_mc_100 <-res_mc

approach = "freq"
res_mc_high = read.csv(paste0("mc/high_", approach ,"_results.csv"))
#res_mc_high$index = NULL
res_mc_avg = read.csv(paste0("mc/avg_", approach, "_results.csv"))
res_mc_low = read.csv(paste0("mc/low_", approach, "_results.csv"))
#res_mc_low$index = NULL
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc$N = "100"
res_mc_high = read.csv(paste0("mc/high_", approach, "1000_results.csv"))
res_mc_high$index = NULL
res_mc_avg = read.csv(paste0("mc/avg_", approach, "1000_results.csv"))
res_mc_low = read.csv(paste0("mc/low_", approach, "1000_results.csv"))
res_mc_low$index = NULL
res_mc_high$scenario = "high"
res_mc_avg$scenario = "avg"
res_mc_low$scenario = "low"
res_mc2 = rbind(res_mc_avg, res_mc_low, res_mc_high)
res_mc2$N = "1000"
res_mc$index <- NULL
res_mc2$group <- paste0(res_mc2$scenario, res_mc2$alpha)
res_mc$group <- paste0(res_mc$scenario, res_mc$alpha)
res_mc2$Reactor_1_100 <- res_mc$Reactor_1
res_mc2$Reactor_1 <- apply(res_mc2[,c("Reactor_1", "Reactor_1_100")], 1, max)
res_mc2$Reactor_1_100 <- NULL
res_mc2 = rbind(res_mc, res_mc2)
res_mc2$bound = "freq"
keys <- c("alpha", "Reactor_1", "scenario", "bound", "N")
res <- rbind(res_mc_100[,keys], res_mc2[,keys])
## PAPER ##
cols = hue_pal()(3)
tikz(paste0(rdir, '/biondi-100vs1000.tex'),width=6,height=3)
ggplot(res, aes(alpha, Reactor_1, color=scenario, linetype=N)) + 
  theme_classic()+
  theme(text = element_text(color=icdarkblue)) +
  theme(plot.background = element_rect(fill=iclightblue)) +
  theme(panel.background = element_rect(fill=mbg)) +
  theme(axis.text = element_text(color=icdarkblue)) +
  guides(color = guide_legend(title.hjust = 1.0), linetype = guide_legend(title.hjust = 1.0)) +
  theme(legend.position = "top", legend.margin=margin(t=-0.1, b=-0.2, unit="cm")) +
  scale_color_manual(values=cols,
                     labels=c("avg\\,", "high\\,", "low\\,")) +
  scale_linetype_manual(values=c(1, 2),
                        labels=c("100\\,", "1000\\qquad")) +
  coord_fixed(ratio=1/300, ylim = c(0, 100), xlim =c(0,0.5)) +
  labs(y="$p^f_j$", x="$\\alpha$", color = "Scenario") + 
  geom_line(size=1.5) +
  facet_grid(.~bound)
dev.off()
## PAPER ##


tikz('/home/jw3617/homedir/STN/STN-Scheduler/results/biondi-100vs1000.tex',width=6,height=2.)
tikz('H:/STN/STN-Scheduler/results/biondi-100vs1000.tex',width=6,height=2.5)
multiplot(ggfreq, ggmc, cols=2, ident=TRUE, remove_legend=TRUE)
dev.off()

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, remove_legend=FALSE, ident=FALSE, width=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    if (remove_legend){
      layout <- rbind(rep(numPlots + 1, cols), layout)
      print(ceiling(numPlots/cols) + 1)
    }
  }
  if (remove_legend == TRUE) {
    legend <- g_legend(plots[[1]])
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    height = rep(1, ceiling(numPlots/cols))
    if (remove_legend == TRUE){
      height = c(0.1, rep(1, ceiling(numPlots/cols)))
    }
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), heights=height)))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      plt <- plots[[i]]
      if (ident){
        if (matchidx$col > 1) {
          plt <- plt + theme(axis.title.y = element_blank(), 
                             axis.text.y = element_blank())
        }
        print(matchidx$row)
        if (matchidx$row < ceiling(numPlots/cols)) {
          print(matchidx$row)
          plt <- plt + theme(axis.title.x = element_blank(), 
                             axis.text.x = element_blank())
        }
      }
      print(plt + theme(legend.position = "none"),
            vp = viewport(layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col))
    }
  }
  if (remove_legend == TRUE){
    matchidx <- as.data.frame(which(layout == numPlots +1, arr.ind = TRUE))
    vp = viewport(layout.pos.row = matchidx$row,
                  layout.pos.col = matchidx$col)
    pushViewport(vp)
    grid.draw(legend)
  }
}
multiplot(h, r1, r2, s, cols=2, ident = TRUE)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


