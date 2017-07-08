#---
#
# original code is from Guo email
# he submitted his manuscript to NEJM
# and I modified his R code to beautify his art style
# please see his email at google account
# R code and data/ Oct 28 , 2016
# Create the correlation heatmap with ggplot2
# part of the code is from drawChinaMap.R
# please see it.
#
#---

# @raw data is from Guo's project and his txt processed data
# @data analysis
# @result presentation
# @update 2071-07-08
#---

library(tidyverse)
library(reshape2)
library(metafor)
library(maptools)
library(plyr)
library(grid)
library(gridBase)
library(gridExtra)


setwd("C:\\Users\\Yisong\\Desktop\\ChinaMapDB\\1\\bou2_4m")

china.map.raw <- readShapePoly("bou2_4p.shp") 
# plot(china.map.raw)
"
how to change to the Chinese characters
"
# http://bbs.pinggu.org/thread-2624815-5-1.html
prov.name <- iconv(china.map.raw@data$NAME, from = 'GBK', to = 'UTF-8')
 
Northeast <- c('黑龙江省','吉林省','辽宁省')
North     <- c('北京市','天津市','河北省','山西省','内蒙古自治区')
Northwest <- c('新疆维吾尔自治区','青海省','甘肃省','宁夏回族自治区','陕西省')
East      <- c('上海市','山东省','江苏省','安徽省','浙江省','江西省','福建省')
Central   <- c('河南省','湖北省','湖南省')
South     <- c('广东省','广西壮族自治区','海南省')
Southwest <- c('重庆市','西藏自治区','云南省','四川省','贵州省')
Exclude   <- c('台湾省','香港特别行政区','澳门特别行政区')

prov.group <- rep(0 ,length(prov.name))
prov.group[prov.name %in% Northeast] <- 1
prov.group[prov.name %in% North]     <- 2
prov.group[prov.name %in% Northwest] <- 7
prov.group[prov.name %in% East]      <- 4
prov.group[prov.name %in% Central]   <- 5
prov.group[prov.name %in% South]     <- 6
prov.group[prov.name %in% Southwest] <- 3
prov.group[prov.name %in% Exclude]   <- 1


# this is for Macau? I am not sure.
# I change it from 0 to 8. I do not need the 0 group.
# I assign this group to 8. It seems right, anyway.
prov.group[899] <- 1

reg.shp      <- data.frame(provclass = prov.group,id = seq(0:924) - 1) 
china.map    <- fortify(china.map.raw)
china.map.df <- join(china.map, reg.shp, type = "full")

setwd("C:\\Users\\Yisong\\Desktop")
pdf("figure1A.pdf")
ggplot(china.map.df, aes(x = long, y = lat, group = group, fill = provclass)) +
     geom_polygon(colour = "grey40") +
     scale_fill_gradient( name = 'Geographical regions',                         
                          labels = rev( c('NorthWest','South','Central',
                                     'East','SouthWest','North','NorthEast') ),
                          low = "white", high = "black",
                          breaks = seq(1:7),
                          guide = 'legend') + 
     coord_map("polyconic")  +
     guides(fill = guide_legend(ncol = 2)) +
     theme( panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = c(0.2,0.98),
            legend.title.align = 0.5
          )

dev.off()

setwd('C:\\Users\\Yisong\\Desktop\\nejm_data')
# setwd('/home/zhenyisong/data/guotingwei/nejm_data')
# import the correlation matrix;
cormat <- read.csv("LD35new.csv", row.names = 1)  # read csv file 
cormat.colnames  <- colnames(cormat)
cormat.colnames  <- sub("\\.", "\\*", cormat.colnames ,perl = T)
cormat.colnames  <- sub("\\.", "\\:", cormat.colnames ,perl = T)
colnames(cormat) <- cormat.colnames
# correlation matrix have to be in the matrix format;
cormat <- as.matrix(cormat)
head(cormat)
#row.names(cormat)<-cormat[,1]
# cormat<-cormat[,-c(1)]

#The package reshape is required to melt the correlation matrix :

melted_cormat <- melt(cormat)

# Functions to Get lower triangle of the correlation matrix;
get_lower_tri <- function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}


# get the uppper upper triangle;
upper_tri <- get_upper_tri(cormat)
upper_tri



# Melt the correlation matrix of upper tri again;

melted_cormat <- melt(upper_tri, na.rm = TRUE)

#setwd('C:\\Users\\Yisong\\Desktop')
#pdf("figure3B.pdf")
dev.off()
figure.3A <- ggplot(data = melted_cormat, aes(X2,X1, fill = value)) +
             geom_tile(color = "white") +
             scale_fill_gradient2(low = "blue", high = "brown", mid = "white", 
                                  midpoint = 0, limit = c(-1,1), space = "Lab", 
                                  name="Pearson\nCorrelation") +
             #theme_minimal() + 
             theme( axis.text.x = element_text( angle = 90, vjust = 1, family = 'serif',
                                                size  = 10, hjust = 1, face = "italic"),
                    axis.text.y = element_text( angle = 0, vjust = 1, family = 'serif',
                                                size  = 10, hjust = 1, face = "italic")) +
             coord_fixed()


# Use geom_text() to add the correlation coefficients on the graph
# Use a blank theme (remove axis labels, panel grids and background, and axis ticks)
# Use guides() to change the position of the legend title

figure.3A + 
  #geom_text(aes(Var2, Var1, label = value), color = "black", size = 1) +
  theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#dev.off()

#----------------------------------------------------------------------
# Figure 4
# @updated 2017-07-08 
#----------------------------------------------------------------------

setwd("C:\\Users\\Yisong\\Desktop\\RE3a_Figures")
# Load input data for all the alleles;
B0702.df <- read.csv("B0702.csv", stringsAsFactors = FALSE)
# # If OR's and 95% CI's, run these commented out lines
# data$beta <- log(data$OR)
# data$se   <- (log(data$UCL)-log(data$LCL))/(2*1.96)
# Assign values for plottin
# Combine data into summary estimate
res_B0702  <- rma(yi = B0702.df$beta, sei = B0702.df$se, method = "FE")
summary(res_B0702)

B07.df <- read.csv("B07.csv", stringsAsFactors = FALSE)
res_B07  <- rma(yi = B07.df$beta, sei = B07.df$se, method = "FE")
summary(res_B07)

C0302.df <- read.csv("C0302.csv", stringsAsFactors = FALSE)
res_C0302  <- rma(yi = C0302.df$beta, sei = C0302.df$se, method = "FE")
summary(res_C0302)


DRB03.df   <- read.csv("DRB03.csv", stringsAsFactors = FALSE)
res_DRB03  <- rma(yi = DRB03.df$beta, sei = DRB03.df$se, method = "FE")
summary(res_DRB03)


DRB07.df   <- read.csv("DRB07.csv", stringsAsFactors = FALSE)
res_DRB07  <- rma(yi = DRB07.df$beta, sei = DRB07.df$se, method = "FE")
summary(res_DRB07)


DRB12.df   <- read.csv("DRB12.csv", stringsAsFactors = FALSE)
res_DRB12  <- rma(yi = DRB12.df$beta, sei = DRB12.df$se, method = "FE")
summary(res_DRB12)



###############################################
#  Working on side by side forest plot
###############################################


# mfrow meaning number of row, 2 is how many panel will be side by side;
# mar meaning margin
# mar (bottom, left, top, right)
par(mfrow = c(2,3), mar = c(5,0,3,0), pin = c(5,5),cex = 0.78)
# Plot combined data
# transf=exp meaning beta will transformed to OR;
#  refline=1 meaning refrence line (0 for beta and 1 for OR);
# annotate=FALSE remove the annotation,for example, .007[-0.01,0.16];
# slab=rep("",length(res$yi) meaning remove study group;
# mlab="Summary Estimate" meaning the lable of summary esitiate;
# xlim=c(0.9,1.3) tell the postion of figure;
# cat(paste('"',"Beta (95%CI)","Heterogeneity p-value=0.7321",'"',sep='\n')), but failed;
# cex=0.8 control the size of text;
# size control the position of the figure;
# line : on which MARgin line;
#outcomes <- res_B0702$yi[1:7]
#sampling.variances <- res_B0702$vi[1:7]
#forest(x = outcomes, vi =  sampling.variances , annotate = FALSE, refline = 0, xlab = "Beta (95%CI)", 
#        slab = labs,  cex=.75, mlab = "Meta p-value=2.49E-08", order = c(7:1),
#        psize = 3)
#

label.h <- -1
forest( res_B0702 , annotate = FALSE, refline = 0, xlab = "Beta (95%CI)", 
        slab = B0702.df$Region, mlab = expression(italic(P[meta])==~2.49E-08), order = c(7:1),
        psize = 3.5)
# set the p value from meta analysis
# mtext(paste("Association p-value=",summary(res_B0702)$pval),side=3, line=-1)
# mtext(paste("Heterogeneity p-value=",summary(res_B0702)$QEp),side=3, line=-2.25)
# set the p value by youreslf;
mtext("HLA-B*07:02",side = 3,line = label.h, adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.7321),side = 1, line = 4, cex = 0.8)


# Plot combined data
# forest(res_B07, annotate=FALSE,refline=0, xlab="Beta (95%CI)", slab=rep("",length(res_B07$yi)), mlab=NA)
forest( res_B07, annotate = FALSE,refline = 0, xlab = "Beta (95%CI)", 
        slab = B07.df$Region, mlab = expression(italic(P[meta])==~8.24E-11), psize = 3, order = c(7:1))
mtext("HLA-B*07",side = 3, line = label.h, adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.5941),side = 1, line = 4, cex = 0.8)

# Plot combined data
forest( res_C0302, annotate = FALSE,refline = 0, xlab = "Beta (95%CI)", 
        slab = C0302.df$Region, mlab = expression(italic(P[meta])==~2.10E-8),psize = 3, order = c(7:1))
mtext("HLA-C*03:02",side = 3, line = label.h, adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.2074),side = 1, line = 4, cex = 0.8)

#dev.off()

#par(mfrow = c(1,3), mar = c(4,0,3,0), pin = c(5,5),cex = 0.85)

# Plot combined data
forest( res_DRB03, annotate = FALSE,refline = 0, xlab = "Beta (95%CI)", 
        slab = DRB03.df$Region, mlab = expression(italic(P[meta])==~4.26E-8),psize = 3, order = c(7:1))
mtext("HLA-DRB1*03",side = 3, line = label.h, adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.4155),side = 1,line = 4, cex = 0.8)

# Plot combined data
forest( res_DRB07, annotate = FALSE,refline = 0, xlab = "Beta (95%CI)", 
        slab = DRB07.df$Region, mlab = expression(italic(P[meta])==~6.67E-11),psize = 3, order = c(7:1))
mtext("HLA-DRB1*07",side = 3, line = label.h, adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.1712),side = 1,line = 4, cex = 0.8)


# Plot combined data
forest( res_DRB12, annotate = FALSE,refline = 0, xlab = "Beta (95%CI)", 
        slab = DRB12.df$Region, mlab = expression(italic(P[meta])==~6.64E-9),psize = 3 ,order = c(7:1))
mtext("HLA-DRB1*12",side = 3, line = label.h,adj = 0, cex = 0.9)
mtext(expression(italic(P[heterous])==~0.9670),side = 1,line = 4, cex = 0.8)

# figure 1
#--end 

#---
# Figure.3B
#---pdf('Figure2X.pdf')
dev.off()
setwd("C:\\Users\\Yisong\\Desktop\\nejm_data")
figure3b.filename <- 'Figure3B_OriginalData.xlsx'
figure3b.data.df  <- read.xlsx( figure3b.filename, sheetIndex = 1, header = TRUE)
colnames(figure3b.data.df)[dim(figure3b.data.df)[2]] <- 'Feq.(%)'
figure3b.data.df$'Feq.(%)' <- round(as.numeric(figure3b.data.df$F),2)

g <- tableGrob(figure3b.data.df, rows = NULL)

find.cell <- function(table, row, col, name = "core-fg") {
    layout <- table$layout
    which(layout$t == row & layout$l == col & layout$name == name)
}

cell.in.x <- c( rep(c(2:4),each = 4),rep(5,3),c(6,6),rep(c(7:14),each = 2),
                c(15,15),c(16,16),c(17,17),c(18,18),c(19,19,19,19),rep(c(20:22),3),
                rep(c(23:25),each = 2))
cell.in.y <- c( rep(c(2:5),3),c(2:4),c(2,3),rep(c(3:4),8),
                c(3,5),c(3,5),c(1,5),c(1,4),c(1,3:5),rep(c(3:5),each = 3),
                rep(c(4:5),3))

cell.len   <- length(cell.in.x)
cell.fill  <- 'green'
for (i in c(1:60) ){
    cell.x = cell.in.x[i]
    cell.y = cell.in.y[i]
    ind <- find.cell(g, cell.x, cell.y, "core-bg")
    if (cell.x > 14) {
        
        g$grobs[ind][[1]][["gp"]] <- gpar(fill = 'lightcoral', col = NA)
    }
    else {
        g$grobs[ind][[1]][["gp"]] <- gpar(fill = 'lightblue3', col = NA)
    }
    
}

grid.newpage()
grid.draw(g)
#ggsave('Figure2X.png', g)



#apply(figure3b.data.df[,c(1:4)], 2, function(x) {substring(x,12,16)})

#---
# Figure3X
#---

#---
# Cookbook for R Graphs
# Shapes and line types
#--

##---
## deprecated!!
##
#figure.last <- rma(yi = c(0.108, 0.035, 0.079, 0.021), sei = c(0.017,0.016,0.011,0.01), method = "FE")
#forest( figure.last, annotate = FALSE,refline = 0, slab = c('m','f','m','f'), alim = c(-0.15, 0.15))
##----

#---
# http://stackoverflow.com/questions/30493564/ggplot2-adding-space-between-bars-by-group-within-group
# http://stackoverflow.com/questions/10525957/how-to-draw-lines-outside-of-plot-area-in-ggplot2
#---
final.figure.df <- data.frame( x.cord   = c(0.2, 0.4, 1.4, 1.6),
                               sex      = factor(c(1, 2, 3, 4)),
                               resp     = c(0.108, 0.035, 0.079, 0.021),
                               group = factor(c(1, 1, 2, 2)),
                               se    = c(0.017,0.016,0.011,0.01) )
# plot.margin	
# margin around entire plot 
# (unit with the sizes of the top, right, bottom, and left margins)
dev.off()
limits <- aes(ymax = resp + se, ymin = resp - se)
p <- ggplot(final.figure.df, aes(y = resp, x = x.cord, group = group, xmax = 2.4, ymin = -0.15, ymax = 0.15))
p + geom_point(shape = 15, cex = 3, color = 'black') + 
    geom_errorbar(limits, , width = 0.04) +
    theme_classic() +
    theme( axis.line.y  = element_blank(), axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(), axis.title.y = element_blank(),
           axis.title.x = element_blank(), axis.line.x = element_line(size = 0.8) ) +
    coord_cartesian(ylim=c(-0.15, 0.15)) + 
    scale_y_continuous(breaks = c(-0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15)) +
    theme(plot.margin = unit(c(6,4,2,1), "cm")) + 
    coord_flip() +
    geom_segment(aes(x = 0.0, y = 0.0, xend = 2.2, yend = 0.0), linetype='dashed', size = 0.5) +
    geom_text( aes(2, -0.12, label = "HLA-B*07") ) + 
    geom_text( aes(1.6, -0.10, label = "Female") ) +
    geom_text( aes(1.4, -0.10, label = "Male") ) +
    geom_text( aes(0.6, -0.12, label = "HLA-DRB1*07") ) +
    geom_text( aes(0.4, -0.10, label = "Female") ) +
    geom_text( aes(0.2, -0.10, label = "Male") ) +
    geom_segment(aes(x = 1.6, y = 0.14,xend = 1.6, yend = 0.145)) +
    geom_segment(aes(x = 1.4, y = 0.145,xend = 1.6, yend = 0.145)) +
    geom_segment(aes(x = 1.4, y = 0.14,xend = 1.4, yend = 0.145)) +
    geom_segment(aes(x = 0.4, y = 0.14,xend = 0.4, yend = 0.145)) +
    geom_segment(aes(x = 0.2, y = 0.145,xend = 0.4, yend = 0.145)) +
    geom_segment(aes(x = 0.2, y = 0.14,xend = 0.2, yend = 0.145)) 

#annotation_custom(grob = linesGrob(), xmin = 1, xmax = 1.1, ymin = 100, ymax = 100)

grid.layout <- as.character(current.vpTree())

result  <- gregexpr("\\(viewport\\[(.*?)\\]\\-",grid.layout, perl = TRUE)
x.start <- attributes(result[[1]])$capture.start[1]
x.stop  <- x.start + attributes(result[[1]])$capture.length[1] - 1
grid.id <- substring(grid.layout,x.start,x.stop)

downViewport(grid.id)
pushViewport(dataViewport( yscale=c(0,100), clip='off',xscale=c(0,1)))
grid.lines(x=c(0.06, 0.79), y = c(56, 56), default.units='native' )
grid.lines(x=c(0.06, 0.79), y = c(35, 35), default.units='native' )
grid.text(x = 0.87, y = 43, default.units='native' ,label= expression(bolditalic(P[heterous]==~0.0015)))
grid.text(x = 0.87, y = 24, default.units='native' ,label = expression(bolditalic(P[heterous]==~6.38%*%10^{-5})))
#----------------------------------------------------------------------
# test code, alternative choice
# I am not sure if tingwei used this code to graph the data
#
#----------------------------------------------------------------------

#install.packages("rmeta")
#
## Simple Meta-analysis
#library(rmeta)
#
## examples of case control analysis;
#data(cochrane)
#cochrane
#
#colnames(cochrane)
#
## fit both fixed effects and random effects models to look at the odds ratios.
#model.FE <- meta.MH(n.trt,n.ctrl,ev.trt,ev.ctrl, names=name,data=cochrane)
#model.RE <- meta.DSL(n.trt,n.ctrl,ev.trt,ev.ctrl, names=name,data=cochrane)
#
## Fixed effects ( Mantel-Haenszel ) meta-analysis
#meta.MH(ntrt = n.trt, nctrl = n.ctrl, ptrt = ev.trt, pctrl = ev.ctrl, 
#              names = name, data = cochrane)
#
#
## forest plot
#
#CPplot <- function(model)
#{
#  c1 <- c("","Study",model$names,NA,"Summary")
#  c2 <- c("Deaths","(Steroid)",cochrane$ev.trt,NA,NA)
#  c3 <- c("Deaths","(Placebo)",cochrane$ev.ctrl,NA,NA)
#  c4 <- c("","OR",format(exp(model[[1]]),digits=2),NA,format(exp(model[[3]]),digits=2))
#  
#  tableText <-cbind(c1,c2,c3,c4)
#  mean   <- c(NA,NA,model[[1]],NA,model[[3]])
#  stderr <- c(NA,NA,model[[2]],NA,model[[4]])
#  low <- mean - 1.96*stderr
#  up <- mean + 1.96*stderr
#  forestplot(tableText,mean,low,up,zero=0, 
#             is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),clip=c(log(0.1),log(2.5)),xlog=TRUE)
#}
#
#CPplot(model.FE)