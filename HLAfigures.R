# @author Yisong Zhen
# @since  2017-10-08
# @update 2017-11-13
# see the original image file
# in the Guo's coorespondance
# raw data was sent by Guo in two seperated mails.
#---

# install.packages('metafor', repo = 'http://mirrors.ustc.edu.cn/CRAN/')
pkgs             <- c( 'metafor','tidyverse','magrittr','stringr','cowplot',
                       'rgdal','sp', 'tmap','openxlsx','grid','gridExtra')
load.libs        <- lapply(pkgs, require, character.only = T)

guo.figures.path <- file.path('D:\\sourcecode\\guo.project\\figures')

#---
# China map
# Figure1A
#---
# the original protocol in Chinese
# http://blog.sina.com.cn/s/blog_6bc5205e0102vma9.html
# the map raw data source is downloaded 
# from and saved in dir
#
# suggested readings
# http://stackoverflow.com/questions/29773240/triple-legend-in-ggplot2-with-point-shape-fill-and-color

# install.packages("maptools")
# install.packages('rgdal',repos = 'http://cran.ism.ac.jp/')
#chooseCRANmirror(graphics = getOption("menu.graphics"), ind = NULL,
#                 useHTTPS = getOption("useHTTPS", FALSE),
#                 local.only = FALSE)
# https://stackoverflow.com/questions/33355444/r-when-trying-to-install-package-internetopenurl-failed
# dependency error. you should install other package before go
# 'mapproj'
# ?
# https://github.com/jknowles/eeptools/issues/34
#---

# the Chinese characters cannot be parsed by pathname
# I therefore relocate the map data file
# 
#--

# tip
# i need to reset the languate setting
# define encoding to UTF-8
#---
Sys.setlocale(category = 'LC_ALL', locale = 'English')
rawmap.data.path <- file.path('D:\\sourcecode\\guo.project\\rawdata\\') %>%
                    paste('ChinaMap-2\\', sep = '') %>%
                    paste('全国省级、地市级、县市级行政区划shp\\', sep = '') %>%
                    paste('省界bou2_4m', sep = '')

#---
#rawmap.data.path <- file.path('D:\\sourcecode\\guo.project\\rawdata\\ChinaMap-2\\setmapinEnglish')
# avoid langauge encoding problem
#---
chinamap.rawdata <- readOGR(rawmap.data.path,'bou2_4p')

prov.name        <- chinamap.rawdata %>% 
                    {iconv(.@data$NAME, from = 'GBK', to = 'UTF-8')}
 
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
prov.group[prov.name %in% Exclude]   <- 8

#---
# this is for Macau? I am not sure.
# I change it from 0 to 8. I do not need the 0 group.
# I assign this group to 8. It seems right, anyway.
#---
prov.group[899] <- 8

chinamap.rawdata@data$regionClass  <- factor( prov.group, levels = 1:8,
                                              labels = c('Northeast','North','Northwest',
                                                         'East', 'Central','South','Southwest', 'Excluded'))
proj4string(chinamap.rawdata) <- '+proj=longlat +datum=WGS84'
china.tmap <- tm_shape(shp = chinamap.rawdata) +
              tm_borders(col = 'burlywood4') +
              tm_fill(col = 'regionClass', title = 'sample collection regions') +
              tm_compass() +
              tm_scale_bar() +
              tm_layout(frame = F)

(china.tmap)

setwd(guo.figures.path)
save_tmap(china.tmap, 'Figure1A.jpg', dpi = 600)


#---
# Figure3
# rawdata load from the specified directory
#---
rawdata.file.path  <- file.path('D:\\sourcecode\\guo.project\\update.data')
rawdata.file.names <- list( 'B07_2.csv',  'C0302_2.csv','DRB07_2.csv',
                            'DRB12_2.csv','B07_F.csv', 'B07_M.csv',
                            'DRB07_F.csv', 'DRB07_M.csv', 'B07_FM.csv',
                            'DRB07_FM.csv')
lab.notation.1 <- c( 'Southwest','Northeast','North',
                     'East','Northwest','Central', 'South')
getForest.data <- function(file.name, lab.notation) {
    f.data   <- read.csv(file.name) %>%
                arrange( match(Region,lab.notation) )
    
    f.result <- f.data %$% 
                {rma(yi = beta, sei = se, method = 'FE')}
    (summary(f.result))
    return(f.result)
}


setwd(rawdata.file.path)
f.all.results <- map(rawdata.file.names, getForest.data, lab.notation = lab.notation.1)
names(f.all.results) <- rawdata.file.names %>% unlist %>%
                        sub(pattern = '.csv', replacement = '')



#---
#  Working on side by side forest plot
#  mfrow meaning number of row, 2 is how many panel will be side by side;
#  mar meaning margin
#---
par(mfrow = c(2,2), mar   = c(5,4,2,2))


#---
# @params for metaR forest function
# transf=exp meaning beta will transformed to OR;
# refline=1 meaning refrence line (0 for beta and 1 for OR);
# annotate=FALSE remove the annotation,for example, .007[-0.01,0.16];
# slab=rep("",length(res$yi) meaning remove study group;
# mlab="Summary Estimate" meaning the lable of summary esitiate;
# xlim=c(0.9,1.3) tell the postion of figure;
# alim the actual x-axis limits;
# steps the number of tick marks for the x-axis (the default is 5);
# cat(paste('"',"Beta (95%CI)","Heterogeneity p-value=0.7321",'"',sep='\n')), but failed;
# cex=0.8 control the size of text;
# size control the position of the figure;
# line : on which MARgin line;
#---

plot.img <- function(data.df, mlab, mtext.1, mtext.2, mtext.3, lab.notation) {
    forest( data.df, annotate = FALSE,refline = 0, 
            xlab = 'Beta (95%CI)', slab = lab.notation, 
            mlab = mlab, psize = 3, alim = c(-0.3,0.3), steps = 10)
    mtext(mtext.3, side = 3, line = -.5, adj = 0)
    mtext(mtext.1, side = 3, line = -2.5)
    mtext(mtext.2, side = 1, line = 4, cex = 0.8, at = 0)
}

plot.1.list.names <- c('B07_2', 'C0302_2', 'DRB07_2', 'DRB12_2')
plot.1.results    <- f.all.results[plot.1.list.names]

mlab.param        <- list( B07_2   = 'Meta p-value=6.90E-10', C0302_2 = 'Meta p-value=4.450E-8',
                           DRB07_2 = 'Meta p-value=1.318E-8', DRB12_2 = 'Meta p-value=1.518E-9' )
mtext.param.1     <- list( B07_2   = 'HLA-B*07',    C0302_2 = 'HLA-C*03:02',
                           DRB07_2 = 'HLA-DRB1*07', DRB12_2 = 'HLA-DRB1*12' )
mtext.param.2     <- list( B07_2   = expression(italic(P)[heterous] == 0.7550),    
                           C0302_2 = expression(italic(P)[heterous] == 0.1729),
                           DRB07_2 = expression(italic(P)[heterous] == 0.1729), 
                           DRB12_2 = expression(italic(P)[heterous] == 0.1139) )

mtext.param.3     <- list( A   = 'A',  B = 'B', C = 'C', D = 'D')  

args.plot.1       <- list(plot.1.results, mlab.param, mtext.param.1, mtext.param.2, mtext.param.3)
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.1) )

setwd(guo.figures.path)
jpeg('Figure3.jpg', width = 1200, height = 802, units = 'mm',res = 150)
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.1) )
dev.off()


#---
# Figure 2 
#
#---

raw.data.path <- file.path('D:\\sourcecode\\guo.project\\update.data')


file.names    <- list('DRB07_F.csv', 'DRB07_M.csv', 'B07_F.csv','B07_M.csv')

read.forest.data <- function(filename, lab.notation) {
    setwd(raw.data.path)
    data <- read_csv( filename, col_names = TRUE, col_types = 'cnn') %>%
            arrange( match(Region,lab.notation) )
    return(data)
}

format.df <- function(data.lists) {
    data.df <- data.frame()
    count.i <- 1
    for(one.list in data.lists) {
        buffle <- one.list %$% 
                  data.frame( beta.point = beta, groups = as.factor(count.i) )
        count.i <- count.i + 1
        data.df <- rbind(data.df, buffle)
    }
    return(data.df)  
}

boxplot.data <- map( file.names, read.forest.data, 
                     lab.notation = lab.notation.1) %>%
                format.df()

B.07.pval    <- paste( 'italic(\'P\')[italic(\'heterogeneity\')]', ' == 0.00767', sep = '')
DRB1.07.pval <- paste( 'italic(\'P\')[italic(\'heterogeneity\')]', ' == 0.00058', sep = '')

#--
# I refenced the following code
# https://www.r-bloggers.com/boxplot-with-mean-and-standard-deviation-in-ggplot2-plus-jitter/
# function for computing mean, DS, max and min values
#---

"
get_five_nums <- function(x) {
  five.nums         <- c( min(x), mean(x) - sd(x), 
                          mean(x), mean(x) + sd(x), max(x))
  names(five.nums ) <- c('ymin', 'lower', 'middle', 'upper', 'ymax')
  return(five.nums)
}
"

# manual draw the map
#
box.width        <- 0.3
ymin.plot.1      <- c( boxplot.data %>% filter(groups == 1) %>% 
                       select(beta.point)%>% min(),
                       boxplot.data %>% filter(groups == 2) %>%
                       select(beta.point) %>% min())
middle.plot.1    <- c(0.03909,0.0993)
female.sd.1      <- 0.014958
male.sd.1        <- 0.016
lower.plot.1     <- c( middle.plot.1[1] - female.sd.1,
                       middle.plot.1[2] - male.sd.1 )
upper.plot.1     <- c( middle.plot.1[1] + female.sd.1,
                       middle.plot.1[2] + male.sd.1 )
ymax.plot.1      <- c( boxplot.data %>% filter(groups == 1) %>% 
                       select(beta.point)%>% unlist %>% max(),
                       boxplot.data %>% filter(groups == 2) %>%
                       select(beta.point) %>% unlist %>% max())
               

five.nums.plot.1 <- data.frame( ymin   = ymin.plot.1, lower = lower.plot.1,
                                middle = middle.plot.1, upper = upper.plot.1,
                                ymax   = ymax.plot.1,  groups = factor(1:2))

plot.1 <- boxplot.data %>%
          filter(groups == 1 | groups == 2) %>%
          ggplot(data = ., aes(x = groups, y = beta.point)) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['upper']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['lower']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 - box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 + box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['middle']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['middle']][1]),
                        size = 1) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.1[['upper']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.1[['upper']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.1[['lower']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.1[['lower']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.1[['upper']][2], 
                             xend = 2 - box.width, yend = five.nums.plot.1[['lower']][2])) +
          geom_segment( aes( x    = 2 + box.width, y    = five.nums.plot.1[['upper']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.1[['lower']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.1[['middle']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.1[['middle']][2]),
                        size = 1) +
          geom_point( aes(fill = groups), , 
                      shape = 16 , size = 2, alpha = 0.5, show.legend = F)  + 
          scale_x_discrete(labels = c('Female','Male')) +
          scale_y_continuous(limits = c(-0.1,0.18)) +
          geom_segment(aes(x = 1, y = 0.15, xend = 2, yend = 0.15) ) +
          geom_segment(aes(x = 1, y = 0.14, xend = 1, yend = 0.15) ) +
          geom_segment(aes(x = 2, y = 0.14, xend = 2, yend = 0.15) ) +
          geom_text( aes(x = 1.5, y = 0.16), 
                     label = B.07.pval, 
                     vjust = 'middle',
                     parse = T)  + 
          geom_text(aes(x = 1.5, y = 0.18),label = 'B*07') +
          theme_classic() +
          ylab(label = 'Beta') +
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

# parameters for second group
#---

ymin.plot.2     <- c( boxplot.data %>% filter(groups == 3) %>% 
                      select(beta.point)%>% min(),
                      boxplot.data %>% filter(groups == 4) %>%
                      select(beta.point) %>% min())
middle.plot.2   <- c(0.018112,0.066471)
female.sd.2     <- 0.009476
male.sd.2       <- 0.010375
lower.plot.2    <- c( middle.plot.2[1] - female.sd.2,
                      middle.plot.2[2] - male.sd.2 )
upper.plot.2    <- c( middle.plot.2[1] + female.sd.2,
                      middle.plot.2[2] + male.sd.2 )
ymax.plot.2     <- c( boxplot.data %>% filter(groups == 3) %>% 
                      select(beta.point)%>% unlist %>% max(),
                      boxplot.data %>% filter(groups == 4) %>%
                      select(beta.point) %>% unlist %>% max())


five.nums.plot.2 <- data.frame( ymin   = ymin.plot.2, lower   = lower.plot.2,
                                middle = middle.plot.2, upper = upper.plot.2,
                                ymax   = ymax.plot.2,  groups = factor(1:2))

plot.2 <- boxplot.data %>%
          filter(groups == 3 | groups == 4) %>%
          ggplot(aes( x = groups, y = beta.point)) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.2[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.2[['upper']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.2[['lower']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.2[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.2[['upper']][1], 
                             xend = 1 - box.width, yend = five.nums.plot.2[['lower']][1])) +
          geom_segment( aes( x    = 1 + box.width, y    = five.nums.plot.2[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.2[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.2[['middle']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.2[['middle']][1]),
                        size = 1) + 
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.2[['upper']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.2[['upper']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.2[['lower']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.2[['lower']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.2[['upper']][2], 
                             xend = 2 - box.width, yend = five.nums.plot.2[['lower']][2])) +
          geom_segment( aes( x    = 2 + box.width, y    = five.nums.plot.2[['upper']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.2[['lower']][2])) +
          geom_segment( aes( x    = 2 - box.width, y    = five.nums.plot.2[['middle']][2], 
                             xend = 2 + box.width, yend = five.nums.plot.2[['middle']][2]),
                        size = 1) +
          geom_point( aes(fill = groups), 
                      shape = 16 , size = 2, alpha = 0.5, show.legend = F)  + 
          scale_x_discrete(labels = c('Female','Male')) +
          scale_y_continuous(limits = c(-0.1,0.18)) +
          geom_segment(aes(x = 1, y = 0.15, xend = 2, yend = 0.15) ) +
          geom_segment(aes(x = 1, y = 0.14, xend = 1, yend = 0.15) ) +
          geom_segment(aes(x = 2, y = 0.14, xend = 2, yend = 0.15) ) +
          geom_text( aes(x = 1.5, y = 0.16), 
                     label = DRB1.07.pval, 
                     vjust = 'middle',
                     parse = T)  + 
          theme_classic() +
          ylab(label = 'Beta') +
          geom_text(aes(x = 1.5, y = 0.18),label = 'DRB1*07') +
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

figure4.boxplot <- plot_grid( plot.1, plot.2, labels = c('A','B'), 
                              ncol = 2, nrow = 1)
setwd(guo.figures.path)
ggsave( 'Figure4.jpeg', plot = figure4.boxplot, 
        dpi = 600, units = 'mm', width = 300)

#---
# Figure 4C
# completed in 2017-11-16
#
#---

raw.data.path     <- 'D:\\sourcecode\\guo.project\\update.data'
setwd(raw.data.path)
figure4C.filename <- 'haplotype.xlsx'
figure4C.df       <- read.xlsx( figure4C.filename, sheet = 1, 
                                colNames = TRUE)

dev.off()
figure4C.table <- tableGrob(figure4C.df, rows = NULL)

find.cell <- function(table, row, col, name = 'core-fg') {
    layout <- table$layout
    which( layout$t    == row & 
           layout$l    == col & 
           layout$name == name)
}

cell.in.x <- c( 2,2,
                rep(3:4, times = 3),
                rep(5:6, each  = 2),
                rep(7:9, each  = 2),
                rep(10,3),
                rep(11:21, each = 2))

cell.in.y <- c( 2,3,
                rep(c(2,3,5),times = 2),
                2,5,2,4,
                1,2,1,5,1,4,
                1,3,4,
                rep(3:4, times = 11))

cell.len   <- length(cell.in.x)
cell.fill  <- 'green'

for (i in c(1 : cell.len) ){
    cell.x = cell.in.x[i]
    cell.y = cell.in.y[i]
    ind <- find.cell(figure4C.table, cell.x, cell.y, 'core-bg')
    if (cell.x >= 14) {
        figure4C.table$grobs[ind][[1]][['gp']] <- gpar(fill = 'red', col = NA)
    }
    else {
        figure4C.table$grobs[ind][[1]][['gp']] <- gpar(fill = 'green', col = NA)
    }
}

grid.newpage()
grid.draw(figure4C.table)

setwd(guo.figures.path)
ggsave('Figure4C.jpeg', figure4C.table, height = 150, dpi = 600, unit = 'mm')
