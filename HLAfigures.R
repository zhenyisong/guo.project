# @author Yisong Zhen
# @since  2017-10-08
# @update 2017-11-13
# see the original image file
# in the Guo's coorespondance
# raw data was sent by Guo in two seperated mails.
#---

# install.packages('metafor', repo = 'http://mirrors.ustc.edu.cn/CRAN/')
pkgs             <- c( 'metafor','tidyverse','magrittr','stringr','cowplot',
                       'rgdal','sp', 'tmap','openxlsx','grid','gridExtra', 
                       'reshape2')
load.libs        <- lapply(pkgs, require, character.only = T)

guo.figures.path <- file.path('D:\\sourcecode\\guo.project\\figures')

#---
# China map
# Figure1A
#---

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
prov.group[prov.name %in% Northwest] <- 3
prov.group[prov.name %in% East]      <- 4
prov.group[prov.name %in% Central]   <- 5
prov.group[prov.name %in% South]     <- 6
prov.group[prov.name %in% Southwest] <- 7
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
dev.off()
setwd(guo.figures.path)
jpeg( file  = 'Figure3.jpg', width = 800, height = 900, 
      units = 'mm', res = 300, pointsize = 50, quality = 50)
#---
#  Working on side by side forest plot
#  mfrow meaning number of row, 2 is how many panel will be side by side;
#  mar meaning margin
#---
par(mfrow = c(2,2), mar   = c(5,4,2,2))
pmap(args.plot.1, plot.img, lab.notation = lab.notation.1)
dev.off()


#---
#
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
                  data.frame( beta.point = beta,
                              sd.error   = se, 
                              groups     = as.factor(count.i) )
        count.i <- count.i + 1
        data.df <- rbind(data.df, buffle)
    }
    return(data.df)  
}

boxplot.data <- map( file.names, read.forest.data, 
                     lab.notation = lab.notation.1) %>%
                format.df()

B.07.pval    <- paste( 'italic(\'P\')[italic(\'heterogeneity\')]', ' == 0.0067', sep = '')
DRB1.07.pval <- paste( 'italic(\'P\')[italic(\'heterogeneity\')]', ' == 0.00058', sep = '')

#--
# I referenced the following code
# https://www.r-bloggers.com/boxplot-with-mean-and-standard-deviation-in-ggplot2-plus-jitter/
# function for computing mean, DS, max and min values
#---


# manual draw the map
#
box.width        <- 0.3
error.bar.jitter <- 0.05
error.bar.width  <- 0.01
ymin.plot.1      <- c( boxplot.data %>% filter(groups == 1) %>% 
                       select(beta.point)%>% min(),
                       boxplot.data %>% filter(groups == 2) %>%
                       select(beta.point) %>% min())

middle.plot.1   <- c(0.018112,0.066471)
female.sd.1     <- 0.009476
male.sd.1       <- 0.010375 

lower.plot.1     <- c( middle.plot.1[1] - female.sd.1,
                       middle.plot.1[2] - male.sd.1 )
upper.plot.1     <- c( middle.plot.1[1] + female.sd.1,
                       middle.plot.1[2] + male.sd.1 )
ymax.plot.1      <- c( boxplot.data %>% filter(groups == 1) %>% 
                       select(beta.point)%>% unlist %>% max(),
                       boxplot.data %>% filter(groups == 2) %>%
                       select(beta.point) %>% unlist %>% max())

              

five.nums.plot.1 <- data.frame( ymin   = ymin.plot.1,   lower = lower.plot.1,
                                middle = middle.plot.1, upper = upper.plot.1,
                                ymax   = ymax.plot.1,  groups = factor(1:2))

plot.1 <- ggplot() +

          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['upper']][1]), 
                       linetype = 'solid') +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['lower']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 - box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 + box.width, y    = five.nums.plot.1[['upper']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['lower']][1])) +
          geom_segment( aes( x    = 1 - box.width, y    = five.nums.plot.1[['middle']][1], 
                             xend = 1 + box.width, yend = five.nums.plot.1[['middle']][1]),
                        size = 0.5) +
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
                        size = 0.5) +

          geom_point(aes( x = 1 - 3 * error.bar.jitter, y = boxplot.data[['beta.point']][1])) +
          geom_segment( aes(x     = 1 - 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][1] - 
                                                                     boxplot.data[['sd.error']][1],
                            xend  = 1 - 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][1] +
                                                                     boxplot.data[['sd.error']][1])) +
          geom_segment( aes( x    = 1 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][1] - 
                                                                   boxplot.data[['sd.error']][1],
                             xend = 1 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][1] - 
                                                                    boxplot.data[['sd.error']][1])) +
          geom_segment( aes( x    = 1 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][1] + 
                                                                   boxplot.data[['sd.error']][1],
                             xend = 1 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][1] +
                                                                    boxplot.data[['sd.error']][1])) +

          geom_point(aes( x = 1 - 2 * error.bar.jitter, y = boxplot.data[['beta.point']][2])) +
          geom_segment( aes(x     = 1 - 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][2] - 
                                                                     boxplot.data[['sd.error']][2],
                            xend  = 1 - 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][2] +
                                                                     boxplot.data[['sd.error']][2])) +
          geom_segment( aes( x    = 1 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][2] - 
                                                                   boxplot.data[['sd.error']][2],
                             xend = 1 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][2] - 
                                                                    boxplot.data[['sd.error']][2])) +
          geom_segment( aes( x    = 1 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][2] + 
                                                                   boxplot.data[['sd.error']][2],
                             xend = 1 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][2] + 
                                                                    boxplot.data[['sd.error']][2])) +
          geom_point(aes( x = 1 - 1 * error.bar.jitter, y = boxplot.data[['beta.point']][3])) +
          geom_segment( aes(x     = 1 - 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][3] - 
                                                                     boxplot.data[['sd.error']][3],
                            xend  = 1 - 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][3] +
                                                                     boxplot.data[['sd.error']][3])) +
          geom_segment( aes( x    = 1 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][3] - 
                                                                   boxplot.data[['sd.error']][3],
                             xend = 1 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][3] - 
                                                                    boxplot.data[['sd.error']][3])) +
          geom_segment( aes( x    = 1 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][3] + 
                                                                   boxplot.data[['sd.error']][3],
                             xend = 1 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][3] + 
                                                                    boxplot.data[['sd.error']][3])) +
          geom_point(aes( x = 1 - 0 * error.bar.jitter, y = boxplot.data[['beta.point']][4])) +
          geom_segment( aes(x     = 1 - 0 * error.bar.jitter, y    = boxplot.data[['beta.point']][4] - 
                                                                     boxplot.data[['sd.error']][4],
                            xend  = 1 - 0 * error.bar.jitter, yend = boxplot.data[['beta.point']][4] +
                                                                     boxplot.data[['sd.error']][4])) +
          geom_segment( aes( x    = 1 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][4] - 
                                                                   boxplot.data[['sd.error']][4],
                             xend = 1 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][4] - 
                                                                    boxplot.data[['sd.error']][4])) +
          geom_segment( aes( x    = 1 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][4] + 
                                                                   boxplot.data[['sd.error']][4],
                             xend = 1 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][4] + 
                                                                    boxplot.data[['sd.error']][4])) +

          geom_point(aes( x = 1 + 1 * error.bar.jitter, y = boxplot.data[['beta.point']][5])) +
          geom_segment( aes(x     = 1 + 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][5] - 
                                                                     boxplot.data[['sd.error']][5],
                            xend  = 1 + 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][5] +
                                                                     boxplot.data[['sd.error']][5])) +
          geom_segment( aes( x    = 1 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][5] - 
                                                                   boxplot.data[['sd.error']][5],
                             xend = 1 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][5] - 
                                                                    boxplot.data[['sd.error']][5])) +
          geom_segment( aes( x    = 1 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][5] +
                                                                   boxplot.data[['sd.error']][5],
                             xend = 1 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][5] +
                                                                    boxplot.data[['sd.error']][5])) +
          geom_point(aes( x = 1 + 2 * error.bar.jitter, y = boxplot.data[['beta.point']][6])) +
          geom_segment( aes(x     = 1 + 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][6] - 
                                                                     boxplot.data[['sd.error']][6],
                            xend  = 1 + 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][6] +
                                                                     boxplot.data[['sd.error']][6])) +
          geom_segment( aes( x    = 1 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][6] - 
                                                                   boxplot.data[['sd.error']][6],
                             xend = 1 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][6] - 
                                                                    boxplot.data[['sd.error']][6])) +
          geom_segment( aes( x    = 1 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][6] +
                                                                   boxplot.data[['sd.error']][6],
                             xend = 1 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][6] +
                                                                    boxplot.data[['sd.error']][6])) +
          geom_point(aes( x = 1 + 3 * error.bar.jitter, y = boxplot.data[['beta.point']][7])) +
          geom_segment( aes(x     = 1 + 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][7] - 
                                                                     boxplot.data[['sd.error']][7],
                            xend  = 1 + 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][7] +
                                                                     boxplot.data[['sd.error']][7])) +
          geom_segment( aes( x    = 1 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][7] - 
                                                                   boxplot.data[['sd.error']][7],
                             xend = 1 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][7] - 
                                                                    boxplot.data[['sd.error']][7])) +
          geom_segment( aes( x    = 1 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][7] +
                                                                   boxplot.data[['sd.error']][7],
                             xend = 1 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][7] +
                                                                    boxplot.data[['sd.error']][7])) +

          geom_point(aes( x = 2 - 3 * error.bar.jitter, y = boxplot.data[['beta.point']][8])) +
          geom_segment( aes(x     = 2 - 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][8] - 
                                                                     boxplot.data[['sd.error']][8],
                            xend  = 2 - 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][8] +
                                                                     boxplot.data[['sd.error']][8])) +
          geom_segment( aes( x    = 2 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][8] - 
                                                                   boxplot.data[['sd.error']][8],
                             xend = 2 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][8] - 
                                                                    boxplot.data[['sd.error']][8])) +
          geom_segment( aes( x    = 2 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][8] + 
                                                                   boxplot.data[['sd.error']][8],
                             xend = 2 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][8] +
                                                                    boxplot.data[['sd.error']][8])) +            
          geom_point(aes( x = 2 - 2 * error.bar.jitter, y = boxplot.data[['beta.point']][9])) +
          geom_segment( aes(x     = 2 - 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][9] - 
                                                                     boxplot.data[['sd.error']][9],
                            xend  = 2 - 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][9] +
                                                                     boxplot.data[['sd.error']][9])) +
          geom_segment( aes( x    = 2 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][9] - 
                                                                   boxplot.data[['sd.error']][9],
                             xend = 2 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][9] - 
                                                                    boxplot.data[['sd.error']][9])) +
          geom_segment( aes( x    = 2 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][9] + 
                                                                   boxplot.data[['sd.error']][9],
                             xend = 2 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][9] +
                                                                    boxplot.data[['sd.error']][9])) +
          geom_point(aes( x = 2 - 1 * error.bar.jitter, y = boxplot.data[['beta.point']][10])) +
          geom_segment( aes(x     = 2 - 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][10] - 
                                                                     boxplot.data[['sd.error']][10],
                            xend  = 2 - 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][10] +
                                                                     boxplot.data[['sd.error']][10])) +
          geom_segment( aes( x    = 2 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][10] - 
                                                                   boxplot.data[['sd.error']][10],
                             xend = 2 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][10] - 
                                                                    boxplot.data[['sd.error']][10])) +
          geom_segment( aes( x    = 2 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][10] + 
                                                                   boxplot.data[['sd.error']][10],
                             xend = 2 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][10] +
                                                                    boxplot.data[['sd.error']][10])) +
          geom_point(aes( x = 2 - 0 * error.bar.jitter, y = boxplot.data[['beta.point']][11])) +
          geom_segment( aes(x     = 2 - 0 * error.bar.jitter, y    = boxplot.data[['beta.point']][11] - 
                                                                     boxplot.data[['sd.error']][11],
                            xend  = 2 - 0 * error.bar.jitter, yend = boxplot.data[['beta.point']][11] +
                                                                     boxplot.data[['sd.error']][11])) +
          geom_segment( aes( x    = 2 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][11] - 
                                                                   boxplot.data[['sd.error']][11],
                             xend = 2 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][11] - 
                                                                    boxplot.data[['sd.error']][11])) +
          geom_segment( aes( x    = 2 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][11] + 
                                                                   boxplot.data[['sd.error']][11],
                             xend = 2 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][11] +
                                                                    boxplot.data[['sd.error']][11])) +
          geom_point(aes( x = 2 + 1 * error.bar.jitter, y = boxplot.data[['beta.point']][12])) +
          geom_segment( aes(x     = 2 + 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][12] - 
                                                                     boxplot.data[['sd.error']][12],
                            xend  = 2 + 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][12] +
                                                                     boxplot.data[['sd.error']][12])) +
          geom_segment( aes( x    = 2 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][12] - 
                                                                   boxplot.data[['sd.error']][12],
                             xend = 2 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][12] - 
                                                                    boxplot.data[['sd.error']][12])) +
          geom_segment( aes( x    = 2 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][12] + 
                                                                   boxplot.data[['sd.error']][12],
                             xend = 2 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][12] +
                                                                    boxplot.data[['sd.error']][12])) +
          geom_point(aes( x = 2 + 2 * error.bar.jitter, y = boxplot.data[['beta.point']][13])) +
          geom_segment( aes(x     = 2 + 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][13] - 
                                                                     boxplot.data[['sd.error']][13],
                            xend  = 2 + 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][13] +
                                                                     boxplot.data[['sd.error']][13])) +
          geom_segment( aes( x    = 2 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][13] - 
                                                                   boxplot.data[['sd.error']][13],
                             xend = 2 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][13] - 
                                                                    boxplot.data[['sd.error']][13])) +
          geom_segment( aes( x    = 2 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][13] + 
                                                                   boxplot.data[['sd.error']][13],
                             xend = 2 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][13] +
                                                                    boxplot.data[['sd.error']][13])) +
          geom_point(aes( x = 2 + 3 * error.bar.jitter, y = boxplot.data[['beta.point']][14])) +
          geom_segment( aes(x     = 2 + 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][14] - 
                                                                     boxplot.data[['sd.error']][14],
                            xend  = 2 + 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][14] +
                                                                     boxplot.data[['sd.error']][14])) +
          geom_segment( aes( x    = 2 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][14] - 
                                                                   boxplot.data[['sd.error']][14],
                             xend = 2 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][14] - 
                                                                    boxplot.data[['sd.error']][14])) +
          geom_segment( aes( x    = 2 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][14] + 
                                                                   boxplot.data[['sd.error']][14],
                             xend = 2 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][14] +
                                                                    boxplot.data[['sd.error']][14])) +
          scale_x_continuous(breaks = c(1,2), limits = c(0.5, 2.5), labels = c('Female','Male')) +
          scale_y_continuous(breaks = seq(-0.1,24, 0.05), limits = c(-0.1,0.25)) +
          geom_segment(aes(x = 1, y = 0.18, xend = 2, yend = 0.18) ) +
          geom_segment(aes(x = 1, y = 0.17, xend = 1, yend = 0.18) ) +
          geom_segment(aes(x = 2, y = 0.17, xend = 2, yend = 0.18) ) +
          geom_text( aes(x = 1.5, y = 0.20), 
                     label = DRB1.07.pval, 
                     vjust = 'middle',
                     parse = T)  + 
          geom_text(aes(x = 1.5, y = 0.23),label = 'DRB1*07') +
          theme_classic() +
          ylab(label = 'Beta') +
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

#---
# parameters for second group
#---

ymin.plot.2     <- c( boxplot.data %>% filter(groups == 3) %>% 
                      select(beta.point)%>% min(),
                      boxplot.data %>% filter(groups == 4) %>%
                      select(beta.point) %>% min())

middle.plot.2    <- c(0.03909,0.0993)
female.sd.2      <- 0.014958
male.sd.2       <- 0.016


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

plot.2 <- ggplot() +

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
                        size = 0.5) + 
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
                        size = 0.5) +

          geom_point(aes( x = 1 - 3 * error.bar.jitter, y = boxplot.data[['beta.point']][15])) +
          geom_segment( aes(x     = 1 - 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][15] - 
                                                                     boxplot.data[['sd.error']][15],
                            xend  = 1 - 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][15] +
                                                                     boxplot.data[['sd.error']][15])) +
          geom_segment( aes( x    = 1 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][15] - 
                                                                   boxplot.data[['sd.error']][15],
                             xend = 1 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][15] - 
                                                                    boxplot.data[['sd.error']][15])) +
          geom_segment( aes( x    = 1 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][15] + 
                                                                   boxplot.data[['sd.error']][15],
                             xend = 1 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][15] +
                                                                    boxplot.data[['sd.error']][15])) +

          geom_point(aes( x = 1 - 2 * error.bar.jitter, y = boxplot.data[['beta.point']][16])) +
          geom_segment( aes(x     = 1 - 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][16] - 
                                                                     boxplot.data[['sd.error']][16],
                            xend  = 1 - 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][16] +
                                                                     boxplot.data[['sd.error']][16])) +
          geom_segment( aes( x    = 1 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][16] - 
                                                                   boxplot.data[['sd.error']][16],
                             xend = 1 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][16] - 
                                                                    boxplot.data[['sd.error']][16])) +
          geom_segment( aes( x    = 1 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][16] + 
                                                                   boxplot.data[['sd.error']][16],
                             xend = 1 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][16] + 
                                                                    boxplot.data[['sd.error']][16])) +
          geom_point(aes( x = 1 - 1 * error.bar.jitter, y = boxplot.data[['beta.point']][17])) +
          geom_segment( aes(x     = 1 - 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][17] - 
                                                                     boxplot.data[['sd.error']][17],
                            xend  = 1 - 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][17] +
                                                                     boxplot.data[['sd.error']][17])) +
          geom_segment( aes( x    = 1 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][17] - 
                                                                   boxplot.data[['sd.error']][17],
                             xend = 1 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][17] - 
                                                                    boxplot.data[['sd.error']][17])) +
          geom_segment( aes( x    = 1 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][17] + 
                                                                   boxplot.data[['sd.error']][17],
                             xend = 1 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][17] + 
                                                                    boxplot.data[['sd.error']][17])) +
          geom_point(aes( x = 1 - 0 * error.bar.jitter, y = boxplot.data[['beta.point']][18])) +
          geom_segment( aes(x     = 1 - 0 * error.bar.jitter, y    = boxplot.data[['beta.point']][18] - 
                                                                     boxplot.data[['sd.error']][18],
                            xend  = 1 - 0 * error.bar.jitter, yend = boxplot.data[['beta.point']][18] +
                                                                     boxplot.data[['sd.error']][18])) +
          geom_segment( aes( x    = 1 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][18] - 
                                                                   boxplot.data[['sd.error']][18],
                             xend = 1 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][18] - 
                                                                    boxplot.data[['sd.error']][18])) +
          geom_segment( aes( x    = 1 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][18] + 
                                                                   boxplot.data[['sd.error']][18],
                             xend = 1 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][18] + 
                                                                    boxplot.data[['sd.error']][18])) +

          geom_point(aes( x = 1 + 1 * error.bar.jitter, y = boxplot.data[['beta.point']][19])) +
          geom_segment( aes(x     = 1 + 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][19] - 
                                                                     boxplot.data[['sd.error']][19],
                            xend  = 1 + 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][19] +
                                                                     boxplot.data[['sd.error']][19])) +
          geom_segment( aes( x    = 1 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][19] - 
                                                                   boxplot.data[['sd.error']][19],
                             xend = 1 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][19] - 
                                                                    boxplot.data[['sd.error']][19])) +
          geom_segment( aes( x    = 1 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][19] +
                                                                   boxplot.data[['sd.error']][19],
                             xend = 1 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][19] +
                                                                    boxplot.data[['sd.error']][19])) +
          geom_point(aes( x = 1 + 2 * error.bar.jitter, y = boxplot.data[['beta.point']][20])) +
          geom_segment( aes(x     = 1 + 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][20] - 
                                                                     boxplot.data[['sd.error']][20],
                            xend  = 1 + 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][20] +
                                                                     boxplot.data[['sd.error']][20])) +
          geom_segment( aes( x    = 1 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][20] - 
                                                                   boxplot.data[['sd.error']][20],
                             xend = 1 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][20] - 
                                                                    boxplot.data[['sd.error']][20])) +
          geom_segment( aes( x    = 1 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][20] +
                                                                   boxplot.data[['sd.error']][20],
                             xend = 1 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][20] +
                                                                    boxplot.data[['sd.error']][20])) +
          geom_point(aes( x = 1 + 3 * error.bar.jitter, y = boxplot.data[['beta.point']][21])) +
          geom_segment( aes(x     = 1 + 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][21] - 
                                                                     boxplot.data[['sd.error']][21],
                            xend  = 1 + 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][21] +
                                                                     boxplot.data[['sd.error']][21])) +
          geom_segment( aes( x    = 1 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][21] - 
                                                                   boxplot.data[['sd.error']][21],
                             xend = 1 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][21] - 
                                                                    boxplot.data[['sd.error']][21])) +
          geom_segment( aes( x    = 1 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][21] +
                                                                   boxplot.data[['sd.error']][21],
                             xend = 1 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][21] +
                                                                    boxplot.data[['sd.error']][21])) +

          geom_point(aes( x = 2 - 3 * error.bar.jitter, y = boxplot.data[['beta.point']][22])) +
          geom_segment( aes(x     = 2 - 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][22] - 
                                                                     boxplot.data[['sd.error']][22],
                            xend  = 2 - 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][22] +
                                                                     boxplot.data[['sd.error']][22])) +
          geom_segment( aes( x    = 2 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][22] - 
                                                                   boxplot.data[['sd.error']][22],
                             xend = 2 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][22] - 
                                                                    boxplot.data[['sd.error']][22])) +
          geom_segment( aes( x    = 2 - 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][22] + 
                                                                   boxplot.data[['sd.error']][22],
                             xend = 2 - 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][22] +
                                                                    boxplot.data[['sd.error']][22])) +            
          geom_point(aes( x = 2 - 2 * error.bar.jitter, y = boxplot.data[['beta.point']][23])) +
          geom_segment( aes(x     = 2 - 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][23] - 
                                                                     boxplot.data[['sd.error']][23],
                            xend  = 2 - 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][23] +
                                                                     boxplot.data[['sd.error']][23])) +
          geom_segment( aes( x    = 2 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][23] - 
                                                                   boxplot.data[['sd.error']][23],
                             xend = 2 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][23] - 
                                                                    boxplot.data[['sd.error']][23])) +
          geom_segment( aes( x    = 2 - 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][23] + 
                                                                   boxplot.data[['sd.error']][23],
                             xend = 2 - 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][23] +
                                                                    boxplot.data[['sd.error']][23])) +
          geom_point(aes( x = 2 - 1 * error.bar.jitter, y = boxplot.data[['beta.point']][24])) +
          geom_segment( aes(x     = 2 - 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][24] - 
                                                                     boxplot.data[['sd.error']][24],
                            xend  = 2 - 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][24] +
                                                                     boxplot.data[['sd.error']][24])) +
          geom_segment( aes( x    = 2 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][24] - 
                                                                   boxplot.data[['sd.error']][24],
                             xend = 2 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][24] - 
                                                                    boxplot.data[['sd.error']][24])) +
          geom_segment( aes( x    = 2 - 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][24] + 
                                                                   boxplot.data[['sd.error']][24],
                             xend = 2 - 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][24] +
                                                                    boxplot.data[['sd.error']][24])) +
          geom_point(aes( x = 2 - 0 * error.bar.jitter, y = boxplot.data[['beta.point']][25])) +
          geom_segment( aes(x     = 2 - 0 * error.bar.jitter, y    = boxplot.data[['beta.point']][25] - 
                                                                     boxplot.data[['sd.error']][25],
                            xend  = 2 - 0 * error.bar.jitter, yend = boxplot.data[['beta.point']][25] +
                                                                     boxplot.data[['sd.error']][25])) +
          geom_segment( aes( x    = 2 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][25] - 
                                                                   boxplot.data[['sd.error']][25],
                             xend = 2 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][25] - 
                                                                    boxplot.data[['sd.error']][25])) +
          geom_segment( aes( x    = 2 - 0 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][25] + 
                                                                   boxplot.data[['sd.error']][25],
                             xend = 2 - 0 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][25] +
                                                                    boxplot.data[['sd.error']][25])) +
          geom_point(aes( x = 2 + 1 * error.bar.jitter, y = boxplot.data[['beta.point']][26])) +
          geom_segment( aes(x     = 2 + 1 * error.bar.jitter, y    = boxplot.data[['beta.point']][26] - 
                                                                     boxplot.data[['sd.error']][26],
                            xend  = 2 + 1 * error.bar.jitter, yend = boxplot.data[['beta.point']][26] +
                                                                     boxplot.data[['sd.error']][26])) +
          geom_segment( aes( x    = 2 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][26] - 
                                                                   boxplot.data[['sd.error']][26],
                             xend = 2 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][26] - 
                                                                    boxplot.data[['sd.error']][26])) +
          geom_segment( aes( x    = 2 + 1 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][26] + 
                                                                   boxplot.data[['sd.error']][26],
                             xend = 2 + 1 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][26] +
                                                                    boxplot.data[['sd.error']][26])) +
          geom_point(aes( x = 2 + 2 * error.bar.jitter, y = boxplot.data[['beta.point']][27])) +
          geom_segment( aes(x     = 2 + 2 * error.bar.jitter, y    = boxplot.data[['beta.point']][27] - 
                                                                     boxplot.data[['sd.error']][27],
                            xend  = 2 + 2 * error.bar.jitter, yend = boxplot.data[['beta.point']][27] +
                                                                     boxplot.data[['sd.error']][27])) +
          geom_segment( aes( x    = 2 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][27] - 
                                                                   boxplot.data[['sd.error']][27],
                             xend = 2 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][27] - 
                                                                    boxplot.data[['sd.error']][27])) +
          geom_segment( aes( x    = 2 + 2 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][27] + 
                                                                   boxplot.data[['sd.error']][27],
                             xend = 2 + 2 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][27] +
                                                                    boxplot.data[['sd.error']][27])) +
          geom_point(aes( x = 2 + 3 * error.bar.jitter, y = boxplot.data[['beta.point']][28])) +
          geom_segment( aes(x     = 2 + 3 * error.bar.jitter, y    = boxplot.data[['beta.point']][28] - 
                                                                     boxplot.data[['sd.error']][28],
                            xend  = 2 + 3 * error.bar.jitter, yend = boxplot.data[['beta.point']][28] +
                                                                     boxplot.data[['sd.error']][28])) +
          geom_segment( aes( x    = 2 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][28] - 
                                                                   boxplot.data[['sd.error']][28],
                             xend = 2 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][28] - 
                                                                    boxplot.data[['sd.error']][28])) +
          geom_segment( aes( x    = 2 + 3 * error.bar.jitter - 
                                    error.bar.width,        y    = boxplot.data[['beta.point']][28] + 
                                                                   boxplot.data[['sd.error']][28],
                             xend = 2 + 3 * error.bar.jitter +
                                    error.bar.width,         yend = boxplot.data[['beta.point']][28] +
                                                                    boxplot.data[['sd.error']][28])) +
          scale_x_continuous(breaks = c(1,2), limits = c(0.5,2.5), labels = c('Female','Male')) +
          scale_y_continuous(breaks = seq(-0.25, 0.42, 0.1), limits = c(-0.25,0.45)) +
          geom_segment(aes(x = 1, y = 0.34, xend = 2, yend = 0.34) ) +
          geom_segment(aes(x = 1, y = 0.33, xend = 1, yend = 0.34) ) +
          geom_segment(aes(x = 2, y = 0.33, xend = 2, yend = 0.34) ) +
          geom_text( aes(x = 1.5, y = 0.38), 
                     label = B.07.pval, 
                     vjust = 'middle',
                     parse = T)  + 
          theme_classic() +
          ylab(label = 'Beta') +
          geom_text(aes(x = 1.5, y = 0.43),label = 'B*07') +
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

figure4.boxplot <- plot_grid( plot.2, plot.1, labels = c('A','B'), 
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
        figure4C.table$grobs[ind][[1]][['gp']] <- gpar(fill = 'blue1', col = NA)
    }
    else {
        figure4C.table$grobs[ind][[1]][['gp']] <- gpar(fill = 'red', col = NA)
    }
}


AH.low.V   <- linesGrob( x = unit(c(148, 148), 'mm'),
                         y = unit(c(0, 15), 'mm'))
AH.low.h1  <- linesGrob( x = unit(c(145, 148), 'mm'),
                         y = unit(c(0, 0), 'mm'))
AH.low.h2  <- linesGrob( x = unit(c(145, 148), 'mm'),
                        y = unit(c(15, 15), 'mm'))
AH.low.txt <- textGrob( label = 'AH', 
                        x = unit(150, 'mm'), 
                        y = unit(7.5, 'mm'),
                        just = 'left')

AH.hi.V   <- linesGrob( x = unit(c(148, 148), 'mm'),
                        y = unit(c(28, 50), 'mm'))
AH.hi.h1  <- linesGrob( x = unit(c(145, 148), 'mm'),
                        y = unit(c(28, 28), 'mm'))
AH.hi.h2  <- linesGrob( x = unit(c(145, 148), 'mm'),
                        y = unit(c(50, 50), 'mm'))
AH.hi.txt <- textGrob( label = 'AH', 
                       x = unit(150, 'mm'), 
                       y = unit(39, 'mm'),
                       just = 'left') 

CH.V      <- linesGrob( x = unit(c(148, 148), 'mm'),
                        y = unit(c(58, 80), 'mm'))
CH.h1     <- linesGrob( x = unit(c(145, 148), 'mm'),
                        y = unit(c(58, 58), 'mm'))
CH.h2     <- linesGrob( x = unit(c(145, 148), 'mm'),
                        y = unit(c(80, 80), 'mm'))
CH.txt    <- textGrob( label = 'CH', 
                       x = unit(150, 'mm'), 
                       y = unit(69, 'mm'),
                       just = 'left')
   

DR.txt    <- textGrob( label = 'DR2', 
                       x = unit(150, 'mm'), 
                       y = unit(130, 'mm'),
                       just = 'left')
   

figure4C.ggplot <- ggplot() + annotation_custom( grob = figure4C.table ) +
                              annotation_custom( grob = AH.low.V) +
                              annotation_custom( grob = AH.low.h1) +
                              annotation_custom( grob = AH.low.h2) +
                              annotation_custom( grob = AH.low.txt) +
                              annotation_custom( grob = AH.hi.V) +
                              annotation_custom( grob = AH.hi.h1) +
                              annotation_custom( grob = AH.hi.h2) +
                              annotation_custom( grob = AH.hi.txt) +
                              annotation_custom( grob = CH.txt) +
                              annotation_custom( grob = CH.V) +
                              annotation_custom( grob = CH.h1) +
                              annotation_custom( grob = CH.h2) +
                              annotation_custom( grob = CH.txt) +
                              annotation_custom( grob = DR.txt)


setwd(guo.figures.path)
ggsave( 'Figure4C.jpg', figure4C.ggplot, 
        width = 190, height = 150, dpi = 600, unit = 'mm')


#---
# in responding to Guo's final desiciosn
# from editor's suggestion
# updated since 02-27-2018
#---

# final figure figure 5A;

setwd('D:\\sourcecode\\guo.project\\update.data')
# setwd('/home/zhenyisong/data/guotingwei/nejm_data')
# import the correlation matrix;
cormat <- read.csv( 'ld_r2_36.csv', row.names = 1)  # read csv file 
cormat.colnames  <- colnames(cormat)
cormat.colnames  <- sub( '\\.', '\\*', cormat.colnames, perl = T)
cormat.colnames  <- sub( '\\.', '\\:', cormat.colnames, perl = T)
colnames(cormat) <- cormat.colnames
# correlation matrix have to be in the matrix format;
cormat <- as.matrix(cormat)
head(cormat)
#row.names(cormat)<-cormat[,1]
# cormat<-cormat[,-c(1)]

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

figure.5A <- ggplot(data = melted_cormat, aes(Var2,Var1, fill = value)) +
             geom_tile(color = 'white') +
             scale_fill_gradient2( low = 'blue', high = 'brown', mid = 'white', 
                                   midpoint = 0, limit = c(-1, 1), space = 'Lab', 
                                   name='Pearson\nCorrelation') +
             #theme_minimal() + 
             theme( axis.text.x = element_text( angle = 90, vjust = 1, family = 'serif',
                                                size  = 9, hjust = 1, face = 'italic'),
                    axis.text.y = element_text( angle = 0, vjust  = 1, family = 'serif',
                                                size  = 9, hjust = 1, face  = 'italic'),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border     = element_blank(),
                    panel.background = element_blank(),
                    axis.ticks       = element_blank(),
                    legend.justification = c(1, 0),
                    legend.position      = c(0.6, 0.7),
                    legend.text          = element_text(
                                             size = 9, colour = 'red'),
                    legend.direction = 'horizontal') +
             guides( fill = guide_colorbar( barwidth = 7, 
                                            barheight = 1,
                                            title.position = 'top', 
                                            title.hjust    = 0.5)) +
             coord_fixed()


# Use geom_text() to add the correlation coefficients on the graph
# Use a blank theme (remove axis labels, panel grids and background, and axis ticks)
# Use guides() to change the position of the legend title
  
setwd(guo.figures.path)
ggsave('Figure5A.jpeg', figure.5A, height = 150, dpi = 600, unit = 'mm')

