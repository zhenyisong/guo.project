# @author Yisong Zhen
# @since  2017-10-08
# @update 2017-11-13
# see the original image file
# in the Guo's coorespondance
# raw data was sent by Guo in two seperated mails.
#---

# install.packages('metafor', repo = 'http://mirrors.ustc.edu.cn/CRAN/')
pkgs      <- c( 'metafor','tidyverse','magrittr','stringr','cowplot',
                'rgdal','sp', 'tmap')
load.libs <- lapply(pkgs, require, character.only = T)

guo.figures.path <- file.path('D:\\sourcecode\\guo.project\\figures')

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

args.plot.1 <- list(plot.1.results, mlab.param, mtext.param.1, mtext.param.2, mtext.param.3)
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.1) )

setwd(guo.figures.path)
jpeg('Figure3.jpg', width = 1200, height = 802, units = 'mm',res = 150)
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.1) )
dev.off()
"
#---
# Figure 2.
#---

plot.2.list.names <- c('B07_F', 'B07_M', 'DRB07_F', 'DRB07_M')
plot.2.results    <- f.all.results[plot.2.list.names]

mlab.param        <- list( B07_F   = 'HLA-B*07 Female Meta p-value=0.009', 
                           B07_M   = 'HLA-B*07 Male Meta p-value=1.50E-9',
                           DRB07_F = 'HLA-DRB1*07 Male Meta p-value=1.54E-10', 
                           DRB07_M = 'HLA-DRB1*07 Female Meta p-value=0.0559' )
mtext.param.1       <- list( B07_F   = 'HLA-B*07 Female ', 
                             B07_M   = 'HLA-B*07 Male ',
                             DRB07_F = 'HLA-DRB1*07 Male ', 
                             DRB07_M = 'HLA-DRB1*07 Female ' )
mtext.param.2       <- list( B07_F   = str_pad('p=0.4531', width = 25, side = 'left'), 
                             B07_M   = str_pad('p=0.2848', width = 25, side = 'left'),
                             DRB07_F = str_pad('p=0.3634', width = 25, side = 'left'), 
                             DRB07_M = str_pad('p=0.0407', width = 25, side = 'left') )
args.plot.1       <- list(plot.2.results, mlab.param, mtext.param.1, mtext.param.2)
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.1) )

#---
# Figure 3.
#---
plot.3.list.names <- c('B07_FM', 'DRB07_FM')
plot.3.results    <- f.all.results[plot.3.list.names]
lab.notation.2 <- c( 'Southwest Female', 'Northeast Female', 'North Female',
                     'East Female', 'Northwest Female', 'Central Female',
                     'South Female', 'Southwest Male', 'Northeast Male',
                     'North Male', 'East Male','Northwest Male',
                     'Central Male','South Male')
mlab.param        <- list( B07_FM   = 'Meta p-value=0.009', 
                           DRB07_FM = 'Meta p-value=0.0559')
mtext.param.1       <- list( B07_FM   = 'HLA-B*07 Female ', 
                             DRB07_FM = 'HLA-DRB1*07 Female ' )
mtext.param.2       <- list( B07_FM   = str_pad('p=0.4531', width = 25, side = 'left'), 
                             DRB07_FM = str_pad('p=0.0407', width = 25, side = 'left') )
args.plot.1       <- list(plot.3.results, mlab.param, mtext.param.1, mtext.param.2)
par(mfrow = c(2,1), mar = c(5,4,1,1))
invisible( pmap(args.plot.1, plot.img, lab.notation = lab.notation.2) )
"

#---
# Figure 4 boxplot
# this change to Figure 2.
# the above figures are deprecated.
#---

raw.data.path <- file.path('D:\\sourcecode\\guo.project\\update.data')


file.names <- list('DRB07_F.csv', 'DRB07_M.csv', 'B07_F.csv','B07_M.csv')

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
plot.1 <- boxplot.data %>%
          filter(groups == 1 | groups ==2) %>%
          ggplot(aes( x = groups, y = beta.point)) +
          geom_point( aes(fill = groups), , 
                      shape = 16 , size = 2, alpha = 0.5, show.legend = F)  +   
          stat_summary( fun.data = 'mean_sdl', fun.args = list(mult = 1),
                        geom = 'crossbar', color = 'red', width = .1) +
          #stat_summary(fun.y = mean, geom = 'point', color = 'red') +
          scale_x_discrete(labels = c('Female','Male')) +
          scale_y_continuous(limits = c(-0.1,0.18)) +
          geom_segment(aes(x = 1, y = 0.15, xend = 2, yend = 0.15) ) +
          geom_segment(aes(x = 1, y = 0.14, xend = 1, yend = 0.15) ) +
          geom_segment(aes(x = 2, y = 0.14, xend = 2, yend = 0.15) ) +
          geom_text( aes(x = 1.5, y = 0.16), 
                     label = B.07.pval, 
                     vjust = 'middle',
                     parse = T)  + 
          geom_text(aes(x = 1.5, y = 0.18),label = 'B * 07') +
          theme_classic() +
          ylab(label = 'Beta') +
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

plot.2 <- boxplot.data %>%
          filter(groups == 3 | groups == 4) %>%
          ggplot(aes( x = groups, y = beta.point)) +
          geom_boxplot(alpha = 0.5, outlier.shape = NA) +
          geom_point( aes(fill = groups), , 
                       shape = 16 , size = 1.4, show.legend = F)  +        
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
          theme( axis.text.x  = element_text( size = 14),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 14) ) +
          theme( aspect.ratio = 1)

figure4.boxplot <- plot_grid( plot.1, plot.2, labels = c('A','B'), 
                              ncol = 2, nrow = 1)
setwd(guo.figures.path)
ggsave( 'Figure4.jpeg', plot = figure4.boxplot, dpi = 600, units = 'mm')
