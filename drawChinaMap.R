# @author Yisong Zhen
# @since  2017-09-30
# @update 2017-01-01
# the original protocol in Chinese
# http://blog.sina.com.cn/s/blog_6bc5205e0102vma9.html
# the map raw data source is downloaded 
# from and saved in dir
#---

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

pkgs <- c('tidyverse', 'rgdal','sp', 'tmap')
load.lib <- lapply(pkgs, require, character.only = TRUE) 


rawmap.data.path <- file.path('E:\\FuWai\\guo.data\\rawdata\\') %>%
                    paste('ChinaMap-2\\', sep = '') %>%
                    paste('全国省级、地市级、县市级行政区划shp\\', sep = '') %>%
                    paste('省界bou2_4m', sep = '')

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

chinamap.rawdata@data$regionClass  <- factor( prov.group, 
                                              labels = c('Northeast','North','Northwest',
                                                          'East', 'Central','South','Southwest', 'Excluded'))

china.tmap <- tm_shape(shp = chinamap.rawdata, projection = 'hd') +
              tm_borders(col = 'burlywood4') +
              tm_fill(col = 'regionClass', title = 'sample collection regions') +
              tm_compass() +
              tm_scale_bar() +
              tm_layout(frame = F)
             



"
 tm_legend( text.size = 1,
                         title.size = 1.2,
                         position = c('left','bottom'), 
                         bg.color = 'white', 
                         bg.alpha = .2, 
                         frame    = 'gray50', 
                         height   =.6, 
                         hist.width   = .2,
                         hist.height  = .2, 
                         hist.bg.color = 'gray60', 
                         hist.bg.alpha = .5)
"
# I tried, but failed.
#---
"
china.tmap <- tm_shape(shp = chinamap.rawdata, projection = 'hd') +
             tm_polygons()
"

