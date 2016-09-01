# the original protocol in Chinese
# http://blog.sina.com.cn/s/blog_6bc5205e0102vma9.html
# the map raw data source is downloaded 
# from and saved in dir

# suggested readings
# http://stackoverflow.com/questions/29773240/triple-legend-in-ggplot2-with-point-shape-fill-and-color

# install.packages("maptools")
library(maptools)
library(ggplot2)
library(dplyr)
library(plyr)


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

#ggplot(china.map.df, aes(x = long, y = lat, group = group, fill = factor(provclass))) +
#     geom_polygon(colour = "grey40") +
#     scale_fill_manual(name = 'Geographical regions',values = c('red','purple','black','blue',
#                                                           'green','yellow','brown','grey'), 
#                         labels = rev(c('','SouthWest','South','Central',
#                                      'East','NorthWest','North','NorthEast')),
#                         guide  = 'legend') +
#     guides(fill=guide_legend(ncol=2)) +
#     coord_map("polyconic")  +
#     theme( panel.grid = element_blank(),
#            panel.background = element_blank(),
#            axis.text = element_blank(),
#            axis.ticks = element_blank(),
#            axis.title = element_blank(),
#            legend.position = c(0.3,0.98),
#            legend.title.align = 0.5
#          )
#
