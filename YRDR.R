library(maps)
library(mapdata)
library(maptools)
library(ggplot2)
library(ggrepel)
setwd("G:/")
data<-read.delim("Pi+Ni.txt")
dat<-read.delim("Pi-Ni.txt")
da<-read.delim("Ni.txt")
d<-read.delim("Pi.txt")
data1<-matrix(ncol=6,nrow=1)
colnames(data1)<-c("Name","log","lat","con","H","group")
b<-26:1
a<-1:26
n<-t(combn(b,2))
m <- t(combn(a,2))
for (i in 1:dim(m)[1]) {
  H<-data[m[i,1],5]
  group<-i
  data1<-rbind(data1,cbind(data[m[i,1],],group),cbind(data[m[i,2],1:4],H,group))
  
}
i<-1
for (i in 1:dim(n)[1]) {
  
  H<-data[n[i,1],5]
  group<-i+325
  data1<-rbind(data1,cbind(data[n[i,1],],group),cbind(data[n[i,2],1:4],H,group))
  
}
data1<-data1[-1,]

setwd("G:/H/")
A<-readShapePoly('bou2_4p.shp')
map(A,region = c('上海市', '浙江省','江苏省','安徽省'))
library(gpclib)
gpclibPermit()
tract <- fortify(A, region = "NAME")
table(iconv(A$NAME, from = "GBK"))
B<-tract[which(tract$id==c("上海市","浙江省","江苏省","安徽省")),]
value<-data1$H
ggplot() + geom_path(data = B, aes(long, lat, group = group), color = 'black', show.legend = F)+
  geom_line(data = data1, aes(log,lat,group=group,color=factor(H)),alpha = 0.8,size=value,show_guide = FALSE)+
  scale_color_manual(values=c('#A9CCE3','#A569BD','#F4D03F'))+
  geom_point(data = data, aes(x = log, y = lat,size =con), alpha = 0.8, color = 'red',show_guide = FALSE)+
  geom_text_repel(data = data, aes(x = log, y = lat, label = Name), family = "STHeiti")+
  coord_map()+
  annotate(geom = "text",x = 115.5, y = 29, hjust = 0,label="Legend")+
  annotate(geom = "point",x = 115.6,y = 28.5,colour = "red",size = 3)+
  annotate(geom = "text",x = 115.8,y = 28.5,hjust = 0,label="City")+
  annotate(geom = "text",x = 115.5,y = 28,hjust = 0,label="Pi+Ni")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.6, yend = 27.6,colour = "#A9CCE3",size = 0.)+
  annotate(geom = "text",x = 116,y = 27.6,hjust = 0,label="0 - 4")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.3, yend = 27.3,colour = "#A569BD",size = 1)+
  annotate(geom = "text",x = 116,y = 27.3,hjust = 0,label="4 - 10")+
  annotate("segment", x = 115.5, xend =115.9, y = 27, yend = 27,colour = "#F4D03F",size = 2)+
  annotate(geom = "text",x = 116,y = 27,hjust = 0,label="10 - 60")+
  theme_set(theme_bw()) +
  theme(legend.position = "bottom",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA))  +
  xlab("") + ylab("") + labs(title = "Yangtze River Delta Urban Agglomeration")
###########################################
dat1<-matrix(ncol=6,nrow=1)
colnames(dat1)<-c("Name","log","lat","con","H","group")
b<-26:1
a<-1:26
n<-t(combn(b,2))
m <- t(combn(a,2))
for (i in 1:dim(m)[1]) {
  H<-dat[m[i,1],5]
  group<-i
  dat1<-rbind(dat1,cbind(dat[m[i,1],],group),cbind(dat[m[i,2],1:4],H,group))
  
}
i<-1
for (i in 1:dim(n)[1]) {
  
  H<-dat[n[i,1],5]
  group<-i+325
  dat1<-rbind(dat1,cbind(dat[n[i,1],],group),cbind(dat[n[i,2],1:4],H,group))
  
}
dat1<-dat1[-1,]
value<-dat1$H
ggplot() + geom_path(data = B, aes(long, lat, group = group), color = 'black', show.legend = F)+
  geom_line(data = dat1, aes(log,lat,group=group,color=factor(H)),size=value,alpha = 0.8,show_guide = FALSE)+
  scale_color_manual(values=c('#A9CCE3','#A569BD','#F4D03F'))+
  geom_point(data = dat, aes(x = log, y = lat,size =con), alpha = 0.8, color = 'red',show_guide = FALSE)+
  geom_text_repel(data = dat, aes(x = log, y = lat, label = Name), family = "STHeiti")+
  coord_map()+
  annotate(geom = "text",x = 115.5, y = 29, hjust = 0,label="Legend")+
  annotate(geom = "point",x = 115.6,y = 28.5,colour = "red",size = 3)+
  annotate(geom = "text",x = 115.8,y = 28.5,hjust = 0,label="City")+
  annotate(geom = "text",x = 115.5,y = 28,hjust = 0,label="Pi-Ni")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.6, yend = 27.6,colour = "#A9CCE3",size = 0.)+
  annotate(geom = "text",x = 116,y = 27.6,hjust = 0,label="-6.357 - -2")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.3, yend = 27.3,colour = "#A569BD",size = 1)+
  annotate(geom = "text",x = 116,y = 27.3,hjust = 0,label="-2 - 0")+
  annotate("segment", x = 115.5, xend =115.9, y = 27, yend = 27,colour = "#F4D03F",size = 2)+
  annotate(geom = "text",x = 116,y = 27,hjust = 0,label="0 - 30")+
  theme_set(theme_bw()) +
  theme(legend.position = "bottom",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA))  +
  xlab("") + ylab("") + labs(title = "Yangtze River Delta Urban Agglomeration")
################################
da1<-matrix(ncol=6,nrow=1)
colnames(da1)<-c("Name","log","lat","con","H","group")
b<-26:1
a<-1:26
n<-t(combn(b,2))
m <- t(combn(a,2))
for (i in 1:dim(m)[1]) {
  H<-da[m[i,1],5]
  group<-i
  da1<-rbind(da1,cbind(da[m[i,1],],group),cbind(da[m[i,2],1:4],H,group))
  
}
i<-1
for (i in 1:dim(n)[1]) {
  
  H<-da[n[i,1],5]
  group<-i+325
  da1<-rbind(da1,cbind(da[n[i,1],],group),cbind(da[n[i,2],1:4],H,group))
  
}
da1<-da1[-1,]
value<-da1$H
ggplot() + geom_path(data = B, aes(long, lat, group = group), color = 'black', show.legend = F)+
  geom_line(data = da1, aes(log,lat,group=group,color=factor(H)),size=value,alpha = 0.8,show_guide = FALSE)+
  scale_color_manual(values=c('#A9CCE3','#A569BD','#F4D03F'))+
  geom_point(data = da, aes(x = log, y = lat,size =con), alpha = 0.8, color = 'red',show_guide = FALSE)+
  geom_text_repel(data = da, aes(x = log, y = lat, label = Name), family = "STHeiti")+
  coord_map()+
  annotate(geom = "text",x = 115.5, y = 29, hjust = 0,label="Legend")+
  annotate(geom = "point",x = 115.6,y = 28.5,colour = "red",size = 3)+
  annotate(geom = "text",x = 115.8,y = 28.5,hjust = 0,label="City")+
  annotate(geom = "text",x = 115.5,y = 28,hjust = 0,label="Ni")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.6, yend = 27.6,colour = "#A9CCE3",size = 0.)+
  annotate(geom = "text",x = 116,y = 27.6,hjust = 0,label="0.526 - 4")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.3, yend = 27.3,colour = "#A569BD",size = 1)+
  annotate(geom = "text",x = 116,y = 27.3,hjust = 0,label="4 - 10")+
  annotate("segment", x = 115.5, xend =115.9, y = 27, yend = 27,colour = "#F4D03F",size = 2)+
  annotate(geom = "text",x = 116,y = 27,hjust = 0,label="10 - 30")+
  theme_set(theme_bw()) +
  theme(legend.position = "bottom",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA))  +
  xlab("") + ylab("") + labs(title = "Yangtze River Delta Urban Agglomeration")
#########################################
d1<-matrix(ncol=6,nrow=1)
colnames(d1)<-c("Name","log","lat","con","H","group")
b<-26:1
a<-1:26
n<-t(combn(b,2))
m <- t(combn(a,2))
for (i in 1:dim(m)[1]) {
  H<-d[m[i,1],5]
  group<-i
  d1<-rbind(d1,cbind(d[m[i,1],],group),cbind(d[m[i,2],1:4],H,group))
  
}
i<-1
for (i in 1:dim(n)[1]) {
  
  H<-d[n[i,1],5]
  group<-i+325
  d1<-rbind(d1,cbind(d[n[i,1],],group),cbind(d[n[i,2],1:4],H,group))
  
}
d1<-d1[-1,]
value<-d1$H
ggplot() + geom_path(data = B, aes(long, lat, group = group), color = 'black', show.legend = F)+
  geom_line(data = d1, aes(log,lat,group=group,color=factor(H)),size=value,alpha = 0.8,show_guide = FALSE)+
  scale_color_manual(values=c('#A9CCE3','#A569BD','#F4D03F'))+
  geom_point(data = d, aes(x = log, y = lat,size =con), alpha = 0.8, color = 'red',show_guide = FALSE)+
  geom_text_repel(data = d, aes(x = log, y = lat, label = Name), family = "STHeiti")+
  coord_map()+
  annotate(geom = "text",x = 115.5, y = 29, hjust = 0,label="Legend")+
  annotate(geom = "point",x = 115.6,y = 28.5,colour = "red",size = 3)+
  annotate(geom = "text",x = 115.8,y = 28.5,hjust = 0,label="City")+
  annotate(geom = "text",x = 115.5,y = 28,hjust = 0,label="Pi")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.6, yend = 27.6,colour = "#A9CCE3",size = 0.)+
  annotate(geom = "text",x = 116,y = 27.6,hjust = 0,label="0.121 - 2")+
  annotate("segment", x = 115.5, xend =115.9, y = 27.3, yend = 27.3,colour = "#A569BD",size = 1)+
  annotate(geom = "text",x = 116,y = 27.3,hjust = 0,label="2 - 10")+
  annotate("segment", x = 115.5, xend =115.9, y = 27, yend = 27,colour = "#F4D03F",size = 2)+
  annotate(geom = "text",x = 116,y = 27,hjust = 0,label="10 - 40")+
  theme_set(theme_bw()) +
  theme(legend.position = "bottom",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA))  +
  xlab("") + ylab("") + labs(title = "Yangtze River Delta Urban Agglomeration")
