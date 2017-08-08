rm(list = ls())

library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(ggplot2)
library(readxl)
library(rJava)
library(xlsxjars)
library(xlsx)


getwd()
setwd("E:\\qianpai")
dir()

####################################################
xybasetotal <- fread('xybase.csv',encoding = 'UTF-8')
xydzdtotal <- fread('xydzd0807.csv',encoding = 'UTF-8')
jingdongzhifubao <- fread('支付宝京东.csv',encoding = 'UTF-8')
ywy <- fread('ywy.csv',encoding = 'UTF-8')

xybasetotal <- xybasetotal[,2:12]
jingdongzhifubao <- jingdongzhifubao[,2:13]

colnames(xydzdtotal) <- c("ajbh" ,"shfzh18" ,"kehu" ,"ajlx" ,"inpici", "hkrq",
                          "dzrq" ,"hkze", "hkmx" ,"huobi","rate" , "zhrmb", "hkbz" )
#####################################################

xydzdhkmx <- xydzdtotal %>% filter(hkbz==1)
xydzdhkze <- xydzdtotal %>% filter(hkbz==0)
unique(xydzdhkmx$kehu)

summary(xydzdhkmx)
xydzd1 <- xydzdhkmx %>% filter(hkmx>=0)


data2 <- jingdongzhifubao %>% filter(kehu=='京东'&pici=='B0')
data2 <- data2 %>% select(ajbh,branch,kehu,ajlx,pici,inpici,shfzh18,zjqkje,zjshje,jdsj,dqsj)

colnames(data2) <- c('bh','branch','kehu','ajlx','pici','inpici','shfzh18','zjqkje','zjshje','jdsj','dqsj')



xybase <- rbind(xybasetotal,data2)


bind1 <- xybase %>% select(bh,shfzh18,kehu,ajlx,inpici,pici,zjqkje)

xydzd <- left_join(xydzdhkmx,bind1,by=c('ajbh'='bh','shfzh18'='shfzh18','kehu'='kehu','ajlx'='ajlx',
                                        'inpici'='inpici'))

xydzd <- na.omit(xydzd)

xydzd$year <- year(xydzd$hkrq)
xydzd$month <- month(xydzd$hkrq)
xydzd1 <- xydzd %>% group_by(year,kehu,month) %>% summarise(sumhkje=sum(hkmx)) %>% arrange(kehu)

test1 <- xydzd1 %>% group_by(kehu) %>% tally()

#京东客户####################################################
xydzd1_3 <- xydzd1 %>% filter(kehu == '京东')

xydzd1_3$year <- as.factor(xydzd1_3$year)

xydzd1_3 %>%
  ggplot(., aes(x = month, y = sumhkje, colour = factor(year))) +
  geom_bar(stat="identity",position="dodge") +
  scale_x_continuous(breaks=1:12) +
  facet_grid(year~ .)+
  ggtitle("京东客户每月回款情况")+
  theme(plot.title = element_text(hjust=0.5),legend.position = "none")+
  xlab("月份")+
  ylab("每月回款金额")





###8-10个月#####################################################3
xydzd1$copy <- paste0(xydzd1$year,"年",xydzd1$month)
xydzd1_4 <- xydzd1 %>% filter(kehu %in% c("拉卡拉","马上金融","亨元金融","众安保险-智联"))
ggplot(xydzd1_4,aes(copy,sumhkje,colour=factor(kehu),shape=factor(kehu)))+
  geom_point(size=4)+
  theme(axis.text.x = element_blank())+
  xlab('2015年-2017年每月回款情况')+
  ylab('回款金额')+
  ggtitle("回款8-10个月客户")+
  theme(plot.title = element_text(hjust=0.5))


###7个月#####################################################3

xydzd1_5 <- xydzd1 %>% filter(kehu %in% c("唯品会",
                                          "众安保险","众安保险-速贷宝","众安保险-小确信",
                                          "众安保险-员工贷"))
ggplot(xydzd1_5,aes(copy,sumhkje,colour=factor(kehu),shape=factor(kehu)))+
  geom_point(size=3)+
  theme(axis.text.x = element_blank())+
  xlab('2015年-2017年每月回款情况')+
  ylab('回款金额')+
  ggtitle("回款7个月客户")+
  theme(plot.title = element_text(hjust=0.5))
###7个月#####################################################3

# xydzd1_6 <- xydzd1 %>% filter(kehu %in% c("众安保险-小确信",
#                                           "唯品会","众安保险","众安保险-速贷宝",
#                                           "众安保险-员工贷"))
# ggplot(xydzd1_6,aes(copy,sumhkje,colour=factor(kehu)))+
#   geom_point(size=3)+
#   theme(axis.text.x = element_blank())+
#   xlab('2014年-2017年每月回款情况')+
#   ylab('回款金额')+
#   ggtitle("回款7个月客户")+
#   theme(plot.title = element_text(hjust=0.5))
###5个月一下#####################################################3

xydzd1_7 <- xydzd1 %>% filter(kehu %in% c("上海银行",
                                          "中信","和谐小贷","招商",
                                          "兴业消金","众安保险-赶集","国美",
                                          "众安保险-随心贷","去哪儿"))
ggplot(xydzd1_7,aes(copy,sumhkje,colour=factor(kehu)))+
  geom_point(size=3)+
  theme(axis.text.x = element_blank())+
  xlab('2015年-2017年每月回款情况')+
  ylab('回款金额')+
  ggtitle("回款5个月以下客户")+
  theme(plot.title = element_text(hjust=0.5))
##########################
xybase1 <-xybase

xybase1$year <- year(xybase1$jdsj)
xybase1$month <- month(xybase1$jdsj)
xybase1 <- xybase1 %>% select(bh,kehu,jdsj,year,month,zjqkje,zjshje)
xybase1 <- distinct(xybase1)

xybase1$num <- 1

xybase1$hkje <- ifelse(xybase1$zjqkje>=xybase1$zjshje,xybase1$zjshje,xybase1$zjqkje)
test2 <- xybase1 %>% group_by(year,kehu,month) %>% summarise(sumqkje=sum(zjqkje),sumshje=sum(hkje),sumnum=sum(num)) %>%
  arrange(kehu)


test2$hshl <- test2$sumshje/test2$sumqkje
str(xybase1)
testtotal <- test2
###########################################################
data2$hkje <- ifelse(data2$zjqkje>=data2$zjshje,data2$zjshje,data2$zjqkje)
data2$year <- year(data2$jdsj)
data2$month <- month(data2$jdsj)
data2$num <-1
jingdong <- data2 %>% group_by(year,kehu,month) %>% summarise(sumqkje=sum(zjqkje),sumshje=sum(hkje),sumnum=sum(num)) %>%
  arrange(kehu)
jingdong$hshl <- jingdong$sumshje/jingdong$sumqkje
jingdong %>%
  group_by(kehu,year,month) %>%
  summarise(totalhshl=sum(hshl)) %>%
  ggplot(.,aes(x=month,y=totalhshl,fill=factor(year)))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks = 1:12)+
  facet_grid(kehu~.)+
  xlab("月份")+
  ylab("回收率")+
  ggtitle("客户回收率统计")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=round(totalhshl,3)),vjust=-0.2)
#########################################################3


hshl0 <- testtotal %>% filter(hshl==0)
hshl1 <- testtotal %>% filter(hshl!=0)
unique(hshl0$kehu)
test11 <- hshl1 %>% group_by(kehu) %>% tally()

hshltotal2 <- testtotal %>% filter(kehu %in% c('众安保险-速贷宝','亨元金融','众安保险-小确信','众安保险-员工贷',
                                               '众安保险-智联'))


hshltotal2 %>%
  group_by(kehu,year,month) %>%
  summarise(totalhshl=sum(hshl)) %>%
  ggplot(.,aes(x=month,y=totalhshl,colour=factor(year)))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  facet_grid(kehu~.)+
  xlab("月份")+
  ylab("回收率")+
  ggtitle("客户回收率统计")+
  theme(plot.title = element_text(hjust = 0.5))
#####################################################

hshltotal3 <- testtotal %>% filter(kehu %in% c('拉卡拉','马上金融','众安保险','京东','唯品会','中信'))
hshltotal3 %>%
  group_by(kehu,year,month) %>%
  summarise(totalhshl=sum(hshl)) %>%
  ggplot(.,aes(x=month,y=totalhshl,fill=factor(year)))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks = 1:12)+
  facet_grid(kehu~.)+
  xlab("月份")+
  ylab("回收率")+
  ggtitle("客户回收率统计")+
  theme(plot.title = element_text(hjust = 0.5))

#####################################################

hshltotal4 <- testtotal %>% filter(!(kehu %in% c('拉卡拉','马上金融','众安保险','京东','唯品会','中信',
                                                 '众安保险-速贷宝','亨元金融','众安保险-小确信','众安保险-员工贷',
                                                 '众安保险-智联')))
hshltotal4 <- hshltotal4 %>% filter(hshl!=0)
hshltotal4$num <-1
hshltotal44 <- hshltotal4 %>% group_by(kehu) %>% summarise(sumhshl=sum(hshl),sumn=sum(num),pjhshl=sumhshl/sumn)
hshltotal44 %>%
  ggplot(.,aes(x=kehu,y=pjhshl,fill=factor(kehu)))+
  geom_bar(stat="identity",position="dodge")+
  xlab("客户")+
  ylab("回收率")+
  ggtitle("客户平均月回收率")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=round(pjhshl,3)),vjust=-0.2)+
  theme(legend.position = "none")


############################################################

rrr <- left_join(ywy,xybase1,by=c('ajbh'='bh'))


rrr <- na.omit(rrr)

rrr1 <- rrr %>% select(kehu,year,czy)
rrr1 <- distinct(rrr1)
rrr1 <- rrr1 %>% group_by(kehu,year) %>% tally()
totalhkje <- testtotal %>% group_by(kehu,year) %>% summarise(sumhkje=sum(sumshje))

totalhkje <- totalhkje %>% filter(sumhkje!=0)
rrchch <- left_join(totalhkje,rrr1,by=c('kehu'='kehu','year'='year'))

rrchch$rjcc <- rrchch$sumhkje/rrchch$n



newrjcc1_1 <- rrchch %>% filter(kehu %in% c('众安保险','唯品会','亨元金融','上海银行'))
newrjcc1_1$year <- as.factor(newrjcc1_1$year)

ggplot(newrjcc1_1,aes(kehu,rjcc,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("客户人均产出10万以上")+
  theme(axis.title = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



newrjcc1_2 <- rrchch %>% filter(kehu %in% c('众安保险-智联','众安保险-员工贷','京东','兴业消金',
                                            '拉卡拉','去哪儿','众安保险-速贷宝',
                                            '中信','众安保险-小确信','众安保险-随心贷'))

newrjcc1_2$year <- as.factor(newrjcc1_2$year)

ggplot(newrjcc1_2,aes(kehu,rjcc,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("客户人均产出1万到10万")+
  theme(axis.title = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size = 9,vjust = 0.5, hjust = 0.5, angle = 20))



newrjcc1_3 <- rrchch %>% filter(kehu %in% c('国美','招商','和谐小贷','众安保险-赶集','马上金融'))

newrjcc1_3$year <- as.factor(newrjcc1_3$year)

ggplot(newrjcc1_3,aes(kehu,rjcc,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("客户人均产出1万以下")+
  theme(axis.title = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))


################################################################33
rrr2 <- rrr %>% select(kehu,year,month,czy)
rrr2 <- distinct(rrr2)
rrr2 <- rrr2 %>% group_by(kehu,year,month) %>% tally()

testtotal1 <- left_join(testtotal,rrr2,by=c('kehu'='kehu','year'='year','month'='month'))

testtotal1 <- testtotal1 %>% filter(sumshje!=0)

testtotal1$num <- 1

kehufawu <- testtotal1 %>% group_by(kehu) %>% summarise(sumhsh=sum(sumnum),sumn=sum(n),
                                                        nnn=sum(num))

kehufawu$pjhsh <- round(kehufawu$sumhsh/kehufawu$nnn)
kehufawu$pjfw <- round(kehufawu$sumn/kehufawu$nnn)


kehufawu1 <- kehufawu %>% filter(kehu %in% c('众安保险','唯品会','京东','亨元金融','上海银行','中信','马上金融'))
kehufawu2 <- kehufawu %>% filter(kehu %in% c('众安保险-智联','众安保险-小确信','和谐小贷','去哪儿','国美','拉卡拉'))


kehufawu100 <- kehufawu%>% filter(pjhsh<50) %>% select(kehu)
print (kehufawu100$kehu)


library(plotrix)

y1_1<- kehufawu1$pjhsh
y1_2 <- kehufawu1$pjfw
xpos1 <- 1:7
x1 <- kehufawu1$kehu
twoord.plot(xpos1,y1_1,xpos1,y1_2,lcol = 4,rcol = 2,lylim=c(300,11000),rylim=c(0,200),xlab = "客户",ylab = "平均每月新委案户数",rylab = "平均每月操作法务数",type=c("bar","b"),
            xticklab=x1,halfwidth=0.2,main="平均每月新委案户数500-11000与平均每月操作法务数")


y2_1<- kehufawu2$pjhsh
y2_2 <- kehufawu2$pjfw
xpos2 <- 1:6
x2 <- kehufawu2$kehu
twoord.plot(xpos2,y2_1,xpos2,y2_2,lcol = 4,rcol = 2,lylim=c(40,300),rylim=c(0,20),xlab = "客户",ylab = "平均每月新委案户数",rylab = "平均每月操作法务数",type=c("bar","b"),
            xticklab=x2,halfwidth=0.2,main="平均每月新委案户数50-500与平均每月操作法务数")


