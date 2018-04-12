################################################################
# First things first ###########################################
################################################################

setwd('/home/roel-peters/data_visualization')
options(scipen=999)
gc()

if (!require(data.table)) { install.packages('data.table') }
if (!require(ggplot2)) { install.packages('ggplot2') }
if (!require(magrittr)) { install.packages('magrittr') }
if (!require(ggridges)) { install.packages('ggridges') }
if (!require(scales)) { install.packages('scales') }
if (!require(stringr)) { install.packages('stringr') }
if (!require(ggmap)) { install.packages('ggmap') }

################################################################
# Data prep ####################################################
################################################################

# Housing data set
dt <- data.table(read.csv('dataset.csv',dec=',', sep= ';')) # No fread, problem with decimal
dt <- dt[CD_PERIOD == 'Y']
dt <- dt[,c('CD_PERIOD','CD_REFNIS_FR','CD_TYPE_FR', 'CD_CLASS_SURFACE') := NULL]

dt$CD_TYPE_NL %<>% 
  gsub('gewone woonhuizen','houses',.) %>%
  gsub("villa's bungalows, landhuizen", 'mansions',.) %>%
  gsub("appartementen, flats, studio's", 'flats',.) %>%
  gsub('bouwgronden', 'land',.)

dt <- dt[CD_TYPE_NL != 'land']
dt_gent <- dt[CD_REFNIS_NL == 'GENT']

# NIS to Postal Code converter
nis <- fread('nis.csv')
nis <- nis[,.(postcode,nis)]

# Geodata
latlong <- fread('latlong.csv')
latlong <- latlong[,.(`Postal Code`,Latitude,Longitude)]

# Employment data
empl <- fread('employment.csv')
empl <- empl[jaar > 2009]
empl <- empl[,gemeente := NULL]
empl <- empl[,werkzaamheidsgraad := as.numeric(gsub('\\,','\\.',werkzaamheidsgraad))]

# Population data
pop <- fread('population.csv')
pop <- pop[,gemeente := NULL]
################################################################
# Theme ########################################################
################################################################

t <- theme(plot.title = element_text(face="bold", margin=margin(t = 15, r = 0, b = 15, l = 0, unit = "pt")),
           axis.text.x = element_text(size=10,color='#000000',angle=45,hjust=1),
           axis.text.y = element_text(size=10,color='#000000'),
           axis.title.x = element_text(face="bold", size=10,color='#000000',margin=margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
           axis.title.y = element_text(face="bold", size=10,color='#000000',margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dashed',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='dashed', size=0),
           legend.text = element_text(size=10,color='#000000'),
           legend.title = element_text(face='bold',size=10,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=10,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#E02128','#1B2C3F','#2672A4','#43A39E','#EB9E0F','#333745','#000000')

################################################################
# First Plot ###################################################
################################################################

dt1 <- dt_gent[,.(mean_price = round(sum(MS_TOTAL_PRICE) / sum(MS_TOTAL_TRANSACTIONS)/1000),transactions=sum(MS_TOTAL_TRANSACTIONS)),by=.(CD_YEAR)]
g1 <- ggplot(dt1,aes(x=CD_YEAR,y=mean_price,label=transactions)) + 
  geom_line(col=pal[1]) + 
  geom_point(aes(size=transactions), col=pal[1]) + 
  geom_text(vjust=3.5,hjust=0.5,size=2.5) +
  labs(y='mean price (1000)',x='',title='Plot 1: Mean property price and transactions in Ghent (2010-2016)') +
  scale_x_continuous(limits=c(2009,2017),breaks=seq(2010,2016)) +
  scale_y_continuous(limits=c(200,280),breaks=seq(200,280,10)) +
  scale_radius(range=c(3,10),name='properties sold') +
  t
g1  
rm(dt1)

################################################################
# Second Plot ##################################################
################################################################

dt2 <- dt_gent[,.(mean_price = MS_MEAN_PRICE/1000),by=.(CD_YEAR,CD_TYPE_NL)]
g2 <- ggplot(dt2,aes(x=CD_YEAR,y=mean_price,color=CD_TYPE_NL,group=CD_TYPE_NL)) + 
  geom_line() +
  geom_point() +
  labs(y='mean price (1000)',x='', title='Plot 2: Mean property price per type (2010-2016)') +
  scale_color_manual(values=pal,name='') +
  scale_x_continuous(limits=c(2009,2017),breaks=seq(2010,2016)) +
  scale_y_continuous(limits=c(180,420),breaks=seq(180,420,40)) +
  t
g2
rm(dt2)

################################################################
# Third Plot ###################################################
################################################################

dt3 <- melt.data.table(dt_gent,id.vars=c('CD_YEAR','CD_TYPE_NL'),measure.vars=c('MS_P10','MS_P25','MS_P50','MS_P75','MS_P90'))
dt3 <- dt3[CD_YEAR %in% c(2010,2016)]

dt3 <- dt3[,legend := variable]
dt3$variable %<>% 
  gsub('MS_P10',0.10,.) %>%
  gsub('MS_P25', 0.25,.) %>%
  gsub('MS_P50', 0.5,.) %>%
  gsub('MS_P75', 0.25,.) %>%
  gsub('MS_P90',0.1,.)
dt3$variable <- as.numeric(dt3$variable)

dt3 <- dt3[,value := value/1000]

dt3$legend %<>% 
  gsub('MS_P10',0.10,.) %>%
  gsub('MS_P25', 0.25,.) %>%
  gsub('MS_P50', 0.5,.) %>%
  gsub('MS_P75', 0.75,.) %>%
  gsub('MS_P90',0.90,.)
dt3$legend <- as.numeric(dt3$variable)
dt3 <- dt3[,legend := paste0('p',as.character(legend * 100))]

g3 <- ggplot(dt3,aes(x=value,y=variable,label=legend, group=as.factor(CD_YEAR))) + 
  geom_area(aes(fill=as.factor(CD_YEAR)),alpha=0.3) +
  geom_line(aes(col=as.factor(CD_YEAR)),alpha=0.8) +
  geom_point(aes(col=as.factor(CD_YEAR))) +
  geom_text(vjust=-1.5,hjust=0.2,size=2.5) +
  facet_grid(CD_TYPE_NL~.) +
  t +
  labs(x='mean price (1000)',y='',title='Plot 4: Mean property price per type and segment (2010 vs 2016)') +
  scale_x_continuous(limits=c(80,620),breaks=seq(80,620,100)) +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2)) +
  scale_fill_manual(values=pal, name='year') +
  scale_color_manual(values=pal,name='year')
g3

rm(dt3)

################################################################
# Fourth Plot ##################################################
################################################################

dt4 <- melt.data.table(dt_gent,id.vars=c('CD_YEAR','CD_TYPE_NL'),measure.vars=c('MS_P10','MS_P25','MS_P50','MS_P75','MS_P90'))
dt4 <- dt4[order(CD_TYPE_NL,variable,CD_YEAR)]
dt4$growth <- with(dt4, ave(value, CD_TYPE_NL,variable,FUN=function(x) c(x / head(x, 1)*100)))

dt4$variable %<>% 
  gsub('MS_P10',0.10,.) %>%
  gsub('MS_P25', 0.25,.) %>%
  gsub('MS_P50', 0.5,.) %>%
  gsub('MS_P75', 0.75,.) %>%
  gsub('MS_P90',0.90,.)
dt4$variable <- as.numeric(dt4$variable)
dt4 <- dt4[,variable := paste0('p',as.character(variable * 100))]

g4 <- ggplot(dt4,aes(x=CD_YEAR,y=growth,group=variable,col=variable)) + 
  geom_line() +
  geom_point() +
  facet_grid(.~CD_TYPE_NL) +
  t +
  scale_color_manual(values=pal,name='segment') +
  scale_x_continuous(limits=c(2009,2017),breaks=seq(2010,2016)) +
  scale_y_continuous(limits=c(90,170),breaks=seq(90,170,10)) +
  labs(y='cumulative growth',x='', title='Plot 5: Cumulative mean price growth per segment (2010-2016)')
g4
rm(dt4)

################################################################
# Fifth Plot ###################################################
################################################################

dt5 <- melt.data.table(dt,id.vars=c('CD_YEAR','CD_TYPE_NL','CD_REFNIS_NL'),measure.vars=c('MS_P10','MS_P25','MS_P50','MS_P75','MS_P90'))
dt5 <- dt5[order(CD_TYPE_NL,variable,CD_YEAR)]
dt5$growth <- with(dt5, ave(value, CD_TYPE_NL,variable,CD_REFNIS_NL,FUN=function(x) x / head(x, 1)*100))

dt5$variable %<>% 
  gsub('MS_P10',0.10,.) %>%
  gsub('MS_P25', 0.25,.) %>%
  gsub('MS_P50', 0.5,.) %>%
  gsub('MS_P75', 0.75,.) %>%
  gsub('MS_P90',0.90,.)
dt5$variable <- as.numeric(dt5$variable)
dt5 <- dt5[,variable := paste0('p',as.character(variable * 100))]

dt5 <- dt5[CD_REFNIS_NL %in% c('GENT','ANTWERPEN','BRUSSEL','CHARLEROI','LUIK')]
dt5$CD_REFNIS_NL <- str_to_title(dt5$CD_REFNIS_NL)

dt5 <- dt5[CD_TYPE_NL == 'flats']

g5 <- ggplot(dt5,aes(x=CD_YEAR,y=growth,group=variable,col=variable)) + 
  geom_line() +
  geom_point() +
  facet_grid(.~CD_REFNIS_NL) +
  t +
  scale_color_manual(values=pal,name='segment') +
  scale_x_continuous(limits=c(2009,2017),breaks=seq(2010,2016)) +
  scale_y_continuous(limits=c(80,170),breaks=seq(80,170,10)) +
  labs(y='cumulative growth',x='',title='Plot 6: Cumulative mean flat price growth per segment in Belgian cities (2010-2016)')
g5
rm(dt5)

################################################################
# Sixth Plot ###################################################
################################################################

dt6 <- dt_gent

g6 <- ggplot(dt6,aes(x=CD_YEAR,y=MS_TOTAL_TRANSACTIONS,fill=CD_TYPE_NL)) +
  geom_bar(stat='identity',position='fill') +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=pal,name='') +
  scale_x_continuous(limits=c(2009,2017),breaks=seq(2010,2016)) +
  labs(x='',y='',title='Plot 7: Transaction share per property type in Ghent (2010-2016)') +
  t
g6

rm(dt6)
################################################################
# Seventh Plot #################################################
################################################################

dt7 <- dt[CD_REFNIS_NL %in% c('BRUSSEL','ANTWERPEN','GENT','CHARLEROI','LUIK')]
dt7 <- dt7[CD_YEAR %in% c(2010,2016)]
dt7 <- dt7[,MS_MEAN_PRICE := MS_MEAN_PRICE / 1000]
dt7 <- dt7[order(CD_TYPE_NL,CD_REFNIS_NL,CD_YEAR)]

dt7$growth <- with(dt7, ave(MS_MEAN_PRICE, CD_TYPE_NL,CD_REFNIS_NL,FUN=function(x) round(x / head(x, 1)*100)-100))
dt7$growth <- sprintf("%+d", dt7$growth)
dt7$growth <- paste0(dt7$growth,'%')
dt7$growth <- gsub('^(\\+0|NA)\\%$',NA,dt7$growth)

dt7$CD_REFNIS_NL <- str_to_title(dt7$CD_REFNIS_NL)

g7 <- ggplot(dt7,aes(x=MS_MEAN_PRICE,y=CD_REFNIS_NL,group=CD_REFNIS_NL, label=growth)) +
  geom_line(col='#a5a5a5') +
  geom_point(size=2.5,aes(col=as.factor(CD_YEAR))) +
  geom_text(size=2,vjust=2.1) +
  facet_grid(~CD_TYPE_NL) +
  scale_color_manual(values=pal,name='') +
  labs(y='',x='mean price (1000)',title='Plot 3: Mean price growth per property type in Belgian cities (2010 vs 2016)') +
  t
g7
rm(dt7)

################################################################
# Eigth Plot ###################################################
################################################################

dt8 <- dt[CD_TYPE_NL %in% c('houses')]
dt8 <- dt8[,.(MS_TOTAL_TRANSACTIONS=sum(MS_TOTAL_TRANSACTIONS),MS_TOTAL_PRICE=sum(MS_TOTAL_PRICE)),by=.(CD_YEAR,CD_REFNIS,CD_REFNIS_NL)]
dt8 <- dt8[order(CD_YEAR,-MS_TOTAL_TRANSACTIONS)]
dt8 <- dt8[,mean_price := round(MS_TOTAL_PRICE / MS_TOTAL_TRANSACTIONS/1000)]

nis <- nis[order(-postcode)]
nis <- nis[,.(postcode=head(postcode,1)),by=.(nis)]

dt8 <- merge(dt8,nis,by.x=('CD_REFNIS'),by.y='nis',all.x=T)
dt8 <- merge(dt8,latlong,by.x='postcode',by.y='Postal Code')
dt8 <- dt8[CD_YEAR %in% c(2010,2016)]
dt8$growth <- with(dt8, ave(mean_price,CD_REFNIS,FUN=function(x) x / head(x, 1)*100-100))
dt8$diff <- with(dt8, ave(mean_price,CD_REFNIS,FUN=function(x) x - head(x, 1)))
dt8$start <- with(dt8, ave(mean_price,CD_REFNIS,FUN=function(x) head(x, 1)))
dt8 <- dt8[CD_YEAR == 2016]

dt8 <- dt8[MS_TOTAL_TRANSACTIONS >= 25] # Remove anything lower than 25 transactions
dt8 <- dt8[!is.na(growth)] # Remove NAs
dt8 <- dt8[order(-growth)]

map <- get_map(location = 'Belgium', zoom = 8)
g8 <- ggmap(map) +
  geom_point(aes(x = Longitude, y = Latitude, color = growth), data = dt8 ,size=6, alpha=0.5) +
  scale_colour_gradient2(low = pal[3], mid = '#ffffff', high = pal[1], midpoint = 15, name='growth (%)') +
  labs(x='',y='',title='Plot 8: Mean house price growth in Belgian cities (2010 vs 2016)') +
  t +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
g8
rm(dt8)

################################################################
# Ninth Plot ###################################################
################################################################

dt9 <- dt[CD_TYPE_NL == 'houses']
dt9 <- dt9[,.(CD_REFNIS,CD_YEAR,MS_MEAN_PRICE=MS_MEAN_PRICE/1000)]

dt9 <- merge(dt9,empl,by.x=c('CD_REFNIS','CD_YEAR'),by.y=c('nis','jaar'),all.y=T)
dt9 <- dt9[CD_YEAR == 2014]
dt9 <- merge(dt9,pop,by.x=c('CD_REFNIS','CD_YEAR'),by.y=c('nis','jaar'),all.x=T)

g9 <- ggplot(dt9,aes(x=werkzaamheidsgraad,y=MS_MEAN_PRICE,col=ifelse(inwoners >= 50000,'population ≥ 50k','population < 50k'))) + 
  geom_point(size=2.5) +
  scale_color_manual(values=pal,name='') + 
  labs(x='employment rate',y='mean house price (1000)',title='Plot 9: Employment and mean house price in Belgian cities (2014)') +
  t
g9
rm(dt9)
