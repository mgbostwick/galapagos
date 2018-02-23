setwd("/Users/michaelbostwick/Documents/Galapagos/BASES_CENSO_UPA_spss")

#Still to do: 
  #Add categorical data from spreadsheets (such as crops)
  #Generate physical data from DEM (This will take a while still)

library(tidyverse)

#Load Data (currently as SPSS files and CSV files, trying out different ones. Cleaned CSV files Use those. Omitting some )
  library(foreign)
  #Censo CGREG-MAG 2015
  CGenerales <- read.csv("Cap1_CaracteristicasGenerales.csv", header = TRUE)
  Superficie <- read.csv("Cap2_Superficie.csv", header = TRUE)
  Riego <- read.csv("Cap3_Riego.csv", header = TRUE)
  UsoSuelo <- read.csv("Cap3_2_UsoSuelo_Hoy.csv", header = TRUE)
  CPermanentes <- read.csv("Cap4_CultivosPermanentes.csv", header = TRUE)
  CTransitorios <- read.csv("Cap5_Cultivos_Transitorios.csv", header = TRUE)
  Arboles <- read.csv("Cap6_ArbolesDispersos.csv", header = TRUE)
  Pastos <- read.csv("Cap7_PastosCultivados.csv", header = TRUE)
  Forestal <- read.csv("Cap8_ActividadForestal.csv", header = TRUE)
  Vacuno <- read.csv("Cap9_GanadoVacuno.csv", header = TRUE)
  #Porcino <- read.spss("Cap10_Ganado Porcino.sav", to.data.frame=TRUE)
  #OtroGanado <- read.spss("Cap11_Otras Especies de Ganado.sav", to.data.frame=TRUE)
  #Aves <- read.spss("Cap12_Aves.sav", to.data.frame=TRUE)
  Gastos <- read.csv("Cap13_Gastos_Inversiones_Financiamiento.csv", header = TRUE)
  Equipo <- read.csv("Cap14_Equipo_maquinaria_instalaciones.csv", header = TRUE)
  Trabajadores <- read.csv("Cap15_Trabajadores.csv", header = TRUE)
  AfiliacionDeshechos <- read.csv("Cap16_AsistenciaTecnica_Afiliacion_ManejoDesechos.csv", header = TRUE)
  
  #INEC 2015
  #Poblacion2015 <- read.spss("Poblacion_CPVG15_AT.sav", to.data.frame=TRUE)
  #PoblacionRural<- subset(Poblacion2015, Poblacion2015$AREA=="Área rural")
 
#Merge all Dataframes into one huge dataframe does not work because of different lengths
  #There are issues when merging them into one dataframe. 
  #Some information is lost, other is duplicated
  #Gonna aggregate what I can, and then remove duplicate UPAs, to be able to have
  #one dataframe with 755 UPAs
  
  #Merge dataframes with 755 Obs
  data<-merge(CGenerales,AfiliacionDeshechos, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, Equipo, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, Gastos, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, Trabajadores, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, Vacuno, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Riego: Aggregate area under irrigation, and add other non-duplicated data
  temp<-aggregate(x = Riego$r3, by = list(Riego$UPA), FUN = sum)
  colnames(temp)[colnames(temp)=="Group.1"] <- "UPA"
  colnames(temp)[colnames(temp)=="x"] <- "r3"
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  temp2<-Riego[!duplicated(Riego$UPA), ]
  temp2$r3<-NULL
  data<-merge(data, temp2, "UPA", all.x=TRUE, all.y = TRUE)
  head(data)

  #Forestal: Aggregate value of sold forestry products
  temp<-aggregate(x = Forestal$af6, by = list(Forestal$UPA), FUN = sum)
  colnames(temp)[colnames(temp)=="Group.1"] <- "UPA"
  colnames(temp)[colnames(temp)=="x"] <- "af6"
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Pastos: Aggregate area of cultivated grasses, and add other non duplicated data
  temp<-aggregate(x = Pastos$pc6, by = list(Pastos$UPA), FUN = sum)
  colnames(temp)[colnames(temp)=="Group.1"] <- "UPA"
  colnames(temp)[colnames(temp)=="x"] <- "pc6"
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  temp2<-Pastos[!duplicated(Pastos$UPA), ]
  temp2$pc1<-NULL
  temp2$pc2<-NULL #Name of the cultivated grass
  temp2$pc3<-NULL 
  temp2$pc5<-NULL
  temp2$pc6<-NULL
  data<-merge(data, temp2, "UPA", all.x=TRUE, all.y = TRUE)
  
  pastos_dedup <- unique(Pastos[,c(1,3)])
  pastos_dedup$value <- 1
  pastos_spread <- pastos_dedup %>% spread(pc2,value)
  colnames(pastos_spread)[2:length(colnames(pastos_spread))] <- paste("Pastos", colnames(pastos_spread)[2:length(colnames(pastos_spread))], sep = "_")
  data<-merge(data, pastos_spread, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Superficie: aggregate surface areas for each UPA
  temp<-aggregate(.~UPA, Superficie, sum)
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Uso Suelo: Aggregate surface areas for land uses for each UPA
  temp<-aggregate(.~UPA, UsoSuelo, sum)
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)

  #CPermanentes: Aggregate surface areas, amount sold, then add other  
  temp<-aggregate(list(CPermanentes$p7, CPermanentes$p8, CPermanentes$p9,
                       CPermanentes$p26, CPermanentes$CosechaLibras, CPermanentes$VentaLibras), by = list(CPermanentes$UPA), FUN = sum)
  names(temp) <- c("UPA", "p7", "p8", "p9", "p26", "CosechaLibras", "VentaLibras")
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  
  CPermanentes_dedup <- unique(CPermanentes[,c(2,4)])
  CPermanentes_dedup$value <- 1
  CPermanentes_spread <- CPermanentes_dedup %>% spread(p2,value)
  colnames(CPermanentes_spread)[2:length(colnames(CPermanentes_spread))] <- paste("CPermanentes", colnames(CPermanentes_spread)[2:length(colnames(CPermanentes_spread))], sep = "_")
  data<-merge(data, CPermanentes_spread, "UPA", all.x=TRUE, all.y = TRUE)
  
  #CTransitorios: Aggregate surface areas, amounts sold, grown.
  temp<-aggregate(list(CTransitorios$t8, CTransitorios$t10, CTransitorios$t28,
                       CTransitorios$libras_cosechada, CTransitorios$libras_vendida), by = list(CTransitorios$UPA), FUN = sum)
  names(temp) <- c("UPA", "t8", "t10", "t28", "libras_cosechada", "libras_vendida")
  data<-merge(data, temp, "UPA",  all.x=TRUE, all.y = TRUE)
  
  CTransitorios_dedup <- unique(CTransitorios[,c(1,3)])
  CTransitorios_dedup$value <- 1
  CTransitorios_spread <- CTransitorios_dedup %>% spread(t2,value)
  colnames(CTransitorios_spread)[2:length(colnames(CTransitorios_spread))] <- paste("CTransitorios", colnames(CTransitorios_spread)[2:length(colnames(CTransitorios_spread))], sep = "_")
  data<-merge(data, CTransitorios_spread, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Arboles: Aggregate surface areas, amounts sold, grown.
  temp<-aggregate(list(Arboles$ad4, Arboles$ad11, Arboles$produccion_en_libras_producto_cosechado_autoconsumo,
                       Arboles$produccion_en_libras_producto_vendido), by = list(Arboles$UPA), FUN = sum)
  names(temp) <- c("UPA", "ad4", "ad11", "produccion_en_libras_producto_cosechado_autoconsumo", "produccion_en_libras_producto_vendido")
  data<-merge(data, temp, "UPA",  all.x=TRUE, all.y = TRUE)
  
 
  arboles_dedup <- unique(Arboles[,c(1,3)])
  arboles_dedup$value <- 1
  arboles_spread <- arboles_dedup %>% spread(ad2,value)
  colnames(arboles_spread)[2:length(colnames(arboles_spread))] <- paste("Arboles", colnames(arboles_spread)[2:length(colnames(arboles_spread))], sep = "_")
  data<-merge(data, arboles_spread, "UPA", all.x=TRUE, all.y = TRUE)
  
#USEFUL VARIABLES MAG 2015 CENSUS
#------------------------------------------------------------------
#GENERAL CHARACTERISTICS - "CGenerales"
  #Land Parcel ID: UPA
  #Island: canton
  #Total Farm Area (ha):s4
  #Terrain Sale Price: s9
  #Live on land? (Yes/No - "Sí"/"No"):c10
  #Age of landowner: c14
  #Place of birth (Galapagos/Outside Galapagos - "Galåpagos/Fuera de Galápagos"): lugar_nacimento
  #Surface area of land parcel (ha): extension_total_UPA

#IRRIGATION - "Riego"
  #Surface Area with Irrigation System: r3

#PERMANENT CROPS - "CPermanentes"
  #Planted Surface :p7
  #Surface area at productive age: p8
  #Surface harvested: p9
  #Total sale price: p26
  #Amount harvested in pounds: CosechaLibras
  #Amount sold in pounds: VentaLibras	
  
#TRANSITORY CROPS - "CTransitorios"
  #Surface Area of Transitory Crops: superficie_total
  #Name of standard transitory crop: t2
  #Number of Transitory Crops: num_cultivo
  #Total sale price of sold transitory crops: t28
  #Weight of transitory crop harvested: libras_cosechada
  #Weight of Transitory crop sold: libras_vendida
  
  #Planted surface: t8
  #Harvested surface: t10	
  #Total sale price: t28	
  #Weight of total harvest in pounds: libras_cosechada
  #Weight of total produciton sale in pounds: libras_vendida
  
#PASTURES - "Pastos"
  #Species name: pc2
  #Condition (Monocrop/Associated - "Solo"/"Asociado"): pc4
  #Surface area (ha):pc6
  
#DISPERSE TREE CROPS - "Arboles
  
  #Tree species: ad2
  #Weight of harvested tree crops:	ad6
 
  #Number of harvested trees: ad4
  #Total Sale price: ad11
  #Weight of harvested production in pounds: produccion_en_libras_producto_cosechado_autoconsumo
  #Weight of sold product in pounds: producción_en_libras_producto_vendido
  
  
#CATTLE PRODUCTION - "Vacuno"
  #Number of cattle on land: v3
  #Percent of feed that comes from grass:v30_a
  #Total milk production (liters): v44
  #Total milk sold: v45
  #Total milk sale price: v49
  #Total beef for self-consumption
  #Total beef for sale: v53_b
  #Total sale price for beef production:v54
  
#FORESTRY - "Forestal"s
  #Did you cut trees for consumption or sale?: af1	
  #Tree species: af2
  #Main use: af4
  #Value of forestry product sales: af6
  
#EXPENSES - "Gastos"
  #Did you spend buying water? ("Sí"/"No"):ga9
  #Amount spent buying water: ga9_a
  #Amount spent in agricultural transport:ga7_a
  #Other agricultural expenses?: ga15_cual
  #Amount spent for other agricultural expenses: ga15_a
  #Amount spent on animal feed: gp16_a
  #Amount spent on transport for cattle production: gp23_a
  #Expenses for agricultural  activities: GastosAgricolas
  #Expenses for cattle raising: GastosPecuarios
  
#EQUIPMENT/FACILITIES - "Equipo"
  #Number of water reservoirs:	e29
  #Capacity of water reservoir:	e29_f
  #Nuber of greenhouses: e30
  #Capacity of Greenhouses (ha):	e30_f
  
#WORKERS - "Trabajadores
  #Number of full-time agricultural workers: tp48_g
  #Total monthly salary of full-time agricultural workers: tp52_g
  #Number of temporary agricultural workers: to53_e
  #Days worked by temporary agricultural workers in the last week:	to56_e
  #Total paid in in last week to temporary agricultural workers:	to57_e
  
  #Total paid in last month to permanent administrators: tp52_a	
  #Total paid in last month to permanent butlers: tp52_b
  #Total paid in last month to permanent agronomists: tp52_c
  #Total paid in last month to permanent veterinaries: tp52_d
  #Total paid in last month to other permanent professionals tp52_e
  #Total paid in last month to permanent truck drivers: tp52_f
  #Total paid in last month to permanent agricultural workers: tp52_g
  #Total paid in last month to other permanent workers: tp52_h
  #Total paid in reference week to temporary agronomists: to57_a
  #Total paid in reference week to temporary vaterinaries: to57_b
  #Total paid in reference week to other temporary professionals: to57_c	
  #Total paid in reference week to temporary truck drivers: to57_d	
  #Total paid in reference week to temporary agricultural workers: to57_e	
  #Total paid in reference week to other temporary workers: to57_f	
  
#LAND USE - "UsoSuelo"
  #Surface area with permanent crops (ha): us1
  #Surface area with transitory crops (ha): us2
  #Surface area of tilled land (ha): us3
  #Surface area fallow land (ha):us4
  #Surface area cultivated pastures (ha):us5
  #Surface area with invasive species (ha): us6
  #Surface area with brush and forest (ha):us7
  #Surface area other uses (ha): us8
  #Total surface area in use (ha): us9

  #Surface area with permanent crops 2yrs ago (ha): us10
  #Surface area with transitory crops 2yrs ago (ha): us11
  #Surface area of tilled land 2yrs ago (ha): us12
  #Surface area fallow land 2yrs ago (ha):us13
  #Surface area cultivated pastures 2yrs ago (ha):us14
  #Surface area with invasive species 2yrs ago (ha): us15
  #Surface area with brush and forest 2yrs ago (ha):us16
  #Surface area other uses 2yrs ago (ha): us17
  #Total surface area in use 2yrs ago (ha): us18

#AFILIATIONS/WASTE MANAGEMENT
  #Do you belong to a farmer cooperative?: d3
  #Do you control nearby vegetation? bio_k

#------------------------------------------------------------------

#Variable Exploration

#There seems to be overlap in some of the variables, also, 
  #these grapsh look slightly different when using individual dataframes
summary(CGenerales$extension_total_UPA)#from general characteristics, total land area 2014
summary(Superficie$s4)#from surface spreadsheet, surface area in use 2014,
summary(UsoSuelo$us9)#from land use spreasheet, total land area in use 2014, 
summary(UsoSuelo$us18)#from land use spreasheet, total land area in in use 2012,
plot(CGenerales$extension_total_UPA)

summary(data$extension_total_UPA)#from general characteristics, total land area 2014
summary(data$s4)#from surface spreadsheet, surface area in use 2014,
summary(data$us9)#from land use spreasheet, total land area in use 2014, 
summary(data$us18)#from land use spreasheet, total land area in in use 2012,
plot(data$extension_total_UPA)

#these two variables seem to be the same?
data$area<-(data$us9-data$s4)#Doesnt work, different length
data$area<-(data$us9-data$extension_total_UPA)#Different Length
data$area<-(data$s4-data$extension_total_UPA)#Different length
summary(data$area)
plot(data$area)
#s4 is not the same, otherwise they should all be 0. us9 and us18 are the same
#extension_total_UPA is the largest of all extensions

#Likewise, for permanent, transitory, and pastures might be different:
summary(data$p7)#from permanent crop spreadsheet, total cultivated area of permanent crop
summary(data$us1)#From land use spreadsheet, area of permanent crops
plot(data$p7)
plot(data$us1)
#Slightly different, 

#Land Use in 2014
boxplot(UsoSuelo$us1,UsoSuelo$us2,UsoSuelo$us3,UsoSuelo$us4, UsoSuelo$us5,
        UsoSuelo$us6, UsoSuelo$us7, UsoSuelo$us8, 
        names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),
        las=2,   
        ylab="Surface area (ha) ", 
        main="Reported land use in 2014 for all islands")

boxplot(data$us1,data$us2,data$us3,data$us4, data$us5,
        data$us6, data$us7, data$us8, 
        names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),
        las=2,   
        ylab="Surface area (ha) ", 
        main="Reported land use in 2014 for all islands")

#Again, slightly different

#Do they add up to total?
UsoSuelo$sumallcov<-UsoSuelo$us1+UsoSuelo$us2+UsoSuelo$us3+UsoSuelo$us4+UsoSuelo$us5+UsoSuelo$us6+UsoSuelo$us7+UsoSuelo$us8 
UsoSuelo$sumallcov<-UsoSuelo$sumallcov-UsoSuelo$us9
summary(data$sumallcov)
#Null?
data$sumallcov<-data$us1+data$us2+data$us3+data$us4+data$us5+data$us6+data$us7+data$us8 
data$sumallcov<-data$sumallcov-data$us9
summary(data$sumallcov)
plot(data$sumallcov) #small outliers

#Add variables calculated from census data
#-----------------------------------------------------------------------
#PRODUCTIVITY
  #Pounds of Permanent, Temporary, Tree, Dairy, and Beef Products, divided by surface area 
  data$productivity <-rowSums(data[,c('CosechaLibras', 'libras_cosechada', 'v44','v53_b','v53_a', 
                                      'produccion_en_libras_producto_cosechado_autoconsumo')], na.rm=TRUE)/data$s4
  plot(data$productivity)
  summary(data$productivity) #
  plot(data$extension_total_UPA, data$productivity, 
     ylab="Crop, Beef, Dairy Productivity (lb/ha)", 
     xlab="Total UPA surface area (ha)", 
     main="UPA Productivity vs. UPA surface area")
#looks like past a certain point, farms become comparatively less productive
  
#PERCENT COVER TYPES IN 2014
  data$percperm<-data$us1*100/data$us9 #Permanent Crops
  data$perctemp<-data$us2*100/data$us9 #Temporary Crops
  data$percpasture<-data$us3*100/data$us9 #Pasture
  data$perctill<-data$us4*100/data$us9 #Tilled Land
  data$percfallow<-data$us5*100/data$us9 #Fallow
  data$percinv<-data$us6*100/data$us9 #Invasive
  data$percbrush<-data$us7*100/data$us9 #Brush
  data$percother<-data$us8*100/data$us9 #Other
  data$perctotal<-data$us9*100/data$us9

#Ok, so all are under 100 when use us9, but some go over 100% when i use s4 as total area. 
#Pastures seem extremely low values (highest <1%) Fallow and Brush are rather high.

#PERCENT COVER TYPES IN 2012
  data$percperm2<-data$us10*100/data$us18 #Permanent crops
  data$perctemp2<-data$us11*100/data$us18 #Temporary
  data$percpasture2<-data$us12*100/data$us18 #Pasture
  data$perctill2<-data$us13*100/data$us18 #Tilled
  data$percfallow2<-data$us14*100/data$us18 #Fallow
  data$percinv2<-data$us15*100/data$us18 #Invasive
  data$percbrush2<-data$us16*100/data$us18 #Brush
  data$percother2<-data$us17*100/data$us18 #Other
  data$perctotal2<-data$us18*100/data$us18

#Pastures are all 0! Fallow still high. 

#CHANGE IN PERCENT COVER FROM 2012 to 2014: 
#Total surface area in 2012 and 2014 are the same, so I will use percents for these calculations
  data$chgperm<-data$percperm-data$percperm2 #Permanent
  data$chgtemp<-data$perctemp-data$perctemp2 #Temporary
  data$chgpasture<-data$percpasture-data$percpasture2 #Pasture
  data$chgtill<-data$perctill-data$perctill2 #Tilled
  data$chgfallow<-data$percfallow-data$percfallow2 #Fallow
  data$chginv<-data$percinv-data$percinv2 #Invasive
  data$chgbrush<-data$percbrush-data$percbrush2 #Brush
  data$chgother<-data$percother-data$percother2 #Other
  data$chgtotal<-data$perctotal-data$perctotal2

#ANNUAL LABOR COST
  data$permworkerspay <-rowSums(data[,c('tp52_a', 'tp52_b', 'tp52_c','tp52_d','tp52_g', 'tp52_e', 'tp52_f', 'tp52_h')], na.rm=TRUE) #Total paid to all permanent worker types in last month
  data$tempworwerspay <-rowSums(data[,c('to57_a', 'to57_b', 'to57_c','to57_d','to57_e', 'to57_f')], na.rm=TRUE)#Total paid to all temporary worker types in reference week
  data$laborcost<-data$permworkerspay*12+data$tempworwerspay*8 #Total Annual labor cost
  summary(data$permworkerspay)

  plot(data$extension_total_UPA, data$laborcost, 
       ylab="Labor cost (USD/yr)", 
       xlab="Total UPA surface area (ha)", 
       main="Labor cost vs. UPA surface area")
  m <- glm(data$extension_total_UPA~data$laborcost)
  abline(m, col="blue", lwd=1)
  summary(m)
  coef(m)
  
  
#NUMBER OF FULL TIME WORKERS SUPPORTED
  data$fulltimework<-(data$laborcost/659)
  sum(data$fulltimework)
  summary(data$fulltimework)
  plot(data$fulltimework)
  
#PRODUCTION COST
  data$productioncost <-rowSums(data[,c('GastosAgricolas','GastosPecuarios')], na.rm=TRUE) #Expenses for all agricultural activities and labor
  summary(data$productioncost)
  
#NET INCOME
  data$expenses <-rowSums(data[,c('GastosAgricolas','GastosPecuarios', 'laborcost')], na.rm=TRUE) #Expenses for all agricultural activities and labor
  summary(data$expenses)
  plot(data$expenses)
  #Gross income: earnings from sale of permanent, transitory, tree, beef, dairy, and forestry products
  data$grossincome <-rowSums(data[,c('p26','t28', 'ad11','v49','v54', 'af6')], na.rm=TRUE) 
  plot(data$grossincome)
  data$netincome<-data$grossincome-data$expenses
  summary(data$netincome)
  plot(data$netincome)
  #mostly negative?.
  
save(data, file='prelim_data.RData')

#Descriptive/Exploratory graphs----------------------------------------------------
  
par(mfrow=c(1,3))  

#Percent Land use in 2014 
boxplot(data$us1,data$us2,data$us3,data$us4, data$us5,
          data$us6, data$us7, data$us8, 
          names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),
          las=2,   
          ylab="Surface area (ha) ", 
          main="Reported land use in 2014 for all islands")  
  
#Percent Land use in 2014  
boxplot(data$percperm,data$perctemp,data$percpasture,data$perctill, data$percfallow,
        data$percinv, data$percbrush, data$percother, 
        names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),las=2,  
        ylab="% of UPA", 
        main="Percent of UPA land cover in 2014 for all islands")

#Change in land use
boxplot(data$chgperm,data$chgtemp,data$chgpasture,data$chgtill, data$chgfallow,
          data$chginv, data$chgbrush, data$chgother, 
          names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),las=2,  
          ylab="% of UPA", 
          main="Change in reported percent of land use from 2012 to 2014 for all islands")
  
  #Nearly no change. Anything positive or negative is an outlier... can this be used at all?
  

#Stratchpad-------------------

#Plot explore
plot(data$tp52_g, data$us6, xlab="permanent workers salary", ylab="Invasive species cover (ha)") #

# Basic Scatterplot Matrix
pairs(~expenses+fulltimework+productivity+percinv+percfallow,data=data, 
      main="Simple Scatterplot Matrix")
levels(data$ga9)

# Scatterplot Matrices from the lattice Package 
library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(data[c('productioncost','grossincome', 'fulltimework','productivity','percinv','percfallow')], groups=data$ga9, data=data,
      panel=panel.superpose, 
      key=list(title="Variable exploration",
               columns=2,
               points=list(pch=super.sym$pch[1:2],
                           col=super.sym$col[1:2]),
               text=list(c("Does not buy water","Buys water"))))

# Scatterplot Matrices from the glus Package 
library(gclus)

dta <- data[c('productioncost','grossincome', 'fulltimework','productivity','percinv','percfallow')] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


#Effect of water
par(mfrow=c(1,3)) 
NoTanks<-subset(data, ga10 == "No")
Tanks<-subset(data, ga10 == "Si")

boxplot(NoTanks$percperm,NoTanks$perctemp,NoTanks$percpasture,NoTanks$perctill, NoTanks$percfallow,
        NoTanks$percinv, NoTanks$percbrush, NoTanks$percother, 
        names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),las=2,  
        ylab="% of UPA", 
        main="DO NOT purchase water")

boxplot(Tanks$percperm,Tanks$perctemp,Tanks$percpasture,Tanks$perctill, Tanks$percfallow,
        Tanks$percinv, Tanks$percbrush, Tanks$percother, 
        names=c("Permanent", "Transitory", "Pastures", "Tilled", "Fallow", "Invasive", "Brush/Forest", "Other"),las=2,  
        ylab="% of UPA", 
        main="DO purchase water")

boxplot(Tanks$percinv, NoTanks$percinv, 
        names=c("Yes Water", "No Water"),las=2,  
        ylab="% of UPA", 
        main="Invasive Species")

#Effect of associated pastures
Silvipasture<-subset(data, pc4 == "Asociado")
Monocrop<-subset(data, pc4 == "Solo")
par(mfrow=c(1,1))
boxplot(Silvipasture$ga10_a,Monocrop$ga10_a, 
        names=c("Silvipasture", "Monocrop"),las=1,  
        ylab="Amount spent on purchasing water", 
        main="Farm structure vs. water purchases")
#This is exciting!!

