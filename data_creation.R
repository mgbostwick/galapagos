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
  Porcino <- read.csv("Cap10_GanadoPorcino.csv",header = TRUE)
  OtroGanado <- read.csv("Cap11_OtrasEspeciesGanado.csv", header = TRUE)
  Aves <- read.csv("Cap12_Aves.csv", header = TRUE)
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
  data<-merge(data, Porcino, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, OtroGanado, "UPA", all.x=TRUE, all.y = TRUE)
  data<-merge(data, Aves, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Riego: Aggregate area under irrigation, and add other non-duplicated data
  temp<-aggregate(x = Riego$r3, by = list(Riego$UPA), FUN = sum)
  colnames(temp)[colnames(temp)=="Group.1"] <- "UPA"
  colnames(temp)[colnames(temp)=="x"] <- "r3"
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  temp2<-Riego[!duplicated(Riego$UPA), ]
  temp2$r3<-NULL
  data<-merge(data, temp2, "UPA", all.x=TRUE, all.y = TRUE)

  #Forestal: Aggregate value of sold forestry products
  temp<-aggregate(x = Forestal$af6, by = list(Forestal$UPA), FUN = sum)
  colnames(temp)[colnames(temp)=="Group.1"] <- "UPA"
  colnames(temp)[colnames(temp)=="x"] <- "af6"
  data<-merge(data, temp, "UPA", all.x=TRUE, all.y = TRUE)
  
  Forestal_dedup <- unique(Forestal[,c(1,3)])
  Forestal_dedup$value <- 1
  Forestal_spread <- Forestal_dedup %>% spread(af2,value)
  Forestal_spread_filter <- Forestal_spread[,colSums(Forestal_spread, na.rm=TRUE) >= 10]
  colnames(Forestal_spread_filter)[2:length(colnames(Forestal_spread_filter))] <- paste("Forestal", colnames(Forestal_spread_filter)[2:length(colnames(Forestal_spread_filter))], sep = "_")
  data<-merge(data, Forestal_spread_filter, "UPA", all.x=TRUE, all.y = TRUE)
  
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
  pastos_spread_filter <- pastos_spread[,colSums(pastos_spread, na.rm=TRUE) >= 10]
  colnames(pastos_spread_filter)[2:length(colnames(pastos_spread_filter))] <- paste("Pastos", colnames(pastos_spread_filter)[2:length(colnames(pastos_spread_filter))], sep = "_")
  data<-merge(data, pastos_spread_filter, "UPA", all.x=TRUE, all.y = TRUE)
  
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
  CPermanentes_spread_filter <- CPermanentes_spread[,colSums(CPermanentes_spread, na.rm=TRUE) >= 10]
  colnames(CPermanentes_spread_filter)[2:length(colnames(CPermanentes_spread_filter))] <- paste("CPermanentes", colnames(CPermanentes_spread_filter)[2:length(colnames(CPermanentes_spread_filter))], sep = "_")
  data<-merge(data, CPermanentes_spread_filter, "UPA", all.x=TRUE, all.y = TRUE)
  
  #CTransitorios: Aggregate surface areas, amounts sold, grown.
  temp<-aggregate(list(CTransitorios$t8, CTransitorios$t10, CTransitorios$t28,
                       CTransitorios$libras_cosechada, CTransitorios$libras_vendida, CTransitorios$num_cultivo), by = list(CTransitorios$UPA), FUN = sum)
  names(temp) <- c("UPA", "t8", "t10", "t28", "libras_cosechada", "libras_vendida","num_cultivo")
  data<-merge(data, temp, "UPA",  all.x=TRUE, all.y = TRUE)
  
  CTransitorios_dedup <- unique(CTransitorios[,c(1,3)])
  CTransitorios_dedup$value <- 1
  CTransitorios_spread <- CTransitorios_dedup %>% spread(t2,value)
  CTransitorios_spread_filter <- CTransitorios_spread[,colSums(CTransitorios_spread, na.rm=TRUE) >= 10]
  colnames(CTransitorios_spread_filter)[2:length(colnames(CTransitorios_spread_filter))] <- paste("CTransitorios", colnames(CTransitorios_spread_filter)[2:length(colnames(CTransitorios_spread_filter))], sep = "_")
  data<-merge(data, CTransitorios_spread_filter, "UPA", all.x=TRUE, all.y = TRUE)
  
  #Arboles: Aggregate surface areas, amounts sold, grown.
  temp<-aggregate(list(Arboles$ad4, Arboles$ad6, Arboles$ad11, Arboles$produccion_en_libras_producto_cosechado_autoconsumo,
                       Arboles$produccion_en_libras_producto_vendido), by = list(Arboles$UPA), FUN = sum)
  names(temp) <- c("UPA", "ad4", "ad6", "ad11", "produccion_en_libras_producto_cosechado_autoconsumo", "produccion_en_libras_producto_vendido")
  data<-merge(data, temp, "UPA",  all.x=TRUE, all.y = TRUE)
  
 
  arboles_dedup <- unique(Arboles[,c(1,3)])
  arboles_dedup$value <- 1
  arboles_spread <- arboles_dedup %>% spread(ad2,value)
  arboles_spread_filter <- arboles_spread[,colSums(arboles_spread, na.rm=TRUE) >= 10]
  colnames(arboles_spread_filter)[2:length(colnames(arboles_spread_filter))] <- paste("Arboles", colnames(arboles_spread_filter)[2:length(colnames(arboles_spread_filter))], sep = "_")
  data<-merge(data, arboles_spread_filter, "UPA", all.x=TRUE, all.y = TRUE)
  
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
  
#PORK PRODUCTION
  #Value from sale of pork products: c12
  
#OTHER CATTLE
  #Value from sale of donkeys: o4_a
  #Value from sale of horses: o4_b
  #Value from sale of mules: o4_c
  #Value from sale of rabbits: o4_d
  #Value from sale of goats: o_e
  #Value from sale of guinea pigs: o4_f
  
#POULTRY
  #Value from sale of chicks: a7_a
  #Value from sale of hens/roosters: a7_b
  #Value from sale of ducks: a7_c
  #Value from sale of turkeys: a7_d
  #Value from sale of baby chicks: a16_a
  #Value from sale of egg-laying hens: a16_b
  #Value from sale of heavy reproductive chickens: a16_c
  #Value from sale of light reproductive chickens: a16_d
  #Value from sale of growing chickens : a16_e
  #Value from sale of other birds: a16_f
  #Value from sale of free-range eggs: a22_e
  #Value from sale of industrially produced eggs: a24_f
  
  
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


#Add variables calculated from census data
#-----------------------------------------------------------------------
#PRODUCTIVITY
  #Pounds of Permanent, Temporary, Tree, Dairy, and Beef Products, divided by surface area 
  data$productivity <-rowSums(data[,c('CosechaLibras', 'libras_cosechada', 'v44','v53_b','v53_a', 'ad6',
                                      'produccion_en_libras_producto_cosechado_autoconsumo')], na.rm=TRUE)/data$s4

#PERCENT COVER TYPES IN 2014
  data$percperm<-data$us1*100/data$us9 #Permanent Crops
  data$perctemp<-data$us2*100/data$us9 #Temporary Crops
  data$percfallow<-data$us3*100/data$us9 #fallow
  data$perctill<-data$us4*100/data$us9 #Tilled
  data$percpasture<-data$us5*100/data$us9 #Pasture
  data$percinv<-data$us6*100/data$us9 #Invasive
  data$percbrush<-data$us7*100/data$us9 #Brush
  data$percother<-data$us8*100/data$us9 #Other
  data$perctotal<-data$us9*100/data$us9

#Ok, so all are under 100 when use us9, but some go over 100% when i use s4 as total area. 
#Pastures seem extremely low values (highest <1%) Fallow and Brush are rather high.

#PERCENT COVER TYPES IN 2012
  data$percperm2<-data$us10*100/data$us18 #Permanent crops
  data$perctemp2<-data$us11*100/data$us18 #Temporary
  data$percfallow2<-data$us12*100/data$us18 #Fallow
  data$perctill2<-data$us13*100/data$us18 #Tilled
  data$percpasture2<-data$us14*100/data$us18 #Pasture
  data$percinv2<-data$us15*100/data$us18 #Invasive
  data$percbrush2<-data$us16*100/data$us18 #Brush
  data$percother2<-data$us17*100/data$us18 #Other
  data$perctotal2<-data$us18*100/data$us18

#Pastures are all 0! Fallow still high. 

#CHANGE IN PERCENT COVER FROM 2012 to 2014: 
#Total surface area in 2012 and 2014 are the same, so I will use percents for these calculations
  data$chgperm<-data$percperm-data$percperm2 #Permanent
  data$chgtemp<-data$perctemp-data$perctemp2 #Temporary
  data$chgpasture<-data$percpasture-data$percpasture2 #fallow
  data$chgtill<-data$perctill-data$perctill2 #Tilled
  data$chgfallow<-data$percfallow-data$percfallow2 #pasture
  data$chginv<-data$percinv-data$percinv2 #Invasive
  data$chgbrush<-data$percbrush-data$percbrush2 #Brush
  data$chgother<-data$percother-data$percother2 #Other
  data$chgtotal<-data$perctotal-data$perctotal2

#ANNUAL LABOR COST
  data$permworkerspay <-rowSums(data[,c('tp52_a', 'tp52_b', 'tp52_c','tp52_d','tp52_g', 'tp52_e', 'tp52_f', 'tp52_h')], na.rm=TRUE) #Total paid to all permanent worker types in last month
  data$tempworwerspay <-rowSums(data[,c('to57_a', 'to57_b', 'to57_c','to57_d','to57_e', 'to57_f')], na.rm=TRUE)#Total paid to all temporary worker types in reference week
  data$laborcost<-data$permworkerspay*12+data$tempworwerspay*8 #Total Annual labor cost
  summary(data$permworkerspay)
  
#NUMBER OF FULL TIME WORKERS SUPPORTED
  data$fulltimework<-as.integer(data$laborcost/659)

#PRODUCTION COST
  data$productioncost <-rowSums(data[,c('GastosAgricolas','GastosPecuarios')], na.rm=TRUE) #Expenses for all agricultural activities and labor

#NET INCOME
  data$expenses <-rowSums(data[,c('GastosAgricolas','GastosPecuarios', 'laborcost')], na.rm=TRUE) #Expenses for all agricultural activities and labor
  #Gross income: earnings from sale of permanent, transitory, tree, beef, dairy, and forestry products
  #As well as from pork, poultry or other type of livestock
  data$grossincome <-rowSums(data[,c('p26','t28', 'ad11','v49','v54', 'af6', 'c12', 'o4_a', 'o4_b', 'o4_c', 'o4_d', 'o_e', 'o4_f', 'a7_a', 'a7_b', 'a7_c', 'a7_d', 'a16_a', 'a16_b', 'a16_c', 'a16_d', 'a16_e', 'a16_f', 'a22_e', 'a24_f')], na.rm=TRUE) 
  data$netincome<-data$grossincome-data$expenses
  
  data$checkincome <-rowSums(data[,c('p26','t28','ad11','v49','v54','c12','o4_a','o4_b','o4_c','o4_d','o_e','o4_f','a7_a','a7_b','a7_c','a7_d',
  'a16_a','a16_b','a16_c','a16_d','a16_e','a16_f','a22_e','a24_f','af6')], na.rm=TRUE)
  
# Correct labeling of Floreana island 
levels(data$canton) <- c(levels(data$canton), "Floreana")
data$canton[data$UPA >= 745] = "Floreana"  

library(dummies)
shape_data <- read.csv("ShapeAttributes.csv")

dummy_cols <- c("Reclass", "ABANDONED", "CONSERVATION", "FARMING", "CATTLE", "FORESTRY", "LODGING", "AGUA",
                "ENERGIA_EL", "TELEFONO_F", "INTERNET", "ALCANTARIL", "VIAS_DE_AC", "RIEGO", "RELIEVE")

dummy_data <- dummy.data.frame(shape_data, names = dummy_cols)
dummy_data  <- dummy_data %>% select(-c(`Reclass `,`AGUA `,`ENERGIA_EL `,`TELEFONO_F `,`INTERNET `,`ALCANTARIL `,
                                        `VIAS_DE_AC `,`RIEGO `,`RELIEVE `,USO_DE_SU))

dup_check <-unique(dummy_data[,1])
shape_agg <-aggregate(x = dummy_data, by = list(dummy_data$UPA), FUN = max)[,-1]

shape_vars <- as.data.frame(colnames(shape_agg[,-1]))
# write_csv(shape_vars, "shape_vars.csv")

data<-merge(data, shape_agg, "UPA",  all.x=TRUE, all.y = TRUE)

save(data, file='prelim_data.RData')