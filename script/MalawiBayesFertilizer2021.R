library(foreign)
library(readstata13)
library(dplyr)
library(car)
library(gmodels)
library(pastecs)
library(foreign)
library(readstata13)
library(spdep)
library(spBayes)
library(classInt)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(tidyr)
library(reshape)
library(tables)
library(MBA)
library(fields)
library(maptools)
library(classInt)
library(Hmisc)
library(nlme)
library(foreign)
library(readstata13)
library(haven)

set.seed(1)

# AG MOD A[HOUSEHOLD LEVEL] -----------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_A_FILT=read.dta13("AG_MOD_A_FILT.dta")
attach(AG_MOD_A_FILT)
summary(AG_MOD_A_FILT)
get.varlabel(AG_MOD_A_FILT)
CrossTable(AG_MOD_A_FILT$ag_c01)

# AG MOD B[CROP LEVEL] ----------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_B=read.dta13("AG_MOD_B.dta")
attach(AG_MOD_B)
summary(ag_b01a)
plot(ecdf(ag_b01a),xlim=c(0,6))

# AG MOD C[PLOT LEVEL] ----------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_C=read.dta13("AG_MOD_C.dta",generate.factors=T)
attach(AG_MOD_C)
summary(ag_c02)
summary(ag_c04c)
table(ag_c04b)
table(ag_c00)
plot(ecdf(ag_c04a),xlim=c(0,6))
plot(ecdf(ag_c04c),xlim=c(0,6))

# PLOT AND FARM SIZES -------------------------------
AG_MOD_C$hectaresfarmer=ifelse(AG_MOD_C$ag_c04b=="Acre",AG_MOD_C$ag_c04a*0.404686,
                               ifelse(AG_MOD_C$ag_c04b=="Hectare",AG_MOD_C$ag_c04a*1,
                                      ifelse(AG_MOD_C$ag_c04b=="Square Meters",AG_MOD_C$ag_c04a*0.0001,0)))

AG_MOD_C$hectaresgps=AG_MOD_C$ag_c04c*0.404686

AG_MOD_C$acresfarmer=ifelse(AG_MOD_C$ag_c04b=="Acre",AG_MOD_C$ag_c04a*1,
                            ifelse(AG_MOD_C$ag_c04b=="Hectare",AG_MOD_C$ag_c04a*2.47105,
                                   ifelse(AG_MOD_C$ag_c04b=="Square Meters",AG_MOD_C$ag_c04a*0.000247105,0)))

attach(AG_MOD_C)

head(sort(AG_MOD_C$hectaresfarmer, decreasing = TRUE))
head(sort(AG_MOD_C$hectaresgps, decreasing = TRUE))
head(sort(AG_MOD_C$ag_c04c, decreasing = TRUE), n=10)
head(sort(AG_MOD_C$acresfarmer, decreasing = TRUE), n=10)


# Reshaping to get total farm sizes
AG_MOD_C_wide=reshape(AG_MOD_C, v.names=c("ag_c04c","acresfarmer"),idvar="case_id",direction="wide",timevar="ag_c00")
attach(AG_MOD_C_wide)

psum <- function(..., na.rm=FALSE) { 
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
} 

AG_MOD_C_wide=transform(AG_MOD_C_wide, new=psum(ag_c04c.R1,ag_c04c.R2,ag_c04c.R3,ag_c04c.R4,ag_c04c.R5,ag_c04c.R6,ag_c04c.R7,ag_c04c.R8,ag_c04c.R9,ag_c04c.R10, na.rm=TRUE))
attach(AG_MOD_C_wide)
library(plyr)
AG_MOD_C_wide$GPSFarmsize=new
AG_MOD_C_wide[,c("new")] <- list(NULL)

AG_MOD_C_wide=transform(AG_MOD_C_wide,new=psum(acresfarmer.R1,acresfarmer.R2,acresfarmer.R3,acresfarmer.R4,acresfarmer.R5,acresfarmer.R6,acresfarmer.R7,acresfarmer.R8,acresfarmer.R9,acresfarmer.R10, na.rm=TRUE))

# AG_MOD_C_wide$Farmerreported_Farmsize=new
# AG_MOD_C_wide[,c("new")] <- list(NULL)

head(sort(AG_MOD_C_wide$new, decreasing = TRUE), n=10)
head(sort(AG_MOD_C_wide$GPSFarmsize, decreasing = TRUE), n=10)

# PLOT GEOVARIABLES[PLOT LEVEL] ---------------------
# There are only 18329 plots that were georeferenced using the distance from the plot to the home.
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
PlotGeovariables=read.dta("PlotGeovariables.dta")
attach(PlotGeovariables)
names(PlotGeovariables)

PlotGeovariableswide=reshape(PlotGeovariables,v.names="dist_hh",idvar="case_id",direction="wide",timevar="plot_id")
attach(PlotGeovariableswide)

# HOUSEHOLD GEOVARIABLES [HOUSEHOLD LEVEL] -----------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
HouseholdGeovariables=read.dta("HouseholdGeovariables.dta")
attach(HouseholdGeovariables)
names(HouseholdGeovariables)

coords <- as.matrix(HouseholdGeovariables[,c("lat_modified","lon_modified")])
plot(coords, pch=1,col="darkgreen", xlab="Latitude", ylab="Longitude")

library(MBA)
library(fields)
library(maptools)
library(classInt)
x.res <- 100; y.res <- 100
surf <- mba.surf(cbind(coords, HouseholdGeovariables$dist_road), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="Lat", ylab="Long")

surf2 <- mba.surf(cbind(coords, HouseholdGeovariables$dist_admarc), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf2, xaxs = "r", yaxs = "r", xlab="Lat", ylab="Long")

surf3 <- mba.surf(cbind(coords, HouseholdGeovariables$dist_boma), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf3, xaxs = "r", yaxs = "r", xlab="Lat", ylab="Long")

# SUMMARY [HOUSEHOLD] ----------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
ihs3_summary=read.dta13("ihs3_summary.dta")
attach(ihs3_summary)
# Merging geovariables

ihs3_summary_hh_geo=right_join(HouseholdGeovariables,ihs3_summary,by=c("case_id","ea_id"))
attach(ihs3_summary_hh_geo)
# Distribution of Incomes Quintiles 
surf3 <- mba.surf(cbind(coords, ihs3_summary_hh_geo$quintil), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf3, xaxs = "r", yaxs = "r", xlab="Lat", ylab="Long")


# Merging plot geovariables----------------
PlotGeovariableswides=PlotGeovariableswide[order(PlotGeovariableswide$case_id),]
ihs3_summary_hh_geos=ihs3_summary_hh_geo[order(ihs3_summary_hh_geo$case_id),]
detach(PlotGeovariableswide)
detach(ihs3_summary_hh_geo)
attach(PlotGeovariableswides)
attach(ihs3_summary_hh_geos)
plotgeo_ihs3_summary_hh_geos=inner_join(PlotGeovariableswides,ihs3_summary_hh_geos,by=c("case_id"))
summary(plotgeo_ihs3_summary_hh_geos$dist_hh.R1)
detach(PlotGeovariableswides)
detach(ihs3_summary_hh_geos)

##### Reshape to plot level
plotgeo_ihs3_summary_hh_geos_wide=reshape(plotgeo_ihs3_summary_hh_geos,idvar="case_id", varying=list(c("dist_hh.R1","dist_hh.R2","dist_hh.R3","dist_hh.R4","dist_hh.R5","dist_hh.R6","dist_hh.R7","dist_hh.R8","dist_hh.R9","dist_hh.R10")),v.names="dist_hh",times=c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10"),timevar="plot_id",direction="long")
#detach(plotgeo_ihs3_summary_hh_geos)
attach(plotgeo_ihs3_summary_hh_geos_wide)
names(plotgeo_ihs3_summary_hh_geos_wide)
CrossTable(plot_id)

plotgeo_ihs3_summary_hh_geos_wide_formerging=filter(plotgeo_ihs3_summary_hh_geos_wide,plotgeo_ihs3_summary_hh_geos_wide$dist_hh !="NA")

CrossTable(plotgeo_ihs3_summary_hh_geos_wide_formerging$plot_id)
detach(plotgeo_ihs3_summary_hh_geos_wide)

# Merging plot level data: AG_MOD_C and AG_MOD_D -----
# AG MOD C[PLOT LEVEL]
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_C=read.dta13("AG_MOD_C.dta",generate.factors=T)
attach(AG_MOD_C)
summary(ag_c02)
summary(ag_c04c)
table(ag_c04b)
table(ag_c00)
plot(ecdf(ag_c04a),xlim=c(0,6))
plot(ecdf(ag_c04c),xlim=c(0,6))

#####

AG_MOD_C$hectaresfarmer=ifelse(AG_MOD_C$ag_c04b=="Acre",AG_MOD_C$ag_c04a*0.404686,
                               ifelse(AG_MOD_C$ag_c04b=="Hectare",AG_MOD_C$ag_c04a*1,
                                      ifelse(AG_MOD_C$ag_c04b=="Square Meters",AG_MOD_C$ag_c04a*0.0001,0)))
AG_MOD_C$hectaresgps=AG_MOD_C$ag_c04c*0.404686
AG_MOD_C$acresfarmer=ifelse(AG_MOD_C$ag_c04b=="Acre",AG_MOD_C$ag_c04a*1,
                            ifelse(AG_MOD_C$ag_c04b=="Hectare",AG_MOD_C$ag_c04a*2.47105,
                                   ifelse(AG_MOD_C$ag_c04b=="Square Meters",AG_MOD_C$ag_c04a*0.000247105,0)))
attach(AG_MOD_C)
head(sort(AG_MOD_C$hectaresfarmer, decreasing = TRUE))
head(sort(AG_MOD_C$hectaresgps, decreasing = TRUE))
head(sort(AG_MOD_C$ag_c04c, decreasing = TRUE), n=10)
head(sort(AG_MOD_C$acresfarmer, decreasing = TRUE), n=10)

# Sort the two datasets by case_id and plot_id
plotgeo_ihs3_summary_hh_geos_wide_formergings=plotgeo_ihs3_summary_hh_geos_wide_formerging[order(plotgeo_ihs3_summary_hh_geos_wide_formerging$case_id,
                                                                                                 plotgeo_ihs3_summary_hh_geos_wide_formerging$plot_id),]
attach(plotgeo_ihs3_summary_hh_geos_wide_formergings)

AG_MOD_Cs=AG_MOD_C[order(AG_MOD_C$case_id,AG_MOD_C$ag_c00),]
attach(AG_MOD_Cs)

AG_MOD_Cs$case_plotid=paste(AG_MOD_Cs$case_id,AG_MOD_Cs$ag_c00)
plotgeo_ihs3_summary_hh_geos_wide_formergings$case_plotid=paste(plotgeo_ihs3_summary_hh_geos_wide_formergings$case_id,plotgeo_ihs3_summary_hh_geos_wide_formergings$plot_id)

plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs=inner_join(plotgeo_ihs3_summary_hh_geos_wide_formergings,AG_MOD_Cs,by=c("case_plotid"))

Mean <- function(x) base::mean(x, na.rm=TRUE)
SD <- function(x) base::sd(x, na.rm=TRUE)

tabular(district ~ Format(digits=2)*(hhsize+hectaresgps+hectaresfarmer)*(Mean+sd),data=plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs)

######################## AG MOD D[PLOT LEVEL]
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_D=read.dta13("AG_MOD_D.dta")
attach(AG_MOD_D)
summary(AG_MOD_D)
#get.varlabel(AG_MOD_D)

#Sort then generate new caseplot_id
AG_MOD_Ds=AG_MOD_D[order(AG_MOD_D$case_id,AG_MOD_D$ag_d00),]
AG_MOD_Ds$case_plotid=paste(AG_MOD_Ds$case_id,AG_MOD_Ds$ag_d00)

#
plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs_AG_MOD_Ds=inner_join(plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs,AG_MOD_Ds,by=c("case_plotid"))

tabular(district ~ Format(digits=2)*(hhsize+hectaresgps+hectaresfarmer)*(mean+sd),data=plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs_AG_MOD_Ds)


# ag_d01 
#"Who in HH makes decisions concerning crops to be planted,input use,timing of cro" 
# "What crops were planted on this[PLOT]during the[RS]?(1st)" 
# ag_d20b 

# AG MOD G [PLOT CROP] ------------------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")
AG_MOD_G=read.dta13("AG_MOD_G.dta",generate.factors=T)
attach(AG_MOD_G)
summary(AG_MOD_G)
get.label(AG_MOD_G)
get.varlabel(AG_MOD_G)

table(ag_g0d)
CrossTable(ag_g0d)
CrossTable(ag_g0b)
CrossTable(ag_g01,ag_g02)
CrossTable(ag_g0d,ag_g01)
CrossTable(ag_g03,ag_g01)
CrossTable(ag_g0d,ag_g03)

# Yield Calculations

CrossTable(ag_g0d,ag_g13b)

# setwd("C:/Users/mkond001/OneDrive/6_Spring 2016/APEC 8904/Paper/Data and Models/Malawi Data/Malawi Data/IHS 3 DATA AND #DOCUMENTS/MALAWI IHS 3 DATA/STATA DATA/IHS3_kgfactor_DTA(1)")
# kgfactor=read.dta13("kgfactor.dta",generate.factors=T)
# get.varlabel(kgfactor)



# Reshaping to plot level so to merge with plot level data
AG_MOD_G_Thin=AG_MOD_G[,c("case_id","ea_id","ag_g0b","ag_g0d","ag_g0d_os","ag_g01")]

AG_MOD_G_Thin=AG_MOD_G

AG_MOD_G_Thin$plot_crop_id=seq(from = 1, to = 28143)
attach(AG_MOD_G_Thin)

AG_MOD_G_Thin$case_plotid=paste(AG_MOD_G_Thin$case_id,AG_MOD_G_Thin$ag_g0b)
attach(AG_MOD_G_Thin)


HHgeo_plotgeo_summary_C_D_Gminsmall=merge(plotgeo_ihs3_summary_hh_geos_wide_formergings_AG_MOD_Cs_AG_MOD_Ds,AG_MOD_G_Thin, by="case_id",all.y=FALSE)
# # PRELIMINARY ANALYSIS ------------------------------------





dev.off()
coords <- as.matrix(HHgeo_plotgeo_summary_C_D_Gminsmall[,c("lon_modified","lat_modified")])
plot(coords, pch=1,cex=sqrt(hhsize)/5,col="darkgreen")


x.res <- 100; y.res <- 100
surfrexpaggcap <- mba.surf(cbind(coords, HHgeo_plotgeo_summary_C_D_Gminsmall$rexpaggcap), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surfrexpaggcap, xaxs = "r", yaxs = "r",xlab="Longitude",ylab="Latitude")
contour(surfrexpaggcap,add=T)

col.br=colorRampPalette(c("green","blue","cyan","yellow","red"))
col.pal=col.br(5)
fixed=classIntervals(hhsize,n=5,style="fixed",fixedBreaks=c(1,2,3,4,5,max(hhsize)+1))
#fixed=classIntervals(hhsize,n=4,style="pretty")
fixed.col=findColours(fixed,col.pal)

plot(coords,col=fixed.col,pch=19,cex=0.5,xlab="Longitude",ylab="Latitude")
legend("topright",fill=attr(fixed.col,"palette"),legend=c("1","2","3","4",">5"),bty="n",title="Household size")


# Proportion of farmers doing mixed farming across
CrossTable(HHgeo_plotgeo_summary_C_D_Gminsmall$district,HHgeo_plotgeo_summary_C_D_Gminsmall$ag_g01,digits=2,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,format=c("SPSS"),dnn = c("District", "Crop Stand"))


###################Plotting Crop Stand by Location
dev.off()
cropstand=ifelse(HHgeo_plotgeo_summary_C_D_Gminsmall$ag_g01=="Pure stand",1,
                 ifelse(HHgeo_plotgeo_summary_C_D_Gminsmall$ag_g01=="Mixed stand",2,0))



col.br=colorRampPalette(c("red","blue"))
col.pal=col.br(5)
fixed=classIntervals(cropstand,n=2,style="fixed",fixedBreaks=c(1,2))
#fixed=classIntervals(hhsize,n=4,style="pretty")
fixed.col=findColours(fixed,col.pal)

plot(coords,col=fixed.col,pch=19,cex=0.5,xlab="Longitude",ylab="Latitude")
legend("topright",fill=attr(fixed.col,"palette"),legend=c("Pure stand","Mixed stand"),bty="n",title="Crop Stand")


# Poverty spatial variation across the country.
CrossTable(district,poor,digits=2,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,format=c("SPSS"))
dev.off()
pov=ifelse(HHgeo_plotgeo_summary_C_D_Gminsmall$poor=="Non-Poor",1,
           ifelse(HHgeo_plotgeo_summary_C_D_Gminsmall$ag_g01=="Poor",2,0))



col.br=colorRampPalette(c("red","blue"))
col.pal=col.br(5)
fixed=classIntervals(pov,n=2,style="fixed",fixedBreaks=c(1,2))
#fixed=classIntervals(hhsize,n=4,style="pretty")
fixed.col=findColours(fixed,col.pal)

plot(coords,col=fixed.col,pch=19,cex=0.5,xlab="Longitude",ylab="Latitude")
legend("topright",fill=attr(fixed.col,"palette"),legend=c("Non-Poor","Poor"),bty="n",title="Poverty")

#Plotting Maize Varieties and Legume Type Stand by Location



# Maize Variety Stand

summary(ag_g13a)

# Compute Yields

# Compute Fertilizer Use
attach(HHgeo_plotgeo_summary_C_D_Gminsmall)

library(mosaic)
mutate(HHgeo_plotgeo_summary_C_D_Gminsmall, unitkgs = derivedFactor(
     "1" = (ag_g13b=="KILOGRAM"),
     "50" = (ag_g13b=="50 KG BAG"),
     "90" = (ag_g13b=="90 KG BAG"),

     "4.26"= (region=="South") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (SMALL)"),
     "4.47"= (region=="Central") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (SMALL)"),
     "4.38"= (region=="North") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (SMALL)"),

     "17.60"= (region=="South") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (LARGE)"),
     "16.46"= (region=="Central") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (LARGE)"),
     "18.49"= (region=="North") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="PAIL (LARGE)"),

     "0.20"= (region=="South") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 10 PLATE"),
     "0.20"= (region=="Central") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 10 PLATE"),
     "0.20"= (region=="North") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 10 PLATE"),

      "0.37"= (region=="South") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 12 PLATE"),
     "0.26"= (region=="Central") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 12 PLATE"),
     "0.33"= (region=="North") & (crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED")) & (ag_g13b=="NO. 12 PLATE"),
    .ordered = TRUE,
     .method ="unique",
     .default=NA
 ))

kg=ifelse(ag_g13b=="KILOGRAM",1,0)
kg50=ifelse(ag_g13b=="50 KG BAG",50,0)
kg90=ifelse(ag_g13b=="90 KG BAG",90,0)

PAILSMALLsouth=ifelse(region=="South" & ag_g13b=="PAIL (SMALL)",4.26,0)

PAILSMALLcentral=ifelse(region=="Central"& ag_g13b=="PAIL (SMALL)",4.47,0)
PAILSMALLnorth=ifelse(region=="North" & ag_g13b=="PAIL (SMALL)",4.38,0)

PAILLARGEsouth=ifelse(region=="South" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",17.60,0)
PAILLARGEcentral=ifelse(region=="Central" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",16.46,0)
PAILLARGEnorth=ifelse(region=="North" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",18.49,0)

N10PLATE=ifelse(crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="NO. 10 PLATE",0.2,0)

N12PLATEsouth=ifelse(region=="South" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",0.37,0)
N12PLATEcentral=ifelse(region=="Central" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",0.26,0)
N12PLATEnorth=ifelse(region=="North" & crop_id==c("MAIZE LOCAL","MAIZE COMPOSITE/OPV","MAIZE HYBRID","MAIZE HYBRID RECYCLED") & ag_g13b=="PAIL (LARGE)",0.33,0)



# +PAILLARGEnorth*ag_g13a+PAILSMALLsouth*ag_g13a+PAILSMALLcentral*ag_g13a+PAILSMALLnorth*ag_g13a+PAILLARGEsouth*ag_g13a+PAILLARGEcentral*ag_g13a+PAILLARGEnorth*ag_g13a+N10PLATE*ag_g13a+N12PLATEsouth*ag_g13a+N12PLATEcentral*ag_g13a+N12PLATEnorth*ag_g13a


LocalMaize=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE LOCAL")
HHgeo_plotgeo_summary_C_D_GminsmallOPV=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE COMPOSITE/OPV")
HHgeo_plotgeo_summary_C_D_GminsmallHybrid=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE HYBRID")
HHgeo_plotgeo_summary_C_D_GminsmallHybridRecycled=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE HYBRID RECYCLED")

MaizeData=bind_rows(HHgeo_plotgeo_summary_C_D_GminsmallLocalMaize,HHgeo_plotgeo_summary_C_D_GminsmallOPV,HHgeo_plotgeo_summary_C_D_GminsmallHybrid,HHgeo_plotgeo_summary_C_D_GminsmallHybridRecycled)

MaizeData$kg=ifelse(MaizeData$ag_g13b=="KILOGRAM",1,0)


MaizeData$kg50=ifelse(MaizeData$ag_g13b=="50 KG BAG",50,0)
MaizeData$kg90=ifelse(MaizeData$ag_g13b=="90 KG BAG",90,0)

attach(MaizeData)
names(MaizeData)

MaizeData$prodn=kg*ag_g13a+kg50*ag_g13a+kg90*ag_g13a

MaizeData$yield_kg_ha=MaizeData$prodn/MaizeData$hectaresgps

tabular(crop_id~ Format(digits=2)*(yield_kg_ha)*(mean+sd),data=MaizeData)

MaizeDatakg_kg50_kg90=filter(MaizeData,MaizeData$prodn!=0)

MaizeDatakg_kg50_kg90noNA=filter(MaizeDatakg_kg50_kg90,MaizeDatakg_kg50_kg90$prodn!="NA")

MaizeDatakg_kg50_kg90noNA$yield_kg_ha=MaizeDatakg_kg50_kg90noNA$prodn/MaizeDatakg_kg50_kg90noNA$hectaresgps

plot(MaizeDatakg_kg50_kg90noNA$yield_kg_ha)


attach(MaizeDatakg_kg50_kg90noNA)

MaizeDatakg_kg50_kg90noNA=filter(MaizeDatakg_kg50_kg90noNA,MaizeDatakg_kg50_kg90noNA$yield_kg_ha!="NA")

MaizeDatakg_kg50_kg90noNAFINAL=as.data.frame(MaizeDatakg_kg50_kg90noNA)

MaizeDatakg_kg50_kg90noNAFINAL=filter(MaizeDatakg_kg50_kg90noNAFINAL,MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha!=411841.9)

MaizeDatakg_kg50_kg90noNAFINAL=as.data.frame(MaizeDatakg_kg50_kg90noNAFINAL)
tabular(MaizeDatakg_kg50_kg90noNAFINAL$region~ Format(digits=2)*(MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha)*(mean+sd),data=MaizeDatakg_kg50_kg90noNAFINAL)

summary(MaizeDatakg_kg50_kg90noNAFINAL)
# Before truncating
coords <- as.matrix(MaizeDatakg_kg50_kg90noNAFINAL[,c("lon_modified","lat_modified")])
plot(coords, pch=1,col="darkgreen", xlab="Longitude", ylab="Latitude")

x.res <- 100; y.res <- 100
surf <- mba.surf(cbind(coords, MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="Longitude", ylab="Latitude")

head(sort(MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha, decreasing = TRUE), n=50)
head(sort(MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha, decreasing = FALSE), n=50)

Maize_0.5toppercentout=filter(MaizeDatakg_kg50_kg90noNAFINAL,MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha<12355.26)
Maize_0.5topdownpercentout=filter(MaizeDatakg_kg50_kg90noNAFINAL,MaizeDatakg_kg50_kg90noNAFINAL$yield_kg_ha>7.1972380)


# Yields

attach(Maize_0.5topdownpercentout)
coords <- as.matrix(Maize_0.5topdownpercentout[,c("lon_modified","lat_modified")])
plot(coords, pch=1,col="darkgreen", xlab="Longitude", ylab="Latitude")

x.res <- 100; y.res <- 100
surf <- mba.surf(cbind(coords, Maize_0.5topdownpercentout$yield_kg_ha), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="Longitude", ylab="Latitude")

#

# Yield Distribution by Treatments
dev.off()
south=subset(Maize_0.5topdownpercentout$yield_kg_ha,region=="South")
central=subset(Maize_0.5topdownpercentout$yield_kg_ha,region=="Central")
north=subset(Maize_0.5topdownpercentout$yield_kg_ha,region=="North")

plot(ecdf(south),xlab="Maize Yield(Kg/ha) ",ylab="p(yield)",main="", col="1",xlim=c(0,10000))
lines(ecdf(central), col="2")
lines(ecdf(north), lwd=0.5,col="3")

legend("topright",legend=c("South","Central","North"),
       col=c("1","2","3"),lty=rep(1,3),title="Region")

#
dev.off()
local=subset(Maize_0.5topdownpercentout$yield_kg_ha,crop_id=="MAIZE LOCAL")
hybrid=subset(Maize_0.5topdownpercentout$yield_kg_ha,crop_id=="MAIZE HYBRID")
opv=subset(Maize_0.5topdownpercentout$yield_kg_ha,crop_id=="MAIZE COMPOSITE/OPV")
hybridrecycled=subset(Maize_0.5topdownpercentout$yield_kg_ha,crop_id=="MAIZE HYBRID RECYCLED")

plot(ecdf(local),xlab="Maize Yield(Kg/ha) ",ylab="p(yield)",main="", col="1",xlim=c(0,10000))
lines(ecdf(hybrid), col="2")
lines(ecdf(opv), col="3")
lines(ecdf(hybridrecycled),col="4")

legend("topright",legend=c("Local","Hybrid","Composite","Hybrid Recycled"),
       col=c("1","2","3","4"),lty=rep(1,4),title="Variety")

# Seed
CrossTable(ag_g04b)
Maize_0.5topdownpercentout$gramseed=ifelse(ag_g04b=="Gram",0.001,0)
Maize_0.5topdownpercentout$kgseed=ifelse(ag_g04b=="Kilogram",1,0)
Maize_0.5topdownpercentout$kg2seed=ifelse(ag_g04b=="2 kg bag",2,0)
Maize_0.5topdownpercentout$kg3seed=ifelse(ag_g04b=="3 kg bag",3,0)
Maize_0.5topdownpercentout$kg3.7seed=ifelse(ag_g04b=="3.7 kg bag",3.7,0)
Maize_0.5topdownpercentout$kg5seed=ifelse(ag_g04b=="5 kg bag",5,0)
Maize_0.5topdownpercentout$kg10seed=ifelse(ag_g04b=="10 kg bag",10,0)
Maize_0.5topdownpercentout$kg50seed=ifelse(ag_g04b=="50 KG BAG",50,0)

#Maize_0.5topdownpercentout$kg90seed=ifelse(ag_g04b=="90 KG BAG",90,0)
attach(Maize_0.5topdownpercentout)

Maize_0.5topdownpercentout$maizeseed=gramseed*ag_g04a+kgseed*ag_g04a+kg2seed*ag_g04a+
  kg3seed*ag_g04a+kg3.7seed*ag_g04a+kg5seed*ag_g04a+kg10seed*ag_g04a+kg50seed*ag_g04a

attach(Maize_0.5topdownpercentout)

Maize_0.5topdownpercentout$maizeseedkg_ha=maizeseed/hectaresgps

attach(Maize_0.5topdownpercentout)


summary(maizeseedkg_ha)

dev.off()
oct=subset(Maize_0.5topdownpercentout$yield_kg_ha,ag_g05a=="October")
nov=subset(Maize_0.5topdownpercentout$yield_kg_ha,ag_g05a=="November")
dec=subset(Maize_0.5topdownpercentout$yield_kg_ha,ag_g05a=="December")
jan=subset(Maize_0.5topdownpercentout$yield_kg_ha,ag_g05a=="January")
feb=subset(Maize_0.5topdownpercentout$yield_kg_ha,ag_g05a=="February")

plot(density(dec),xlab="Maize Yield(Kg/ha) ",ylab="p(yield)",main="", col="1",xlim=c(0,10000))
lines(density(oct), col="2")
lines(density(nov), col="3")
lines(density(jan), col="4")
lines(density(feb), col="5")


legend("right",legend=c("December","October","November","January","February"),
       col=c("1","2","3","4","5"),lty=rep(1,6),title="Planting Month")

# Fertlizer Amounts
require(dplyr)
Maize_0.5topdownpercentout %>%
  mutate(ag_d39d=ifelse(is.na(ag_d39d),0,ag_d39d))
Maize_0.5topdownpercentout %>%
  mutate(ag_d39i=ifelse(is.na(ag_d39i),0,ag_d39i))


summary(Maize_0.5topdownpercentout$ag_d39i)


Maize_0.5topdownpercentout$totalfertuse=Maize_0.5topdownpercentout$ag_d39d+Maize_0.5topdownpercentout$ag_d39i
attach(Maize_0.5topdownpercentout)
summary(totalfertuse)
Maize_0.5topdownpercentout$totalfertusekg_ha=totalfertuse/hectaresgps
attach(Maize_0.5topdownpercentout)
summary(totalfertusekg_ha)
Maize_0.5topdownpercentout$totalfertusekg_ha_sq=totalfertusekg_ha*totalfertusekg_ha
attach(Maize_0.5topdownpercentout)
# Soil quality
CrossTable(ag_d21)

#Summary Table




## Summary Statistics
tabular(Format(digits=2)*(rexpaggcap+hhsize+dist_hh+dist_road+dist_popcenter+dist_admarc+dist_auction+dist_boma+dist_borderpost+hectaresgps+totalfertusekg_ha+yield_kg_ha)*(mean+min+max)~region,data=Maize_0.5topdownpercentout)




###

coords <- as.matrix(Maize_0.5topdownpercentout[,c("lon_modified","lat_modified")])
plot(coords, pch=1,col="darkgreen", xlab="Longitude", ylab="Latitude")

x.res <- 100; y.res <- 100
surf <- mba.surf(cbind(coords, Maize_0.5topdownpercentout$totalfertusekg_ha), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r", yaxs = "r", xlab="Longitude", ylab="Latitude")

# Regression Model

olsmodelagronomic=lm(Maize_0.5topdownpercentout$yield_kg_ha~totalfertusekg_ha+totalfertusekg_ha_sq+maizeseedkg_ha+hectaresgps+crop_id)

summary(olsmodelagronomic)

olsmodelagronomic_soci=lm(Maize_0.5topdownpercentout$yield_kg_ha~totalfertusekg_ha+totalfertusekg_ha_sq+dist_hh+maizeseedkg_ha+ag_d22+hectaresgps+ag_d36+ag_d38+crop_id+head_gender+poor)

summary(olsmodelagronomic_soci)
library(texreg)
# Bayesian Regression model
#plotreg(olsmodelagronomic)



a=list(olsmodelagronomic,olsmodelagronomic_soci)
htmlreg(a,file="frequentist.doc")





# Bayesian Model
n.samples=10
cand.1=bayesLMRef(olsmodelagronomic,n.samples)
summary(cand.1)

#
p <- 2 ## This is the number of columns in the design matrix
## Set the prior mean and precision for the regression
beta.prior.mean <- as.matrix(rep(0, times=p))
beta.prior.precision <- matrix(0, nrow=p, ncol=p)

## For use with bayesGeostatExact, do the following
phi <- 0.014 ## Set the spatial range (from the variogram)
alpha <- 0.016/0.08 ## Set the nugget/partial-sill ratio
sigma.sq.prior.shape <- 2.0 ## Set IG shape for sigma.sq (partial sill)
sigma.sq.prior.rate <- 0.08 ## Set IG scale for sigma.sq (partial sill)


memory.limit(size = 20000)

options(width = 65)
library(fields)
library(cluster)
library(classInt)
library(xtable)

# m=200,1000 samples
m <- 20
km.knots <- kmeans(coords, m)$centers
cl.knots <- clara(coords, m)$medoids


plot(coords, pch=19, cex=0.5, xlab="Easting (m)", ylab="Northing (m)",main="200 knots,n=1000")
points(km.knots, pch=5, cex=1, col="blue")
points(cl.knots, pch=6, cex=1, col="green")
legend("bottomleft", cex=1, pch=c(19,5,6), bty="n", col=c("black","blue","green"), legend=c("observations","kmeans","clara"))

coords2=as.matrix(coords)
n.samples <- 10

# With 200 knots_ First Chain
bef.sp <- spLM(Maize_0.5topdownpercentout$yield_kg_ha~totalfertusekg_ha,
               data=Maize_0.5topdownpercentout,coords=coords2, starting=list("phi"=3/200,"sigma.sq"=0.08,
                                                                             "tau.sq"=0.02), tuning=list("phi"=0.05, "sigma.sq"=0.05, "tau.sq"=0.05),
               priors=list("phi.Unif"=c(3/1000,3/50), "sigma.sq.IG"=c(2, 0.08),"tau.sq.IG"=c(2, 0.02)), cov.model="gaussian",n.samples=n.samples)

round(summary(mcmc(bef.sp$p.theta.samples))$quantiles,3)
round(summary(mcmc(cbind(bef.sp$p.beta.samples,bef.sp$p.theta.samples)))$quantiles=c(0.025,0.50,0.975),3)




quantiles = c(0.025,0.50, 0.975)

# CONVERSION FACTORS ----------------------------------------
setwd("G:/My Drive/2021 Malawi Bayes Fertilizer/Data/dta")

kgfactor=read.dta13("kgfactor.dta",generate.factors=T)
get.varlabel(kgfactor)
attach(kgfactor)

# LEGUMES DATA MANAGEMENT ---------------------------------

# Groundnuts
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTCG7=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUNDNUT CG7")

HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTCHALIMBANA=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUNDNUT CHALIMBANA")
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTJL24=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUNDNUT JL24")
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTMANIPINTA=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUNDNUT MANIPINTA")
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTMAWANGA=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUNDNUT MAWANGA")

GNUTData=bind_rows(HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTCG7,HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTCHALIMBANA,HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTJL24,HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTMANIPINTA,HHgeo_plotgeo_summary_C_D_GminsmallGROUNDNUTMAWANGA)
GNUTData=as.data.frame(GNUTData)


library(car)    
x=4.08/4.38
18.49*x

GNUTData$conversions=recode(GNUTData$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=4.08;'PAIL (LARGE)'=17.22356;'NO. 10 PLATE'=0.11; 'NO. 12 PLATE'=0.25; 'BALE'=150; 'BASKET (DENGU)'=50; 'OX - CART'=416.6;'OTHER (SPECIFY)'=NA", as.numeric.result=TRUE,as.factor.result = FALSE)           

# Groundnut prodn
GNUTData$prodn=GNUTData$conversions*GNUTData$ag_g13a
# Groundnut yield
GNUTData$yield_kg_ha=GNUTData$prodn/GNUTData$hectaresgps




###################### Other Legumes 
####################Beans
HHgeo_plotgeo_summary_C_D_GminsmallBEANS=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="BEANS")
HHgeo_plotgeo_summary_C_D_GminsmallBEANS$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallBEANS$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=3.285;'PAIL (LARGE)'=13.8675;'NO. 10 PLATE'=0.15; 'NO. 12 PLATE'=0.24; 'BASKET (DENGU)'=50; 'OX - CART'=416.6;'OTHER (SPECIFY)'=NA", as.numeric.result=TRUE,as.factor.result = FALSE) 

# Beans prodn
HHgeo_plotgeo_summary_C_D_GminsmallBEANS$prodn=HHgeo_plotgeo_summary_C_D_GminsmallBEANS$conversions*HHgeo_plotgeo_summary_C_D_GminsmallBEANS$ag_g13a
# Beans yield
HHgeo_plotgeo_summary_C_D_GminsmallBEANS$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallBEANS$prodn/HHgeo_plotgeo_summary_C_D_GminsmallBEANS$hectaresgps

# Ground Beans
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="GROUND BEAN(NZAMA")

HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'PAIL (SMALL)'=3.066;'PAIL (LARGE)'=12.943;'NO. 10 PLATE'=0.14; 'NO. 12 PLATE'=0.24;'OX - CART'=416.6;'OTHER (SPECIFY)'=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

# Ground Beans prodn
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$prodn=HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$conversions*HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$ag_g13a
# Ground Beans yield
HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$prodn/HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN$hectaresgps


# Peas
HHgeo_plotgeo_summary_C_D_GminsmallPEA=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="PEA")
HHgeo_plotgeo_summary_C_D_GminsmallPEA$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallPEA$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=3.066;'PAIL (LARGE)'=12.943;'BASKET (DENGU)'=50;'OX - CART'=416.6;'OTHER (SPECIFY)'=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

# Peas prodn
HHgeo_plotgeo_summary_C_D_GminsmallPEA$prodn=HHgeo_plotgeo_summary_C_D_GminsmallPEA$conversions*HHgeo_plotgeo_summary_C_D_GminsmallPEA$ag_g13a
# Peas yield
HHgeo_plotgeo_summary_C_D_GminsmallPEA$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallPEA$prodn/HHgeo_plotgeo_summary_C_D_GminsmallPEA$hectaresgps


# Pigeon peas
HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="PIGEONPEA(NANDOLO")
HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=2.847;'PAIL (LARGE)'=12.0185;'NO. 10 PLATE'=0.13; 'NO. 12 PLATE'=0.32;'BASKET (DENGU)'=50;'OX - CART'=416.6;else=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

# Pigeon Peas prodn
HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$prodn=HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$conversions*HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$ag_g13a
# Pigeon Peas yield
HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$prodn/HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA$hectaresgps

# Soybean
HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="SOYABEAN")
HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=2.847;'PAIL (LARGE)'=12.0185;'BASKET (DENGU)'=50;'OX - CART'=416.6;else=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

# Soybean Prodn
HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$prodn=HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$conversions*HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$ag_g13a
# Soybean yield
HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$prodn/HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN$hectaresgps


# MAIZE DATA MANAGEMENT ------------------------------ 
HHgeo_plotgeo_summary_C_D_GminsmallLocalMaize=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE LOCAL")
HHgeo_plotgeo_summary_C_D_GminsmallOPV=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE COMPOSITE/OPV")
HHgeo_plotgeo_summary_C_D_GminsmallHybrid=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE HYBRID")
HHgeo_plotgeo_summary_C_D_GminsmallHybridRecycled=filter(HHgeo_plotgeo_summary_C_D_Gminsmall,HHgeo_plotgeo_summary_C_D_Gminsmall$crop_id=="MAIZE HYBRID RECYCLED")

HHgeo_plotgeo_summary_C_D_GminsmallMaize=bind_rows(HHgeo_plotgeo_summary_C_D_GminsmallLocalMaize,HHgeo_plotgeo_summary_C_D_GminsmallOPV,HHgeo_plotgeo_summary_C_D_GminsmallHybrid,HHgeo_plotgeo_summary_C_D_GminsmallHybridRecycled)

HHgeo_plotgeo_summary_C_D_GminsmallMaize$conversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallMaize$ag_g13b,"'KILOGRAM'=1;'50 KG BAG'=50;'90 KG BAG'=90;'PAIL (SMALL)'=4.38;'PAIL (LARGE)'=18.49;'NO. 10 PLATE'=0.20;'NO. 12 PLATE'=0.33; 'BASKET (DENGU)'=50;'OX - CART'=416.6;else=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

# Computing production values for maize
HHgeo_plotgeo_summary_C_D_GminsmallMaize$prodn=HHgeo_plotgeo_summary_C_D_GminsmallMaize$conversions*HHgeo_plotgeo_summary_C_D_GminsmallMaize$ag_g13a
# Computing yield (kg) per ha for maize
HHgeo_plotgeo_summary_C_D_GminsmallMaize$yield_kg_ha=HHgeo_plotgeo_summary_C_D_GminsmallMaize$prodn/HHgeo_plotgeo_summary_C_D_GminsmallMaize$hectaresgps

 
# APPENDING MAIZE AND LEGUME DATA ---------------------------

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes=bind_rows(HHgeo_plotgeo_summary_C_D_GminsmallMaize,
                                                          GNUTData,
                                                          HHgeo_plotgeo_summary_C_D_GminsmallBEANS,
                                                          HHgeo_plotgeo_summary_C_D_GminsmallGROUNDBEAN,
                                                          HHgeo_plotgeo_summary_C_D_GminsmallPEA,
                                                          HHgeo_plotgeo_summary_C_D_GminsmallPIGEONPEA,
                                                          HHgeo_plotgeo_summary_C_D_GminsmallSOYABEAN)
#save.image("C:/Users/mkond001/OneDrive/6_Spring 2016/APEC 8904/Paper/Data and Models/MaizeLegumePlotLevelData.RData")


# Data Management for fertilizer and seeds variables ----------

#Seeds
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$seedconversions=recode(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_g04b, "'Gram'=0.001;'Kilogram'=1;'2 kg bag'=2;'3 kg bag'=3;'3.7 kg bag'=3.7;'5 kg bag'=5;'10 kg bag'=10;'50 kg bag'=50;else=NA", as.numeric.result=TRUE,as.factor.result = FALSE)

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$seedkg=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_g04a*HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$seedconversions

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$seedkg_ha=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$seedkg/HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$hectaresgps


#Fertilizer

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes=as.data.frame(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)
attach(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)
require(dplyr)

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert1 <- replace(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_d39d,which(is.na(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_d39d)),0)

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert2 <- replace(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_d39i,which(is.na(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$ag_d39i)),0)

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertuse=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert1+HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert2  

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertuse_kg_ha= HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertuse/HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$hectaresgps

#### TotalNused 
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalNuse=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert1*0.23+HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert2*0.46  

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalNuse_kg_ha= HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalNuse/HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$hectaresgps

#### Total phosphorus used
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalPuse=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert1*0.21

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalPuse_kg_ha= HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalPuse/HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$hectaresgps

## Total surphur used
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalSuse=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$fert1*0.04

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalSuse_kg_ha= HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalSuse/HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$hectaresgps

# SEPARATE OLS REGRESSION MODELS ---------------
library(bayesm)
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertusekg_ha_sq=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertuse_kg_ha*HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$totalfertuse_kg_ha
attach(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)

# Models
#OLS Model

olsmodel=lm(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+dist_hh+ag_d22+hectaresgps+ag_d36+ag_d38+crop_id+head_gender+poor)
summary(olsmodel)

library(nlme)

MaizeLegume=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes[ , c("yield_kg_ha", "totalfertuse_kg_ha","totalfertusekg_ha_sq","seedkg_ha","ag_g01","crop_id")] 

separateOLSmodel <- lmList(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha+ag_g01|crop_id, data=MaizeLegume,na.action=na.omit)

library(texreg)

htmlreg(separateOLSmodel,file="separateOLSmodel.doc",doctype=TRUE,html.tag=TRUE,head.tag=TRUE,body.tag=TRUE)

separateOLSmodel

plot(intervals(separateOLSmodel))

Mean <- function(x) base::mean(x, na.rm=TRUE)
tabular(Format(digits=2)*(rexpaggcap+hhsize+dist_hh+head_age+quintil+dist_road+dist_popcenter+dist_admarc+dist_auction+dist_boma+dist_borderpost+hectaresgps+totalfertuse_kg_ha)*(Mean)~region+1,data=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)

library(MCMCpack)
help(MCMCregress)


maizevars <- c("MAIZE LOCAL", "MAIZE HYBRID","MAIZE COMPOSITE/OPV","MAIZE HYBRID RECYCLED")
Maize=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id %in% maizevars) 
Maize$variety=Maize$crop_id
attach(Maize)


gnutvars=c("GROUNDNUT CG7","GROUNDNUT CHALIMBANA","GROUNDNUT JL24","GROUNDNUT MANIPINTA","GROUNDNUT MAWANGA")
Groundnuts=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id %in% gnutvars) 
Groundnuts$variety=Groundnuts$crop_id


CrossTable(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id,HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$region, digits=2, prop.c=TRUE,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE)

BEANS=c("BEANS")
Beans=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id %in% BEANS)

PIGEONPEAS=c("PIGEONPEA(NANDOLO")
Pigionpeas=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id %in% PIGEONPEAS)


SOYABEANS=c("SOYABEAN")
Soyabeans=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id %in% SOYABEANS)

#Maize Regression
posteriorm  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha+ag_g01+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+variety+district, data=Maize, verbose=TRUE)
plot(posteriorm)
raftery.diag(posteriorm)
summary(posteriorm,quantiles = c(0.025,0.50, 0.975))

# # Hierarchial Maize Production Function
# 
# posteriorhier  <- MCMChregress(fixed=yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha+ag_g01+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+variety, random= ~totalfertuse_kg_ha+totalfertusekg_ha_sq,data=Maize, verbose=0,  group = "district",r=40,nu=0.001,delta=0.001)


# Maize Regression with Experimental Prior
B=matrix(0,40,40)
B[3,3]=0.001

Bo=c()
posteriorme  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha+ag_g01+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+variety+district
                            
                            ,data=Maize, verbose=TRUE, b0=c(0,400,0,0,0,0,0,0,0,0,0,0,
                                                            0,0,0,0,0,0,0,0,0,0,
                                                            0,0,0,0,0,0,0,0,0,0,
                                                            0,0,0,0,0,0,0,0),B0=B)
plot(posteriorme)
raftery.diag(posteriorme)

summary(posteriorme,quantiles = c(0.025,0.50, 0.975))

#+seedkg_ha+ag_g01+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+variety+district
#Maize Regression with Previous Survey Priors

posteriorm1  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha
                            
                            ,data=Maize, verbose=TRUE)
B=matrix(0,2,2)
B[2,2]=0.0001

posteriorm2  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha
                            
                            ,data=Maize, verbose=TRUE, b0=c(0,400),B0=B)

summary(posteriorm1,quantiles = c(0.025,0.50, 0.975))
summary(posteriorm2,quantiles = c(0.025,0.50, 0.975))

BayesFactor(posteriorm, posteriorme)

#R> summary(BF)
#
#
#


#Groundnut Regression

posteriorg  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+variety+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, data=Groundnuts, verbose=TRUE)

plot(posteriorg)
raftery.diag(posteriorg)
summary(posteriorg, quantiles = c(0.025,0.50, 0.975))

##Beans regression
attach(Beans)
posteriorb  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, data=Beans, verbose=TRUE)

plot(posteriorb)
raftery.diag(posteriorb)
summary(posteriorb, quantiles = c(0.025,0.50, 0.975))

## PigeonPeas Regression
attach(Pigionpeas)
posteriorp  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, data=Pigionpeas, verbose=TRUE)

plot(posteriorp)
raftery.diag(posteriorp)
summary(posteriorp, quantiles = c(0.025,0.50, 0.975))

## Soyabeans Regressions
attach(Soyabeans)
posteriors  <- MCMCregress(yield_kg_ha~totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, data=Soyabeans, verbose=TRUE)

plot(posteriors)
raftery.diag(posteriors)
summary(posteriors, quantiles = c(0.025,0.50, 0.975))



library(help=MCMCpack)







# RAY PRODUCTION APPROACH -------------------------------------

# HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes %>%
#     group_by(cased_plotid) %>%
#     summarise(yieldsq = sum(yield_kg_ha))
# 
# split(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$yield_kg_ha, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id)



# maize <- c("MAIZE LOCAL", "MAIZE HYBRID","MAIZE COMPOSITE/OPV","MAIZE HYBRID RECYCLED")
# 
# gnut=c("GROUNDNUT CG7","GROUNDNUT CHALIMBANA","GROUNDNUT JL24","GROUNDNUT MANIPINTA","GROUNDNUT MAWANGA")
# 
# library(car)
# 
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop_id=HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop

attach(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)
edit(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes)

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="MAIZE LOCAL"] <- "Maize"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="MAIZE HYBRID"] <- "Maize"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="MAIZE COMPOSITE/OPV"] <- "Maize"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="MAIZE HYBRID RECYCLED"] <- "Maize"

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="GROUNDNUT CG7"] <- "Groundnuts"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="GROUNDNUT CHALIMBANA"] <- "Groundnuts"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="GROUNDNUT JL24"] <- "Groundnuts"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="GROUNDNUT MANIPINTA"] <- "Groundnuts"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="GROUNDNUT MAWANGA"] <- "Groundnuts"

HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="BEANS"] <- "Beans"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="PIGEONPEA(NANDOLO"] <- "Pigeonpeas"
HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop[HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop=="SOYABEAN"] <- "Soyabeans"



crops=c("Maize","Groundnuts","Beans","Pigeonpeas","Soyabeans")
Maizefivelegumesdata=filter(HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes, HHgeo_plotgeo_summary_C_D_GminsmallMaizeLegumes$crop %in% crops)
attach(Maizefivelegumesdata)

for(t in unique(Maizefivelegumesdata$crop)) {
  Maizefivelegumesdata[paste("crop",t,sep="")] <- ifelse(Maizefivelegumesdata$crop==t,1,0)
}

edit(Maizefivelegumesdata)

Maizefivelegumesdata$maizeyield=Maizefivelegumesdata$cropMaize*Maizefivelegumesdata$yield_kg_ha
Maizefivelegumesdata$groundnutsyield=Maizefivelegumesdata$cropGroundnuts*Maizefivelegumesdata$yield_kg_ha
Maizefivelegumesdata$beansyield=Maizefivelegumesdata$cropBeans*Maizefivelegumesdata$yield_kg_ha
Maizefivelegumesdata$Pigeonpeasyield=Maizefivelegumesdata$cropPigeonpeas*Maizefivelegumesdata$yield_kg_ha
Maizefivelegumesdata$Soyabeansyield=Maizefivelegumesdata$cropSoyabeans*Maizefivelegumesdata$yield_kg_ha

attach(Maizefivelegumesdata)

# Labor Variable ------------------------------------
names(Maizefivelegumesdata)

Maizefivelegumesdata$landprepfamlab1=Maizefivelegumesdata$ag_d42b*Maizefivelegumesdata$ag_d42b*Maizefivelegumesdata$ag_d42d
Maizefivelegumesdata$landprepfamlab2=Maizefivelegumesdata$ag_d42f*Maizefivelegumesdata$ag_d42g*Maizefivelegumesdata$ag_d42h
Maizefivelegumesdata$landprepfamlab3=Maizefivelegumesdata$ag_d42j*Maizefivelegumesdata$ag_d42k*Maizefivelegumesdata$ag_d42l
Maizefivelegumesdata$landprepfamlab4=Maizefivelegumesdata$ag_d42n*Maizefivelegumesdata$ag_d42o*Maizefivelegumesdata$ag_d42p

require(dplyr)
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab1 = ifelse(is.na(landprepfamlab1),0,landprepfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab2 = ifelse(is.na(landprepfamlab2),0,landprepfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab3 = ifelse(is.na(landprepfamlab3),0,landprepfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab4 = ifelse(is.na(landprepfamlab4),0,landprepfamlab4))

Maizefivelegumesdata$landprepfamlab=Maizefivelegumesdata$landprepfamlab1+Maizefivelegumesdata$landprepfamlab2+Maizefivelegumesdata$landprepfamlab3+
  Maizefivelegumesdata$landprepfamlab4


Maizefivelegumesdata$weedplusfamlab1=Maizefivelegumesdata$ag_d43b*Maizefivelegumesdata$ag_d43b*Maizefivelegumesdata$ag_d43d
Maizefivelegumesdata$weedplusfamlab2=Maizefivelegumesdata$ag_d43f*Maizefivelegumesdata$ag_d43g*Maizefivelegumesdata$ag_d43h
Maizefivelegumesdata$weedplusfamlab3=Maizefivelegumesdata$ag_d43j*Maizefivelegumesdata$ag_d43k*Maizefivelegumesdata$ag_d43l
Maizefivelegumesdata$weedplusfamlab4=Maizefivelegumesdata$ag_d43n*Maizefivelegumesdata$ag_d43o*Maizefivelegumesdata$ag_d43p

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab1 = ifelse(is.na(weedplusfamlab1),0,weedplusfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab2 = ifelse(is.na(weedplusfamlab2),0,weedplusfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab3 = ifelse(is.na(weedplusfamlab3),0,weedplusfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab4 = ifelse(is.na(weedplusfamlab4),0,weedplusfamlab4))

Maizefivelegumesdata$weedplusfamlab=Maizefivelegumesdata$weedplusfamlab1+Maizefivelegumesdata$weedplusfamlab2+Maizefivelegumesdata$weedplusfamlab3+Maizefivelegumesdata$weedplusfamlab4

Maizefivelegumesdata$harvfamlab1=Maizefivelegumesdata$ag_d44b*Maizefivelegumesdata$ag_d44b*ag_d44d
Maizefivelegumesdata$harvfamlab2=Maizefivelegumesdata$ag_d44f*Maizefivelegumesdata$ag_d44g*ag_d44h
Maizefivelegumesdata$harvfamlab3=Maizefivelegumesdata$ag_d44j*Maizefivelegumesdata$ag_d44k*ag_d44l
Maizefivelegumesdata$harvfamlab4=Maizefivelegumesdata$ag_d44n*Maizefivelegumesdata$ag_d44o*ag_d44p

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab1 = ifelse(is.na(harvfamlab1),0,harvfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab2 = ifelse(is.na(harvfamlab2),0,harvfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab3 = ifelse(is.na(harvfamlab3),0,harvfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab4 = ifelse(is.na(harvfamlab4),0,harvfamlab4))


Maizefivelegumesdata$harvfamlab=Maizefivelegumesdata$harvfamlab1+Maizefivelegumesdata$harvfamlab2+Maizefivelegumesdata$harvfamlab3+Maizefivelegumesdata$harvfamlab4

Maizefivelegumesdata$totalfamilylabor=Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab

attach(Maizefivelegumesdata)

# Output norm variable

edit(Maizefivelegumesdata)


# Land and Labor Productivity 
Maizefivelegumesdata$prodnperlaborhour=Maizefivelegumesdata$prodn/Maizefivelegumesdata$totalfamilylabor
Maizefivelegumesdata$loglandprodty=log(Maizefivelegumesdata$maizeyield)
Maizefivelegumesdata$loglaborprodty=log(Maizefivelegumesdata$prodnperlaborhour)
plot(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty)

#
Maizefivelegumesdata$landperprodn=Maizefivelegumesdata$hectaresgps/Maizefivelegumesdata$prodn
Maizefivelegumesdata$loglandperprodn=log(Maizefivelegumesdata$landperprodn)

Maizefivelegumesdata$laborperprodn=Maizefivelegumesdata$totalfamilylabor/Maizefivelegumesdata$prodn
Maizefivelegumesdata$loglaborperprodn=log(Maizefivelegumesdata$laborperprodn)

library(ggplot2)
ruttanplotdata=cbind(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty,Maizefivelegumesdata$loglandperprodn,Maizefivelegumesdata$loglaborperprodn)


pairs(ruttanplotdata, pch = 21)
plotmatrix(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty,Maizefivelegumesdata$loglandperprodn,Maizefivelegumesdata$loglaborperprodn)


library(car)
scatterplotMatrix(~loglandprodty,loglaborprodty|crop, data=Maizefivelegumesdata,
                  main="Land and labor productivity by crop")






# SUR Model
library(systemfit)
system <- list(maizeyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, 
               
               groundnutsyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
               
               beansyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
               Pigeonpeasyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
               Soyabeansyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district)

#OLS estimation:
fitOls <- systemfit(system, data = Maizefivelegumesdata )
round( coef( summary( fitOls ) ), digits = 4 )

a=list(fitOls)
library(texreg)
htmlreg(a,file="sols")
#

# With Total Family Labor Only
system2 <- list(maizeyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, 
                
                groundnutsyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
                
                beansyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
                Pigeonpeasyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
                Soyabeansyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district)


#OLS estimation:
fitOls2 <- systemfit(system2, data = Maizefivelegumesdata )
round( coef( summary( fitOls2 ) ), digits = 4 )

b=list(fitOls2)
library(texreg)
htmlreg(b,file="sols2")

+aez+twi_mwi
#
Maizefivelegumesdatanfinal$elevation= Maizefivelegumesdatanfinal$srtm_eaf
Maizefivelegumesdatanfinal$slope= Maizefivelegumesdatanfinal$afmnslp_pct
Maizefivelegumesdatanfinal$rainfall= Maizefivelegumesdatanfinal$anntot_avg
Maizefivelegumesdatanfinal$aez= Maizefivelegumesdatanfinal$ssa_aez09
Maizefivelegumesdatanfinal$topographicalwetness= Maizefivelegumesdatanfinal$twi_mwi

## 
system3 <- list(yieldmaizeall~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+elevation+slope+rainfall+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+dist_road+district, 
                
                yieldgroudnutsall~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+elevation+slope+rainfall+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+dist_road+district,
                
                yieldbeansall~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+elevation+slope+rainfall+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+dist_road+district,
                yieldpigeonpeasall~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+elevation+slope+rainfall+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+dist_road+district,
                yieldsoyabeansall~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+elevation+slope+rainfall+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+dist_road+district)


#SUR estimation:



#
library(systemfit)
fitOls3 <- systemfit(system3, data = Maizefivelegumesdatanfinal, method="SUR" )
round( coef( summary( fitOls3 ) ), digits = 4 )

library(texreg)
htmlreg(list(fitOls3),file="sur3")
plotreg(fitOls3,omit.coef="(Intercept)|ag_d36|totalfamilylaborperha|seedkg_ha|ag_g01|soiltype|soilquality|hectaresgps|head_gender|head_age|hhsize|poor|head_edlevel|dist_hh|district" )
dev.off()

#normedoutput~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+district,data=Maizefivelegumesdatanfinal
# FROM LABOR ECONOMICS CODES
names(Maizefivelegumesdata)
library(dplyr)

Maizefivelegumesdata$landprepfamlab1=Maizefivelegumesdata$ag_d42b*Maizefivelegumesdata$ag_d42b*Maizefivelegumesdata$ag_d42d
Maizefivelegumesdata$landprepfamlab2=Maizefivelegumesdata$ag_d42f*Maizefivelegumesdata$ag_d42g*Maizefivelegumesdata$ag_d42h
Maizefivelegumesdata$landprepfamlab3=Maizefivelegumesdata$ag_d42j*Maizefivelegumesdata$ag_d42k*Maizefivelegumesdata$ag_d42l
Maizefivelegumesdata$landprepfamlab4=Maizefivelegumesdata$ag_d42n*Maizefivelegumesdata$ag_d42o*Maizefivelegumesdata$ag_d42p

require(dplyr)
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab1 = ifelse(is.na(landprepfamlab1),0,landprepfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab2 = ifelse(is.na(landprepfamlab2),0,landprepfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab3 = ifelse(is.na(landprepfamlab3),0,landprepfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(landprepfamlab4 = ifelse(is.na(landprepfamlab4),0,landprepfamlab4))

Maizefivelegumesdata$landprepfamlab=Maizefivelegumesdata$landprepfamlab1+Maizefivelegumesdata$landprepfamlab2+Maizefivelegumesdata$landprepfamlab3+
  Maizefivelegumesdata$landprepfamlab4


Maizefivelegumesdata$weedplusfamlab1=Maizefivelegumesdata$ag_d43b*Maizefivelegumesdata$ag_d43b*Maizefivelegumesdata$ag_d43d
Maizefivelegumesdata$weedplusfamlab2=Maizefivelegumesdata$ag_d43f*Maizefivelegumesdata$ag_d43g*Maizefivelegumesdata$ag_d43h
Maizefivelegumesdata$weedplusfamlab3=Maizefivelegumesdata$ag_d43j*Maizefivelegumesdata$ag_d43k*Maizefivelegumesdata$ag_d43l
Maizefivelegumesdata$weedplusfamlab4=Maizefivelegumesdata$ag_d43n*Maizefivelegumesdata$ag_d43o*Maizefivelegumesdata$ag_d43p

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab1 = ifelse(is.na(weedplusfamlab1),0,weedplusfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab2 = ifelse(is.na(weedplusfamlab2),0,weedplusfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab3 = ifelse(is.na(weedplusfamlab3),0,weedplusfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(weedplusfamlab4 = ifelse(is.na(weedplusfamlab4),0,weedplusfamlab4))

Maizefivelegumesdata$weedplusfamlab=Maizefivelegumesdata$weedplusfamlab1+Maizefivelegumesdata$weedplusfamlab2+Maizefivelegumesdata$weedplusfamlab3+Maizefivelegumesdata$weedplusfamlab4

Maizefivelegumesdata$harvfamlab1=Maizefivelegumesdata$ag_d44b*Maizefivelegumesdata$ag_d44b*Maizefivelegumesdata$ag_d44d
Maizefivelegumesdata$harvfamlab2=Maizefivelegumesdata$ag_d44f*Maizefivelegumesdata$ag_d44g*Maizefivelegumesdata$ag_d44h
Maizefivelegumesdata$harvfamlab3=Maizefivelegumesdata$ag_d44j*Maizefivelegumesdata$ag_d44k*Maizefivelegumesdata$ag_d44l
Maizefivelegumesdata$harvfamlab4=Maizefivelegumesdata$ag_d44n*Maizefivelegumesdata$ag_d44o*Maizefivelegumesdata$ag_d44p

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab1 = ifelse(is.na(harvfamlab1),0,harvfamlab1))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab2 = ifelse(is.na(harvfamlab2),0,harvfamlab2))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab3 = ifelse(is.na(harvfamlab3),0,harvfamlab3))
Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(harvfamlab4 = ifelse(is.na(harvfamlab4),0,harvfamlab4))

Maizefivelegumesdata$harvfamlab=Maizefivelegumesdata$harvfamlab1+Maizefivelegumesdata$harvfamlab2+Maizefivelegumesdata$harvfamlab3+Maizefivelegumesdata$harvfamlab4

Maizefivelegumesdata$totalfamilylabor=Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab

attach(Maizefivelegumesdata)

# Land and Labor Productivity 
Maizefivelegumesdata$prodnperlaborhour=Maizefivelegumesdata$prodn/Maizefivelegumesdata$totalfamilylabor
Maizefivelegumesdata$loglandprodty=log(Maizefivelegumesdata$maizeyield)
Maizefivelegumesdata$loglaborprodty=log(Maizefivelegumesdata$prodnperlaborhour)
plot(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty)

#
Maizefivelegumesdata$landperprodn=Maizefivelegumesdata$hectaresgps/Maizefivelegumesdata$prodn
Maizefivelegumesdata$loglandperprodn=log(Maizefivelegumesdata$landperprodn)

Maizefivelegumesdata$laborperprodn=Maizefivelegumesdata$totalfamilylabor/Maizefivelegumesdata$prodn
Maizefivelegumesdata$loglaborperprodn=log(Maizefivelegumesdata$laborperprodn)

library(ggplot2)
#ruttanplotdata=cbind(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty,Maizefivelegumesdata$loglandperprodn,Maizefivelegumesdata$loglaborperprodn)
# pairs(ruttanplotdata, pch = 21)
# plotmatrix(Maizefivelegumesdata$loglandprodty,Maizefivelegumesdata$loglaborprodty,Maizefivelegumesdata$loglandperprodn,Maizefivelegumesdata$loglaborperprodn)
# library(car)
# scatterplotMatrix(~loglandprodty,loglaborprodty|crop, data=Maizefivelegumesdata,
#    main="Land and labor productivity by crop")
library(reporttools)
#vars <- Maizefivelegumesdata[,c("head_gender","region","poor","head_edlevel","soilquality","crop", "cropstand","urban")]
#tableNominal(vars = vars, , vertical = FALSE, longtable = FALSE)

# GRAPHICAL ANALYSIS USING HAYAMI & RUTTAN GRAPH
qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata, colour = district)
qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata, colour = crop)
qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata, colour = region)
#par(mfrow=c(2,2))
qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata,colour = region, geom = c("point", "smooth"),xlab="Log(land productivity, kg/ha)",ylab="Log(labor productivity,kg/hr)")

urban=qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata,colour = urban, geom = c("point", "smooth"),xlab="Log(land productivity, kg/ha)",ylab="Log(labor productivity,kg/hr)")
previous_theme <- theme_set(theme_bw())
urban

poor=qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata,colour = poor, geom = c("point", "smooth"),xlab="Log(land productivity, kg/ha)",ylab="Log(labor productivity,kg/hr)")
previous_theme <- theme_set(theme_bw())
poor

gender=qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata,colour = head_gender, geom = c("point", "smooth"),xlab="Log(land productivity, kg/ha)",ylab="Log(labor productivity,kg/hr)")
previous_theme <- theme_set(theme_bw())
gender


qplot(loglaborprodty, data = Maizefivelegumesdata, geom = "density", colour = crop)

qplot(loglandprodty, loglaborprodty, data=Maizefivelegumesdata,colour=factor(quintil), geom = c("point", "smooth"),xlab="Log(land productivity, kg/ha)",ylab="Log(labor productivity,kg/hr)")

qplot(loglaborprodty, data = Maizefivelegumesdata, geom = "density", colour = factor(quintil))

##inverse farm size and land productivity
Maizefivelegumesdata$lnhactaresgps=log(Maizefivelegumesdata$hectaresgps)

qplot(lnhactaresgps,loglandprodty, data=Maizefivelegumesdata,colour=head_gender, geom = c("point", "smooth"),ylab="Log(land productivity, kg/ha)",xlab="Log(hactarage,ha)")

## Inverse laborhours and labor productivity
Maizefivelegumesdata$lntotalfamilylabor=log(Maizefivelegumesdata$totalfamilylabor)

qplot(lntotalfamilylabor,loglaborprodty, data=Maizefivelegumesdata,colour=head_gender, geom = c("point", "smooth"),ylab="Log(labor productivity, kg/ha)",xlab="Log(Total family labor, hrs/year)")

#boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
#SUR Production Functions

# system <- list(maizeyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district, 
#                
#                 groundnutsyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
#                
#                beansyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
#                 Pigeonpeasyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district,
#                 Soyabeansyield~Maizefivelegumesdata$landprepfamlab+Maizefivelegumesdata$weedplusfamlab+Maizefivelegumesdata$harvfamlab+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+district)
# 
# #OLS estimation:
# fitOls <- systemfit(system, data = Maizefivelegumesdata )
# round( coef( summary( fitOls ) ), digits = 4 )
# 
# a=list(fitOls)
# library(texreg)
# htmlreg(a,file="sols")
# #

# With Total Family Labor Only
library(systemfit)
Maizefivelegumesdata$lnmaizeyield=log(Maizefivelegumesdata$maizeyield)
Maizefivelegumesdata$lngroundnutsyield=log(Maizefivelegumesdata$groundnutsyield)
Maizefivelegumesdata$lnbeansyield=log(Maizefivelegumesdata$beansyield)
Maizefivelegumesdata$lnPigeonpeasyield=log(Maizefivelegumesdata$Pigeonpeasyield)
Maizefivelegumesdata$lnSoyabeansyield=log(Maizefivelegumesdata$Soyabeansyield)

Maizefivelegumesdata$lntotalfamilylabor=log(Maizefivelegumesdata$totalfamilylabor)

Maizefivelegumesdata$soiltype=Maizefivelegumesdata$ag_d21
Maizefivelegumesdata$soilquality=Maizefivelegumesdata$ag_d22

Maizefivelegumesdata$cropstand=Maizefivelegumesdata$ag_g01

system2 <- list(maizeyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+urban, 
                
                groundnutsyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+urban,
                
                beansyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+urban,
                Pigeonpeasyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+urban,
                Soyabeansyield~Maizefivelegumesdata$totalfamilylabor+totalfertuse_kg_ha+totalfertusekg_ha_sq+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+urban)

#SUR estimation:
fitOls2 <- systemfit(system2,method="SUR",data = Maizefivelegumesdata )
round( coef( summary( fitOls2 ) ), digits = 4 )

b=list(fitOls2)
library(texreg)
htmlreg(b,file="sols2")

###### Calculating Output Norm
yNames = c( "maizeyield", "groundnutsyield", "beansyield","Pigeonpeasyield", "Soyabeansyield")

nOutput <- length(yNames)
distance <- 0

# norm=function (yNames,data) 
# {
#     for (i in 1:nOutput) {
#         distance <- distance + data[[yNames[i]]]^2
#     }
#     distance <- sqrt(distance)
# }


#ab=norm(yNames,data=Maizefivelegumesdata)

# ab=as.data.frame(ab)
# 
#  sinProd <- 1
#     for (i in 1:(nOutput - 1)) {
#         ratio <- data[[yNames[i]]]/(distance * sinProd)
#         ratio[ratio > 1] <- 1
#         ratio[ratio < -1] <- -1
#         data[[paste("theta", i, sep = "_")]] <- acos(ratio)
#         
#         sinProd <- sinProd * sin(data[[paste("theta", i, sep = "_")]])
#     }
# }
# 
# ac=thetas(yNames,data=Maizefivelegumesdata)

#Translog Ray
library(frontier)
estResultRay <- frontierTranslogRay( yNames = c( "yieldmaizeall", "yieldgroudnutsall","yieldbeansall","yieldpigeonpeasall","yieldsoyabeansall"),
                                     xNames = c( "totalfertuse_kg_ha", "totalfamilylaborperha" ),
                                     data = Maizefivelegumesdatanfinal )
summary( estResultRay )


##
Maizefivelegumesdatanfinal$normedoutput=estResultRay$distance

Maizefivelegumesdatanfinal$theta_1=estResultRay$theta_1
Maizefivelegumesdatanfinal$theta_2=estResultRay$theta_2
Maizefivelegumesdatanfinal$theta_3=estResultRay$theta_3
Maizefivelegumesdatanfinal$theta_4=estResultRay$theta_4

Maizefivelegumesdatanfinal$totalfamilylaborperha=Maizefivelegumesdatanfinal$totalfamilylabor/Maizefivelegumesdatanfinal$hectaresgps
Maizefivelegumesdatanfinal$organicfertuse=Maizefivelegumesdatanfinal$ag_d36

attach(Maizefivelegumesdatanfinal)
quadraticray=lm(normedoutput~theta_1+theta_2+theta_3+theta_4+totalfertuse_kg_ha+totalfertusekg_ha_sq+organicfertuse+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+rainfall+elevation+slope+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+district,data=Maizefivelegumesdatanfinal)

summary(quadraticray)

#Without Thetas
quadraticraynotheta=lm(normedoutput~totalfertuse_kg_ha+totalfertusekg_ha_sq+organicfertuse+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+rainfall+elevation+slope+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+district,data=Maizefivelegumesdatanfinal)

summary(quadraticraynotheta)
library(texreg)

# ##### With District Random Efffects
# library(nlme)
# quadraticrayrandom=lme(normedoutput~theta_1+theta_2+theta_3+theta_4+totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh,random =~1|district,data=Maizefivelegumesdatanfinal,na.action=na.rm)

quadraticrayall=htmlreg(list(quadraticray,quadraticraynotheta), file="RayProductionFunctions")

library(sfa)
library(frontier)
quadraticrayraystochastic <- sfa( normedoutput~totalfertuse_kg_ha+totalfertusekg_ha_sq+ag_d36+totalfamilylaborperha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+seedkg_ha*totalfertuse_kg_ha+ag_g01*totalfertuse_kg_ha+soiltype+soilquality+hectaresgps+head_gender+head_age+hhsize+poor+head_edlevel+dist_hh+district,data=Maizefivelegumesdatanfinal )

quadraticrayraystochastic=frontierTranslogRay(yName=c("yieldmaizeall","yieldgroudnutsall","yieldbeansall","yieldpigeonpeasall","yieldsoyabeansall"),xNames = c("hhsize"), data=Maizefivelegumesdatanfinal)

#"ag_d36","totalfamilylaborperha","seedkg_ha","ag_g01","soiltype","soilquality","hectaresgps","head_gender","head_age","hhsize","poor","head_edlevel","dist_hh","district")
summary( quadraticrayraystochastic )

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(maizeid = ifelse(duplicated(plot_crop_id),1,cropMaize))

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(groundnutsid = ifelse(duplicated(plot_crop_id),1,cropGroundnuts))

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(beansid = ifelse(duplicated(plot_crop_id),1,cropBeans))

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(pigeonpeasid = ifelse(duplicated(plot_crop_id),1,cropPigeonpeas))

Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(soyabeansid = ifelse(duplicated(plot_crop_id),1,cropSoyabeans))


Maizefivelegumesdata <- Maizefivelegumesdata %>%
  mutate(maizeid = ifelse(duplicated(plot_crop_id),maizeyield,maizeyield))


library(data.table)
library(dplyr)
dt1=data.table(Maizefivelegumesdata)

dt2=copy(dt1) [,list(yieldnorm=sqrt(sum(yield_kg_ha*yield_kg_ha))),by=.(plot_crop_id)]      

dtmaize=copy(dt1) [,list(yieldmaizeall=sqrt(sum(maizeyield*maizeyield))),by=.(plot_crop_id)] 

dtgroundnuts=copy(dt1) [,list(yieldgroudnutsall=sqrt(sum(groundnutsyield*groundnutsyield))),by=.(plot_crop_id)] 

dtbeans=copy(dt1) [,list(yieldbeansall=sqrt(sum(beansyield*beansyield))),by=.(plot_crop_id)]  

dtpigeonpeas=copy(dt1) [,list(yieldpigeonpeasall=sqrt(sum(Pigeonpeasyield*Pigeonpeasyield))),by=.(plot_crop_id)]

dtsoyabeans=copy(dt1) [,list(yieldsoyabeansall=sqrt(sum(Soyabeansyield*Soyabeansyield))),by=.(plot_crop_id)]  


Maizefivelegumesdatan=right_join(Maizefivelegumesdata,dt2,by="plot_crop_id")

Maizefivelegumesdatann=right_join(Maizefivelegumesdatan,dtmaize,by="plot_crop_id")

Maizefivelegumesdatannn=right_join(Maizefivelegumesdatann,dtgroundnuts,by="plot_crop_id")

Maizefivelegumesdatannnn=right_join(Maizefivelegumesdatannn,dtbeans,by="plot_crop_id")

Maizefivelegumesdatannnnn=right_join(Maizefivelegumesdatannnn,dtpigeonpeas,by="plot_crop_id")

Maizefivelegumesdatanfinal=right_join(Maizefivelegumesdatannnnn,dtsoyabeans,by="plot_crop_id")

