####################################################
Maizefivelegumesdatanfinal$hhid=as.factor(Maizefivelegumesdatanfinal$cased_plotid)
attach(Maizefivelegumesdatanfinal)

rayposteriorhierhhid=MCMChregress(fixed=yieldnorm~totalfertuse_kg_ha + totalfertusekg_ha_sq+organicfertuse + totalfamilylaborperha + seedkg_ha + ag_g01 + soiltype + soilquality + slope+rainfall+elevation+ hectaresgps + head_gender + head_age + 
                                    hhsize + poor + head_edlevel + dist_hh,
                                  
                                  random=~totalfertuse_kg_ha + totalfertusekg_ha_sq+organicfertuse + totalfamilylaborperha + seedkg_ha + ag_g01 + soiltype + soilquality + slope+rainfall+elevation+ hectaresgps + head_gender + head_age + 
                                    hhsize + poor + head_edlevel + dist_hh,data=Maizefivelegumesdatanfinal, verbose=1,
                                  
                                  group = "hhid",R=diag(24),r=24,nu=0.001,delta=0.001)

#################################################################
ride.constWood <- summary(rayposteriorhierhhid$mcmc[ , grepl("b.totalfertuse_kg_ha", 
                                                             colnames(rayposteriorhierhhid$mcmc))] + rayposteriorhierhhid$mcmc[ , "beta.totalfertuse_kg_ha"])

plot(density(ride.constWood$statistics[,1]),xlim=c(-50,50))

summary(ride.constWood$statistics[,1])
