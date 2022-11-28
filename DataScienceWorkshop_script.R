

install.packages('manipulate'); install.packages('raster'); install.packages('mgcv');


library(manipulate); library(raster); library(mgcv);





#Case 1: Land use change
manipulate(Year=slider(2006,2018),agplot(Year))





#Case 2: Climate change
manipulate(
  r1=checkbox(FALSE,"Alaska"),r3=checkbox(FALSE,"Egypt"),
  r4=checkbox(FALSE,"Greenland"),r6=checkbox(FALSE,"Patagonia"),r7=checkbox(FALSE,"USA"),
  What_to_Plot=picker("Mean Temperature","Max Temperature"),Climate_Scenario=picker("2.6 dergees warmer","8.5 dergees warmer"),
  plotfun(which(c(r1,FALSE,r3,r4,TRUE,r6,r7)),What_to_Plot,Climate_Scenario)
)

#You can explore these trends more here:
#https://climateknowledgeportal.worldbank.org/country/united-states/climate-data-projections






#Case 3: Do bigger mammals sleep more?
manipulate(Predicting_Variable=picker("Body weight","Brain weight","log Body weight","log Brain weight"),
           model=checkbox(FALSE,"Make a model of this relationship"),
           sleepplot(Predicting_Variable,model)
)






#Case 4: Kelp forest dynamics and ocean climate change

MyRandomNums=c(1)
gamdat$MyRandomNums=jitter(sample(MyRandomNums,nrow(gamdat),replace=TRUE))

manipulate(
  v1=checkbox(TRUE,VarNms[1]),v2=checkbox(FALSE,VarNms[2]),v3=checkbox(FALSE,VarNms[3]),
  v4=checkbox(FALSE,VarNms[4]),v5=checkbox(FALSE,VarNms[5]),v6=checkbox(FALSE,VarNms[6]),v7=checkbox(FALSE,VarNms[7]),
  Wiggliness=picker(7,-1,3,4,5,6,8,10,16),
  kelpplot(which(c(v1,v2,v3,v4,v5,v6,v7)),kmx=as.numeric(Wiggliness))
)













### APPENDIX 1: PLOTTING FUNCTIONS
#Case 1: Land use change
agplot=function(yr){
  #set plot colors: "white",    "red",    "pink", "blue",    "gray",   "green","yellow"
  custpal = c("#FFFFFF","#8B0000","#FF1493","#CDC5BF","#436EEE","#008B00","#EE9A00")
  #set plot area and x-axis
  layout(matrix(c(1,1,2,1,1,2),2,3,TRUE)); Yrs=2006:2018; yri=which(Yrs==yr);
  #plot land use in given year
  image(raster(crp[[yri]]),col=custpal,axes=FALSE,xlab="West to East",ylab="South to North",cex.lab=2,main=paste0("Marion, KS reservoir watershed in ",yr),cex.main=2)
  legend("topright",c("Corn","Soybeans","Nature/Pasture","Other crops"),cex=2,seg.len=0.75,col=custpal[-c(1,4,5)],lwd=10,box.col="transparent",bg="transparent")
  #plot time trend in land use up to given year
  plot(Yrs,rep(-100,length(Yrs)),ylim=c(0,60),frame.plot=F,main="Percent of cropland",las=1,ylab="",cex.main=2,xlab="Year",cex.axis=2,cex.lab=2)
  matlines(Yrs[1:yri],matrix(acrops[1:yri,1:3],ncol=3),type="o",col=c(custpal[2:3],"black"),lty=1,lwd=2,pch=16,cex=2)
  legend("topleft",c("Both","Soybeans","Corn"),cex=2,seg.len=0.75,col=c("black",custpal[3:2]),lwd=10,box.col="transparent",bg="transparent")
  par(mfrow=c(1,1))
}



#Case 2: Climate change
plotfun=function(regs,var,scen){
  X=ata[,1,1]; par(mfrow=1:2);
  source=(1:NR)+NR*(var=="Mean Temperature")
  tp1=ata[,8+9*(scen=="8.5 dergees warmer")+0:2, source]
  tp1[X<2015,,]=ata[X<2015,2:4, source] #plug in observed data
  tp2=tp1[,1,]-matrix(ata[1,2,source],length(X),NR,byrow=TRUE)
  
  matplot(X,tp1[,1,regs],col=regs,type="l",lwd=3,lty=1,ylim=range(tp1[,,regs],na.rm=TRUE)+c(-3,3),xlab="Year",main=paste0(var,"\n (degrees F)"),ylab="")
  for(R in regs) polygon(c(X,rev(X)),c(tp1[,2,R],rev(tp1[,3,R])),col=adjustcolor(R,alpha.f=0.2),border=0)
  legend("topleft",dim3info[regs,1],col=regs,lwd=3,box.col="transparent",bg="transparent")
  matplot(X,tp2[,regs],col=regs,type="l",lwd=3,lty=1,ylim=range(tp2,na.rm=TRUE),xlab="Year",main=paste0(var,"\n (degrees F above 1995 temperature)"),ylab="")
  par(mfrow=c(1,1))
}



#Case 3: Do bigger mammals sleep more?
sleepplot=function(var,addmodel){
  y=sleepy$sleep_total; xcands=2.2*sleepy[,c("bodywt","brainwt")]; xcands=cbind(xcands,log(xcands));
  x=xcands[,which(c("Body weight","Brain weight","log Body weight","log Brain weight")==var)]
  model=lm(y~x); R2=round(100*(cor(y[!is.na(x)],model$fitted.values)^2));
  plotlab=paste0("Predicting how long a species sleeps duration from its ",var)
  if(addmodel) plotlab=paste0(plotlab,". \n Model prediction (line) explains ",R2,"% of data")
  plot(x,y,las=1,cex.lab=1.5,pch=16,col=8,xlab=paste0(var," of species (pounds)"),ylab="How long species sleep (hours)",main=plotlab)
  if(addmodel) lines(na.omit(x),model$fitted.values,lwd=3)
}



#Case 4: Kelp forest dynamics and ocean climate change
sa=function(dati) aggregate(dati,by=list(dati$year),FUN=function(x) mean(x,na.rm=TRUE))
chisum=function(gami,chis=summary(gami)$chi.sq,R2=gami$r.sq) R2*chis/sum(chis)
gamfactplot=function(x,CL,y=-x$fit,xv=1:length(y)){ lines(xv,y,col=CL,lwd=2); polygon(c(xv,rev(xv)),c(y+x$se,rev(y-x$se)),col=adjustcolor(CL,alpha.f=0.2),border=0); }

kelpplot=function(vars,kmx=6){
  layout(matrix(c(rep(0,3),2,2, rep(1,3),2,2, rep(1,3),3,3, rep(1,3),3,3), 4,5,TRUE))
  NV=1+length(vars); mathVars=1[-1]; if(NV>1) mathVars=paste("+s(",VarNms[vars],",k=",kmx,")",sep="");
  math=as.formula(paste0(c("barren~s(MyRandomNums)",mathVars),collapse=" "))
  model=gam(math,data=gamdat); gamdat$pred=model$fitted.values; rsa=sa(gamdat);
  
  matplot(rsa[,1],100*rsa[,c("barren","pred")],main="Percent of reefs barren",col=c(1,4),frame.plot=F,las=1,cex.axis=1.5,cex.lab=1.5,cex.main=2,xlab="Year",ylab="",type="l",lwd=2,lty=1)
  legend("topleft",c("Observed in data","Predicted by our model"),cex=2,text.col=c(1,4),box.col="transparent",bg="transparent")
  
  R2reg=100*cor(rsa[,c("barren","pred")])[1,2]^2; predan=chisum(model,R2=R2reg); VarRank=order(predan);
  barplot(pmax(predan[VarRank],3),xlab="Percent of change explained",cex.names=1.5,col=(1:8)[1+c(0,vars)][VarRank],horiz=TRUE,las=1,main="Explanatory power \n of each variable",cex.main=1.75,cex.lab=1.5)
  
  effs=plot(model,select=0); effsYlm=range(unlist(lapply(effs,function(i) range(-i$fit))));
  plot(1:1e2,col=0,ylim=effsYlm,ylab="",las=1,frame.plot=F,cex.lab=1.5,cex.main=1.5,xlab="Variable level, % of maximum",main="Estimated effect on kelp")
  for(fi in 1:length(effs)) gamfactplot(effs[[fi]],(1:8)[1+c(0,vars)][fi])
  par(mfrow=c(1,1))
}







### APPENDIX 2: CODE TO IMPORT DATA
wdset=function() setwd("C:\\Users\\vkara\\Dropbox\\Conferences\\Data Science Haskell")

#Case 1: Land use change
wdset(); setwd("marion_cdl\\tif");
#Lump land use into categories of interest
corn=1; soy=c(5,26); lake=c(111,190,195); urban=121:124; nature.pasture=c(58,63,81,82,83,87,88,112,141,142,143,152,176);
subst=function(x,xn=x*NA){ xn[is.na(x)]=0; xn[x==corn]=1; xn[x%in%soy]=2; xn[x%in%urban]=3; xn[x%in%lake]=4; xn[x%in%nature.pasture]=5; xn[is.na(xn)]=6; xn[,colSums(xn)>0]; }
#Assemble dataset of land use in each year
crp=list(); for(i in Sys.glob("*.tif")) crp=c(crp,list(subst(as.matrix(raster(i)))));
#calculate time trends
acrops=do.call("rbind",lapply(crp,function(x,xv=as.vector(x)) c(sum(xv==1),sum(xv==2),sum(xv%in%(1:2)),sum(xv%in%c(1,2,6)),sum(xv>0))))
acrops[,1:3]=100*acrops[,1:3]/acrops[,c(4,4,4)]



#Case 2: Climate change
library(rio); library(abind);
wdset(); setwd("climate data")
Toread=Sys.glob("*.csv") #ger all RDS files starting with name
NR=length(Toread)/2; ata=as.matrix(import(Toread[1]));
for(i in Toread[-1]) ata=abind(ata,as.matrix(import(i)),along=3)
dim3info=cbind(rep(c("Alaska","Canada","Egypt","Greenland","Kansas","Patagonia","USA"),2),rep(c("Max","Mean"),each=NR))
dimnames(ata)[[3]]=apply(dim3info,1,paste0,collapse="_")
ata[,-1,]=ata[,-1,]*9/5+32 #convert to Fahrenheit



#Case 3: Do bigger mammals sleep more?
library(ggplot2); sleepy=as.data.frame(msleep)



#Case 4: Kelp forest dynamics and ocean climate change
wdset(); gamdat=readRDS("gamdatp.rds"); VarNms=names(gamdat)[4:10];




