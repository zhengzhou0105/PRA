# In original dataset, units of dose include mg/kg/day, NA, mg/m3, mg/L, mg/kg, ppm, 

#---------------------------------------------------------------
# for any feed or drinking water, unit must be in ppm, mg/kg or mg/L. 
# male or female, rat or mice, duration t in week
efsa<-function(sex,spe,route,t){
  a<-0
  b<-0
  if (spe=="r") {
    if (sex=="m"){
      if (route=="f"){
        a=.175
        b=.294
      } else {
        a=.180
        b=.268
      }
    } else {
      if (route=="f"){
        a=.158
        b=.216
      } else {
        a=.167
        b=.232
      }
    }
  } else {
    if (sex=="m"){
      if (route=="f"){
        a=.223
        b=.115
      } else {
        a=.218
        b=.161
      }
    } else {
      if (route=="f"){
        a=.262
        b=.094
      } else {
        a=.293
        b=.264
      }
    }
  }
  efsa<-a*(t^(-b))
}

efsat=efsa("m","m","f",100)

dose=function(c,efsat){
  dose=c*efsat
}

doset=dose(64,efsat)
#-------------------------------------
# heq conversion
# heq to dose conversion should be done by reverse calculation with conversion factor from IRIS database
cov=function(dose,bw){
  cov=dose/((BW/70)^0.25)
}

#----------------------------
# inhalation 
# unit mg/m3 t in minutes bw in kg
inh<-function(c,bw,t,spe){
  if (spe=="rat") {
    a<-0.56
    b<-0.8206
  } else if (spe=="mice"){
    a<-1.38
    b<-1.0496
  } else {
    a<-.46
    b<-0.7579
  }
  inh<-c*a*(bw^b)/1000*t/bw
}


inhalation=inh(157.25,.02,6*60,"mice")
