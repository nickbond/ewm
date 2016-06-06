
# 2014 Nate Miller

#This basic R function calculated oxygen saturation values for natural waters
#in units of umole O2/L, mg O2/L, torr, mbar, and kPa from user supplied values 
#of salinity, temperature (degrees C), atmospheric pressure (mbar), and desired
#saturation state.


O2.saturation<-function(salinity, temp, measured.atmP, perc.sat, corrected.DO.only=TRUE) {
  
  a=4.9e1
  b=-1.335
  c=2.759e-2
  d=-3.235e-4
  e=1.598e-6
  p=5.516e-1
  q=-1.759e-2
  r=2.253e-4
  s=-2.654e-7
  t=5.362e-8
  A=5.257e1
  B=6.69e3
  C=4.681
  TK=temp+273
  Chloride=(salinity-0.03)/1.805
  atmPsealevel=1013
  MolVol=22.414
  MWO2=32
  
  alpha=a+(b*temp)+(c*temp^2)+(d*temp^3)+(e*temp^4)-(Chloride*(p+(q*temp)+(r*temp^2)+(s*temp^3)+(t*temp^4)))
  bunsen=alpha/1000
  vapP=exp(A-(B/TK)-(C*log(TK)))
  
  umoleO2.per.L<-(((measured.atmP-vapP)/atmPsealevel)*(perc.sat/100)*0.2095*bunsen*1e6*(1/MolVol))
  mgO2.per.L<-umoleO2.per.L*(MWO2/1000)
  pO2.torr<-((measured.atmP-vapP)*((perc.sat/100)*0.2095))*0.75
  pO2.mbar<-pO2.torr/0.75
  pO2.kPa<-pO2.mbar/10

  if(corrected.DO.only == FALSE) {
    output<-data.frame(salinity, temp, measured.atmP, perc.sat, umoleO2.per.L, mgO2.per.L, pO2.torr, pO2.mbar, pO2.kPa)
  }  
  else {
    output <- mgO2.per.L
  }
  return(output)
}
