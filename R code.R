
library(dplyr)
library(ggplot2)
dataNew = read.csv("/Users/jim/Downloads/IS4250 Mock Data.csv")

BSI = dataNew[,2]
gender = dataNew[,3]
age = dataNew[,4] 

HA = dataNew[dataNew$BSI=="HA",]
CA = dataNew[dataNew$BSI=="CA",]

HA_F = HA[HA$Gender=="F",]
HA_M = HA[HA$Gender=="M",]
CA_F = CA[CA$Gender=="F",]
CA_M = CA[CA$Gender=="M",]

HA_F_18 = HA_F[HA_F$Age>=0&HA_F$Age<=18,]
HA_F_39 = HA_F[HA_F$Age>=19&HA_F$Age<=39,]
HA_F_59 = HA_F[HA_F$Age>=40&HA_F$Age<=59,]
HA_F_79 = HA_F[HA_F$Age>=60&HA_F$Age<=79,]
HA_F_80 = HA_F[HA_F$Age>=80,]

HA_M_18 = HA_M[HA_M$Age>=0&HA_M$Age<=18,]
HA_M_39 = HA_M[HA_M$Age>=19&HA_M$Age<=39,]
HA_M_59 = HA_M[HA_M$Age>=40&HA_M$Age<=59,]
HA_M_79 = HA_M[HA_M$Age>=60&HA_M$Age<=79,]
HA_M_80 = HA_M[HA_M$Age>=80,]

CA_F_18 = CA_F[CA_F$Age>=0&CA_F$Age<=18,]
CA_F_39 = CA_F[CA_F$Age>=19&CA_F$Age<=39,]
CA_F_59 = CA_F[CA_F$Age>=40&CA_F$Age<=59,]
CA_F_79 = CA_F[CA_F$Age>=60&CA_F$Age<=79,]
CA_F_80 = CA_F[CA_F$Age>=80,]

CA_M_18 = CA_M[CA_M$Age>=0&CA_M$Age<=18,]
CA_M_39 = CA_M[CA_M$Age>=19&CA_M$Age<=39,]
CA_M_59 = CA_M[CA_M$Age>=40&CA_M$Age<=59,]
CA_M_79 = CA_M[CA_M$Age>=60&CA_M$Age<=79,]
CA_M_80 = CA_M[CA_M$Age>=80,]

HA_18_F_Rate = nrow(HA_F_18)/65000*100000
HA_39_F_Rate = nrow(HA_F_39)/65000*100000
HA_59_F_Rate = nrow(HA_F_59)/65000*100000
HA_79_F_Rate = nrow(HA_F_79)/65000*100000
HA_80_F_Rate = nrow(HA_F_80)/65000*100000

HA_18_M_Rate = nrow(HA_M_18)/50000*100000
HA_39_M_Rate = nrow(HA_M_39)/50000*100000
HA_59_M_Rate = nrow(HA_M_59)/50000*100000
HA_79_M_Rate = nrow(HA_M_79)/50000*100000
HA_80_M_Rate = nrow(HA_M_80)/50000*100000

CA_18_F_Rate = nrow(CA_F_18)/65000*100000
CA_39_F_Rate = nrow(CA_F_39)/65000*100000
CA_59_F_Rate = nrow(CA_F_59)/65000*100000
CA_79_F_Rate = nrow(CA_F_79)/65000*100000
CA_80_F_Rate = nrow(CA_F_80)/65000*100000

CA_18_M_Rate = nrow(CA_M_18)/50000*100000
CA_39_M_Rate = nrow(CA_M_39)/50000*100000
CA_59_M_Rate = nrow(CA_M_59)/50000*100000
CA_79_M_Rate = nrow(CA_M_79)/50000*100000
CA_80_M_Rate = nrow(CA_M_80)/50000*100000

#Female
Values = matrix(c(CA_18_F_Rate,CA_39_F_Rate,CA_59_F_Rate,CA_79_F_Rate,CA_80_F_Rate,HA_18_F_Rate,HA_39_F_Rate,HA_59_F_Rate,HA_79_F_Rate,HA_80_F_Rate),nrow=2,ncol=5,byrow=TRUE)
colors = c("white","black")
ages = c("0-18","19-39","40-59","60-79","80+")
regions = c("Community acquired","Healthcare associated")

bar = barplot(Values,main="Incident rate",names.arg=ages,xlab="Age(years)",ylim=c(0,500),ylab="Incident rate per 100,000 person-years",col=colors)

# Add the legend to the chart.
legend("topleft", regions, cex=1, fill=colors)

#Male
Values2 = matrix(c(CA_18_M_Rate,CA_39_M_Rate,CA_59_M_Rate,CA_79_M_Rate,CA_80_M_Rate,HA_18_M_Rate,HA_39_M_Rate,HA_59_M_Rate,HA_79_M_Rate,HA_80_M_Rate),nrow=2,ncol=5,byrow=TRUE)

bar2 = barplot(Values2,main="Incident rate",names.arg=ages,xlab="Age(years)",ylim=c(0,500),ylab="Incident rate per 100,000 person-years",col=colors)

# Add the legend to the chart.
legend("topleft", regions, cex=1, fill=colors)




library(ggplot2)
BSI_data=read.csv("/Users/WangTianQi/Downloads/IS4250 Mock Data.csv")

attach(BSI_data)
y9899=0
y0001=0
y0203=0
y0405=0
y0607=0
for(i in 1:733)
{if(BSI_data[i,"Year"] == 1998 | BSI_data[i,"Year"] == 1999) y9899=y9899+1
else if(BSI_data[i,"Year"] == 2000 | BSI_data[i,"Year"] == 2001) y0001=y0001+1
else if(BSI_data[i,"Year"] == 2002 | BSI_data[i,"Year"] == 2003) y0203=y0203+1
else if(BSI_data[i,"Year"] == 2004 | BSI_data[i,"Year"] == 2005) y0405=y0405+1
else y0607=y0607+1
}
y9899HA=0 
y0001HA=0
y0203HA=0
y0405HA=0
y0607HA=0
for(i in 1:733)
{if((BSI_data[i,"Year"] == 1998 | BSI_data[i,"Year"] == 1999) & BSI_data[i,"BSI"] == "HA") y9899HA=y9899HA+1
else if((BSI_data[i,"Year"] == 2000 | BSI_data[i,"Year"] == 2001) & BSI_data[i,"BSI"] == "HA") y0001HA=y0001HA+1
else if((BSI_data[i,"Year"] == 2002 | BSI_data[i,"Year"] == 2003) & BSI_data[i,"BSI"] == "HA") y0203HA=y0203HA+1
else if((BSI_data[i,"Year"] == 2004 | BSI_data[i,"Year"] == 2005) & BSI_data[i,"BSI"] == "HA") y0405HA=y0405HA+1
else if((BSI_data[i,"Year"] == 2006 | BSI_data[i,"Year"] == 2007) & BSI_data[i,"BSI"] == "HA") y0607HA=y0607HA+1
}

ratio=1/(2*115000)*100000
year_all=numeric(10)
dim(year_all)=c(2,5)
year_all[1,]=c(y9899-y9899HA, y0001-y0001HA, y0203-y0203HA, y0405-y0405HA, y0607-y0607HA)*ratio
year_all[2,]=c(y9899HA, y0001HA, y0203HA, y0405HA, y0607HA)*ratio
rowNames=c("CA", "HA")
row.names(year_all)=rowNames
colNames=c("1998-1999", "2000-2001", "2002-2003", "2004-2005", "2006-2007")
colnames(year_all)=colNames
bar=barplot(year_all, main="Incident Rate by Calendar Year and Site of Acquisition", names.arg = colNames, xlab="Calendar Year", ylab="Incident Rate per 100,000 person-years", ylim=c(0,100), space=1, col=c("white","black"))
legend("topright",c("Community-acquired","Healthcare-associated"), cex=1, fill=c("white","black"))

resist9899HA=0
resist9899CA=0
resist0001HA=0
resist0001CA=0
resist0203HA=0
resist0203CA=0
resist0405HA=0
resist0405CA=0
resist0607HA=0
resist0607CA=0
for(i in 1:733)
{
  if((BSI_data[i,"Year"] == 1998 | BSI_data[i,"Year"] == 1999) & BSI_data[i,"BSI"] == "HA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist9899HA=resist9899HA+1
  if((BSI_data[i,"Year"] == 1998 | BSI_data[i,"Year"] == 1999) & BSI_data[i,"BSI"] == "CA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist9899CA=resist9899CA+1
  if((BSI_data[i,"Year"] == 2000 | BSI_data[i,"Year"] == 2001) & BSI_data[i,"BSI"] == "HA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0001HA=resist0001HA+1
  if((BSI_data[i,"Year"] == 2000 | BSI_data[i,"Year"] == 2001) & BSI_data[i,"BSI"] == "CA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0001CA=resist0001CA+1
  if((BSI_data[i,"Year"] == 2002 | BSI_data[i,"Year"] == 2003) & BSI_data[i,"BSI"] == "HA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0203HA=resist0203HA+1
  if((BSI_data[i,"Year"] == 2002 | BSI_data[i,"Year"] == 2003) & BSI_data[i,"BSI"] == "CA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0203CA=resist0203CA+1
  if((BSI_data[i,"Year"] == 2004 | BSI_data[i,"Year"] == 2005) & BSI_data[i,"BSI"] == "HA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0405HA=resist0405HA+1
  if((BSI_data[i,"Year"] == 2004 | BSI_data[i,"Year"] == 2005) & BSI_data[i,"BSI"] == "CA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0405CA=resist0405CA+1
  if((BSI_data[i,"Year"] == 2006 | BSI_data[i,"Year"] == 2007) & BSI_data[i,"BSI"] == "HA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0607HA=resist0607HA+1
  if((BSI_data[i,"Year"] == 2006 | BSI_data[i,"Year"] == 2007) & BSI_data[i,"BSI"] == "CA" & BSI_data[i,"Fluoroquinolone.Resistance"] == "Y") resist0607CA=resist0607CA+1
}
resistance_all=numeric(10)
dim(resistance_all)=c(2,5)
resistance_all[1,]=c(resist9899CA/(y9899-y9899HA), resist0001CA/(y0001-y0001HA), resist0203CA/(y0203-y0203HA), resist0405CA/(y0405-y0405HA), resist0607CA/(y0607-y0607HA))*100
resistance_all[2,]=c(resist9899HA/y9899HA, resist0001HA/y0001HA, resist0203HA/y0203HA, resist0405HA/y0405HA, resist0607HA/y0607HA)*100
row.names(resistance_all)=rowNames
colnames(resistance_all)=colNames

type1=c("CA","CA","CA","CA","CA","HA","HA","HA","HA","HA")
year1=c("1998-1999", "2000-2001", "2002-2003", "2004-2005", "2006-2007","1998-1999", "2000-2001", "2002-2003", "2004-2005", "2006-2007")
value1=c(c(resist9899CA/(y9899-y9899HA), resist0001CA/(y0001-y0001HA), resist0203CA/(y0203-y0203HA), resist0405CA/(y0405-y0405HA), resist0607CA/(y0607-y0607HA))*100, c(resist9899HA/y9899HA, resist0001HA/y0001HA, resist0203HA/y0203HA, resist0405HA/y0405HA, resist0607HA/y0607HA)*100)
df_test=data.frame(type1,year1,value1)
ggplot(df_test, aes(x=year1, y=value1,colour=type1, group=type1))+geom_line(size=2)+ggtitle("In Vitro Fluoroquinolone Resistance Rates by Calendar Year and Site of Acquisition ")+xlab("Calendar Year")+ylab("Resistance Rate %")+ylim(0,20)+geom_point(aes(shape=type1),size=5)+scale_color_discrete(name="BSI", labels=c("Community-acquired","Healthcare-associated"))+scale_shape_discrete(name="BSI",labels=c("Community-acquired","Healthcare-associated")) 
