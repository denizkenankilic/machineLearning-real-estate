# Install Necessary Packages
library(readxl)
install.packages('gtools')
library(gtools)
install.packages('xlsReadWrite')
library(xlsReadWrite)

#Reading Data From Excel File
AmesHousing<-read_excel("AmesHousing.xls")
names(AmesHousing)
plot(AmesHousing[,82], type="l")
summary(AmesHousing)

#AmesHousing$'Garage Finish'<-matrix(AmesHousing$'Garage Finish',nrow = 2930,ncol = 1)

# Filling Null Spaces and Replacing Chars with Numerics
i=0
 for(i in 1:2931){
   if(invalid(AmesHousing$'Garage Finish'[i])) AmesHousing$'Garage Finish'[i] = 'NA'
   if(AmesHousing$'Garage Finish'[i] == 'Fin'){
     AmesHousing$'Garage Finish'[i]=0
     }else if(AmesHousing$'Garage Finish'[i] == 'Unf'){
         AmesHousing$'Garage Finish'[i]=1
       }else if(AmesHousing$'Garage Finish'[i] == 'NA'){
           AmesHousing$'Garage Finish'[i]=2
         }
       else {
         AmesHousing$'Garage Finish'[i]=3
       }
     }

# Looking Data     
AmesHousing$'Garage Finish' 

# Writing Data in txt and xls Formats
write.table(AmesHousing$'Garage Finish', "C:/Users/user50/Desktop/R Codes/Exporting Files/Garage Finish.txt", sep="\t")
#For Excel File
# library(xlsx)
# write.xlsx(mydata, "c:/mydata.xlsx")

# Plotting Data and Save to File as PDF or PNG
pdf("C:/Users/user50/Desktop/R Codes/Exporting Files/Garage Finish.pdf")
plot(AmesHousing$'Garage Finish')
dev.off()
