# looking at scans 

#############################################################
###WAY 1: Read in your OPUS files from folder in windows explorer###
### This way requires the OPUS macro to output spc files
#############################################################
### ALL SPECTRA
#############################################################
# set path for .spc data for spectral data

library(hyperSpec)
setwd("C:/Users/jamie.clark/Desktop/MARFIN_Project_data_desktop")
files.t<-list.files(path=("SPECTRA/ALL_SPECTRA.spc")
) #set the file path to the folder with your files.

files.t
# read in all the chosen directory files (ALWAYS)
library(here)
spectrumlist.t <-lapply(paste0(here("SPECTRA","ALL_SPECTRA.spc"),"/",files.t),read.spc) #this contains the OPUS file information stored as data blocks.
#these data blocks will need to be indexed (see below code) for analysis.

# loop through the file names and pull them out (ALWAYS)
filenames.t <- matrix(data = NA, nrow = length(spectrumlist.t), ncol = 1) #set empty matrix to be filled by loop

for (j in 1:length(spectrumlist.t)){
  filenames.t[j,] <- spectrumlist.t[[j]]@data$filename
}

#make a dataframe out of the portions of the files that I am interested in for analysis
specs.t <- lapply(spectrumlist.t, function (x) as.data.frame(cbind(x@wavelength, x@data$spc[1:length(x@data$spc)]))) 

#add file names to the list of files
names(specs.t)<-lapply(spectrumlist.t, function (x) x@data$filename) 


#turn list into a dataframe and add a column with the filename
specsdf.t<-dplyr::bind_rows(specs.t,.id="id")

colnames(specsdf.t)<-c("sample_id_spc","wavenumbers","absorbance_units")

# want to overwrite the large filename with a shortened version, the nubmer 22 will change depending on length of sample id name
#specsdf.t$filename <- str_left(specsdf.t$filename, 81)

specsdf.t$sample_id_spc <- gsub('C:/Users/jamie.clark/Desktop/MARFIN_Project_data_desktop/SPECTRA/ALL_SPECTRA.spc/','',specsdf.t$sample_id_spc)


# plot raw spectra
library(ggplot2)
ggplot(specsdf.t)+
  geom_line(aes(x = wavenumbers, y = absorbance_units, group = filename), color = "black")+
  scale_x_reverse()+
  theme_classic()

# transforming data from long to wide 
library(tidyr)
library(dplyr)
#rm(specdf.wide.t)
#rm(spec.df.wide.2.t)
#rm(spec.df.wide.t)

specdf.wide.t <- spread(specsdf.t, wavenumbers, absorbance_units)
specdf.wide.t$filename <- gsub('WHITE_GRUNT_','',specdf.wide.t$filename) # removing the .spc, 18 will change depending on length of sample id
specdf.wide.t$filename <- gsub('.spc','',specdf.wide.t$filename)

#rm(spec.df.wide.2.t)
#spec.df.wide.2.t <- specdf.wide.t %>% select(filename, everything()) #getting sampling id to first column 

spec.df.wide.t <- specdf.wide.t %>% select(sample_id_spc, everything())
#spec.df.wide.t <- spec.df.wide.2.t[,-c(1:2)] # removing the sample id and filename columns
#rownames(spec.df.wide.t) <- spec.df.wide.2.t[,1]
saveRDS(spec.df.wide.t, "SPECTRA/Meta_Data/spec.df.wide.t.RDS")

## read in metadata #
# read in meta data file, make sure sample id column matches the scan names
# NEED TO MAKE SURE THAT META DATA IS SORTED BY SAMPLE ID SO THAT IT MATCHES THE SPECTRA FILE 
meta.data.t <- read.csv(file="ALLcurrentspectradata.csv", header=T)
meta.data.t$Fractional_Age_1 <- as.numeric(as.character(meta.data.t$Fractional_Age_1))
saveRDS(meta.data.t, "SPECTRA/Meta_Data/meta.data.t.RDS")

meta.data.t[-which(meta.data.t$sample_id_spc%in%substr(files.t,1,nchar(files.t)-4)),] # checking to see if any spectra files are missing by comparing to meta data

#colnames(df)[1]  <- "student_name"

# merge the meta data with the dataframe of scan data
specsdf_age_wide.t <- merge(spec.df.wide.t, meta.data.t, by="sample_id_spc", all.x=TRUE)

# to plot, need the data in long format, specsdf.t is wide format
#specsdf.t$sample_id_spc <- substr(specsdf.t$filename, 1, 18)
specsdf_age.t <- merge(specsdf.t, meta.data.t, by="sample_id_spc", all.x=TRUE)
saveRDS(specsdf_age.t, "SPECTRA/Meta_Data/specsdf_age.t.RDS")

library(ggplot2)
# WITH BOTH REGIONS, side by side
ggplot(data=specsdf_age.t, aes(x=wavenumbers, y=absorbance_units, group=sample_id_spc, color=Fractional_Age_1))+
  geom_line() +
  scale_color_gradient(high = "blue", low = "red", trans="reverse")+
  scale_x_reverse(limits=c(7192,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_x_reverse(limits=c(11535,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_y_continuous(limits=c(0.19,0.255))+
  #scale_y_continuous(limits=c(0.172,0.255))+
  labs(x="Wavelength", y="Absorbance", color="Frac. age")+
  theme_bw()+
  theme(axis.title.y = element_text(size=12,margin=margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5))+
  facet_wrap(~Region)
ggsave("Output/PLots/Carols_rawspectrabyage", width=7.5, height=5) 


# Carolinas by age
ggplot(data=specsdf_age.t, aes(x=wavenumbers, y=absorbance_units, group=sample_id_spc, color=Fractional_Age_1, filter(Region = "Carolinas")))+
  geom_line() +
  scale_color_gradient(high = "blue", low = "red", trans="reverse")+
  scale_x_reverse(limits=c(7192,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_x_reverse(limits=c(11535,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_y_continuous(limits=c(0.19,0.255))+
  #scale_y_continuous(limits=c(0.172,0.255))+
  labs(x="Wavelength", y="Absorbance", color="Frac. age")+
  theme_bw()+
  theme(axis.title.y = element_text(size=12,margin=margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5))

# Florida by age
ggplot(data=specsdf_age.t, aes(x=wavenumbers, y=absorbance_units, group=sample_id_spc, color=Fractional_Age_1, filter(Region = "Florida")))+
  geom_line() +
  scale_color_gradient(high = "blue", low = "red", trans="reverse")+
  scale_x_reverse(limits=c(7192,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_x_reverse(limits=c(11535,4192),breaks=seq(4192,11535,750))+ #can take out limits for whole spec
  #scale_y_continuous(limits=c(0.19,0.255))+
  #scale_y_continuous(limits=c(0.172,0.255))+
  labs(x="Wavelength", y="Absorbance", color="Frac. Age")+
  theme_bw()+
  theme(axis.title.y = element_text(size=12,margin=margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5))

ggsave("Output/PLots/FL_rawspectrabyage", width=7.5, height=5) 



##############
###Packages###
##############

library(viridis)
library(vegan)
library(pls)
library(mdatools)
library(ade4)
library(plyr)
library(bio3d)
library(dplyr)
library(tidyr)
library(grDevices)

## PLSR #########
xmatrix <- as.matrix(spec.df.wide.t)

# code for using macro to get .spc and import way 1 
Age <- meta.data.t$Fractional_Age_1
OtoWt <- meta.data.t$Final_Otolith_WT

###################################################################################################################
### No preprocessing fit and visualize ###
###################################################################################################################
###Plot PCA###
pca <-prcomp(xmatrix, scale = TRUE)
summary(pca)
scores <-data.frame(pca$x)

par(mar = c(1,1,2,2))
length(scores) <- length(Age)

PCA <- data.frame(Age,scores) # can add more things like species

s.class(PCA[,4:5],
        fac = fAge,  # color by groups
        #col = c("black", "dark red", "red","dark orange", "green", "dark green", "blue", "dark blue", "purple", "brown"),
        col = c("black", "red","dark orange", "green", "blue", "purple", "brown","turquoise","grey38"),
        grid = FALSE,
        label = levels(fAge),
        cellipse = 2, #2 standard deviations (95% conf interval)
        clabel = 1.5,
        cpoint = 1,
        sub="No preprocessing",
        possub = "topleft"
)



###scree plot to visualize proportions of variance explained across PCs###
par(mar =c(5,5,1,1))

# calculate proportional variance for each PC
PoV <- pca$sdev^2/sum(pca$sdev^2)

#Scree Plot of proportional variances
plot(PoV[1:20], ylab = "Variance Explained",  xlab = "Principle Components")
lines(PoV[1:20])

### Try running the iPLS algorithm with mdatools ### 
# need to remove spectra above wavenumber 8000, 8000 corresponds to column 507+ and 7000 is column 382
#xmatrix.sub <- xmatrix[, 1:507]
xmatrix.sub <- xmatrix[, 1:382]

iPLS.whitegrunt <- ipls(xmatrix.sub, Age, glob.ncomp = 10, center = T, scale = T, cv = 100,
                     int.ncomp = 10, int.num = 10, ncomp.selcrit = "wold", 
                     method = "forward", silent = F)
summary(iPLS.sardine)
plot(iPLS.sardine, main = NULL)

vars <- iPLS.sardine$var.selected # selecting column numbers

