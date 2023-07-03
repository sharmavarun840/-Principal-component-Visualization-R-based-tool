# Make .par file
genotypename: input.bed
snpname: input2.bim
indivname: input2.fam
evecoutname: input.evec
evaloutname: input.eval
numoutevec: 20
numoutlieriter: 0
snpweightoutname: input.snpweight

#input here is yourfile name can be renamed as your own file name
#make sure the.fam file donot have larger names otherwise thir will be error in running the PCA

#use command to run PCA
smartpca -p input.par

#for visulaistaion###
##########PREPARE pop info file for smartpca plot#######################
pca_popinfo=read.csv("Popinfo.csv", header=TRUE)
names(pca_popinfo)

#the names of the columns are visible here of the popinfo file

write.csv(pca_popinfo, "pop_4smartpca.csv", row.names = FALSE)

# a file named 4smartpca.csv will be made in your working directory folder

famfile=read.table("input2.fam", header=FALSE)
famfile$indorder=c(1:dim(famfile)[1])

#inorder column is added into the famfile

head(famfile);tail(famfile);dim(famfile)
# to visualise the fam file above command is used

popinfo=read.csv("pop_4smartpca.csv", header=TRUE)
head(popinfo);dim(popinfo)
#visualised .csv file

popinfo2=merge(popinfo,famfile, by.x=c("FID"),by.y=c("V2"))
#a new file popinfo2 is made where popinfo and fam file is merged.
#V2 column in the fam file is removed as it was also having sample ID so just one sampleID in the popinfo file is kept.

head(popinfo2);dim(popinfo2)
#visualise file 


popinfo=popinfo2[order(popinfo2$indorder),]

head(popinfo);dim(popinfo)
# have one extra column named inorder

val=read.table("input.eval", header=F)
head(val)
#just viualise

###replace only first colon in the first column (FID:IID) with a space in the .evec file in shell####
#This correction in evec file is needed as many of the IIDs have several colons###

###sed 's/:/ /' input.evec > input1.evec

vec=read.table("input1.evec", header=F)
head(vec)

#the first column is seprated

vec$V23 <- NULL
head(vec);tail(vec);dim(vec)

#V23 is removed

varexp=val$V1/sum(val$V1)
varexp*100

vec$POPADMIX_IID=paste(vec$V1,vec$V2,sep="_")
head (vec)

#a new column of POPADMIX_ID is added in which 1 and 2 column are merged#

pca_data = merge(popinfo, vec, by.x=c("FID"),by.y=c("V2"))
head(pca_data);dim(pca_data)

#merged file have popinfo data and eig file 

table(factor(pca_data$CONTINENT))
#continets with numbers of individuals will apear

names(pca_data)
#headers of pcadata file (no of columns with headrers are seen)

write.csv(pca_data,"PCApop_Asia.csv", row.names = FALSE)
 #a new csv is written in the working directory named (PCAdata_Euroasia.csv)

table(pca_data$CONTINENT)
# continent name with no of indv.is seen here

table(pca_data$SCONT, exclude=FALSE)
#abbreviated continent names with no of individuals is visible

continents=c("lightsalmon3","turquoise","lightskyblue4","mediumpurple1","darkgreen",
             "wheat","hotpink","tan1","palegreen","darkslategray3","gold",
             "burlywood2","steelblue1","navy")

# the number of colors should be same as that of table pca content  
library(ggplot2)
ggplot(pca_data, aes(-V3.y, -V4.y)) + 
  geom_point(size=2, shape=21, colour = "gray27", aes(fill = CONTINENT)) + 
  scale_color_manual(values=continents, name="Geographical regions") +
  scale_fill_manual(values=continents, name="Geographical regions") +
  xlab(paste("PC1"," [",round(varexp[1]*100,1),"%]",sep="")) + 
  ylab(paste("PC2"," [",round(varexp[2]*100,1),"%]",sep="")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16), 
        legend.text  = element_text(size = 14),
        legend.key.size = unit(0.75, "cm")) +
  guides(colour=guide_legend(override.aes=list(size=4, color = "grey")))


## if PC are needed to be changed they are changed in line 92 and 93
####also change in the 89 line this takes the PC components calculated####
##xlab(paste("PC1"," [",round(varexp[1]*100,1),"%]",sep="")) + 
##ylab(paste("PC2"," [",round(varexp[2]*100,1),"%]",sep="")) +

