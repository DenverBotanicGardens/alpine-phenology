#Family names
#Family names for each genus    


genera <- gsub(" .*$", "", bloom2[,2])
head(genera)

write.table(unique(genera), file =  "Q:/Research/Projects/alpine-phenology/speciesbloom2.csv", sep=",")

#In SEINet loaded genera, updated with ASU Taxonomic Thesaurus
# 121 Families, 457 Genera
asuFams <- read.csv("Q:/Research/Projects/alpine-phenology/colo_seinet_alpine_2016_genus.csv")

asuFams$genus <- gsub(" .*$", "", asuFams[,2])

tochar <- c(1:3)
asuFams[,tochar] <- sapply(asuFams[,tochar], function(ch){
  as.character(ch)
}) 

#missing Polemonium
asuFams <- rbind(asuFams,data.frame(Family = "Polemoniaceae",
                                    ScientificName = "Polemonium sp.",
                                    ScientificNameAuthorship = "L.",
                                    TaxonId = max(asuFams$TaxonId)+1,
                                    genus = "Polemonium"))

compare to previous dataset
```{r}
bloom<-read.csv(text = getURL("https://raw.githubusercontent.com/DenverBotanicGardens/alpine-phenology/master/QR_final_R_plant_climate_data_2.csv")) 

minJul.dates <- lapply(unique(bloom$sppListID), function(x){
  this.species <- bloom[bloom$sppListID == x,c(4:5,11,19:26)]
  these.rows <- aggregate(startDayOfYear~Year, this.species, FUN=min)
  unique(merge(this.species, these.rows, by = c("startDayOfYear","Year")))
})

mindate <- function(bl50){
  lapply(unique(bl50$sppListID), function(x){
    #almost, need to use grep with "Avg" or type them all out...
    this.species <- bl50[bl50$sppListID == x,names(bl50) %in% c("Scientific.Name",
                                                                "sppListID",
                                                                "startDayOfYear",
                                                                "Year","Raw_Precip",
                                                                "Avg_Hi","Avg_Lo")]   #c(4:5,11,19:28)]
    these.rows <- aggregate(startDayOfYear~Year, this.species, FUN=min)
    unique(merge(this.species, these.rows, by = c("startDayOfYear","Year")))
  })
  names(minJul.dates) <- unique(bl50$sppListID)
  min.dates <- do.call(rbind,minJul.dates)
}

bl50 <- subset(bloom, bloom$year_plant > 1949) 
min1950 <- mindate(bl50)
min1950 <- as.data.frame(sapply(min1950, gsub, 
                                pattern = "coerulea", 
                                replacement = "caerulea"))

#Fix double space in Hierochola hirta
min1950 <- as.data.frame(sapply(min1950, gsub,
                                pattern="Hierochlo  hirta",
                                replacement = "Hierochlo hirta"))

tonumeric <- c(1:2,4:length(min1950))
min1950[,tonumeric] <- sapply(min1950[,tonumeric], function(s){
  as.numeric(as.character(s))
})

i <- sapply(min1950,is.factor)
min1950[i] <- lapply(min1950[i], as.character)

min1950$SciName <- sub( "(^[^ ]+[ ][^ ]+)(.+$)", "\\1", min1950$Scientific.Name)

unique(bloom2$scientificName[complete.cases(match(unique(bloom2$scientificName),unique(min1950$SciName)))])
```
