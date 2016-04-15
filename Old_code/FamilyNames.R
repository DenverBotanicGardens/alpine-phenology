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

#compare to previous dataset

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


# With the ID, no longer have species names as row names...   
# Select the first instance of each synonym - not the accepted, just one of them
first <- synonyms[!duplicated(synonyms$ID),]
nrow(first)
head(spyears)
spyears$ID <- row.names(spyears)
spyears <- merge(first[,-3],spyears, by="ID")
colnames(spyears)[2] <- "Species"

sp.10plus <- spyears[spyears$DataYrs>=10,]
nrow(sp.10plus) #547 -> 479 -> 498, after changing all spelling to coerulea from caerulea and Eritrichum to Eritrichium, fixing arabis/boechera, synonymy


#Line 352

#Pull only the species that have 10 or more years of data for analyses

tenyearplus <- merge(bloom2,sp.10plus,by = "ID")
length(unique(tenyearplus$scientificName)) #545 -> 544 -> 449 -> 741
length(unique(tenyearplus$ID)) # 498


#Pull out earliest collection


#x<- unique(tenyearplus$ID)[100]

minJul.dates.2 <- lapply(unique(tenyearplus$ID), function(x){
  this.species <- tenyearplus[tenyearplus$ID == x,]
  these.rows <- aggregate(startDayOfYear~Year, this.species, FUN=min)
  merge(unique(this.species[,c("ID","Year","startDayOfYear","DataYrs")]), these.rows, by = c("startDayOfYear","Year"))
})

min1950_2 <- do.call(rbind,minJul.dates.2)
head(min1950_2)

# check that none have more than one a year
singlefoo <- data.frame(table(min1950_2$Year,min1950_2$ID))
nrow(singlefoo[singlefoo$Freq> 1,]) #154? -> 0

# add back the first species name (not the synonyms) for each data ID
onename <- tenyearplus[!duplicated(tenyearplus$ID),]
min1950_2 <- merge(min1950_2, onename, by=c("ID","startDayOfYear","Year","DataYrs"))

#Check that there is only one species collection date per year
singlefoo <- data.frame(table(min1950_2$Year,min1950_2$ID))
nrow(singlefoo[singlefoo$Freq> 1,]) #11? -> 1?? -> 0

length(unique(min1950_2$ID)) #246 species
nrow(min1950_2) #246!! Should have more, right?


