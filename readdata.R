require(httr)
require(jsonlite)
require(dplyr)

filecache <- "cache/listevt.Rdata"

# Définition des paramétres --------------------------------------------------

url <- "https://granddebat.fr/graphql/internal"
query <- "query EventRefetchRefetchQuery($cursor: String, $count: Int, $theme: ID, $project: ID, $userType: ID, $search: String, $isFuture: Boolean) {
events(first: $count, after: $cursor, theme: $theme, project: $project, search: $search, userType: $userType, isFuture: $isFuture) {
totalCount
edges {
node {
id
lat
lng
url
fullAddress
createdAt
startAt
endAt
title
fullAddress
url
themes {
id
title
url
}
__typename
}
cursor
}
pageInfo {
hasPreviousPage
hasNextPage
startCursor
endCursor
}
}
}"
query_variables <- '{"cursor": null,"count": 4000,"theme": null,"project": null,"userType": null,"search": null,"isFuture": null}'

pbody <- list(query = query, variables = query_variables)
res <- POST(url, body = pbody, encode="json")

rparsed <- content(res, as = "parsed", encoding = "UTF-8")
totalcount <- rparsed$data$events$totalCount
rtext <- content(res, as = "text", encoding = "UTF-8")
x <- fromJSON (rtext, flatten = TRUE)
listevt <- bind_rows(x$data$events$edges)
listevt <- filter(listevt, !is.na(cursor))

# Mise au format date
listevt[,"node.createdAt"] <- as.POSIXct(listevt[,"node.createdAt"], format="%Y-%m-%d %H:%M:%S")
listevt[,"node.startAt"] <- as.POSIXct(listevt[,"node.startAt"], format="%Y-%m-%d %H:%M:%S")
listevt[,"node.endAt"] <- as.POSIXct(listevt[,"node.endAt"], format="%Y-%m-%d %H:%M:%S")

# Ajout d'une variable createdAt pour avoir le jour de création de l'event
listevt$createdAt <- as.character(round(listevt[,"node.createdAt"], "day"))
listevt$createdAt <- as.Date(listevt$createdAt, "%Y-%m-%d")

# Ajout d'une variable startAt pour avoir le jour de crÃ©ation de l'event
listevt$startAt <- as.character(round(listevt[,"node.startAt"], "day"))
listevt$startAt <- as.Date(listevt$startAt, "%Y-%m-%d")

# Ajout d'une variable weekDay pour avoir le jour de création de l'event
listevt$weekDay <- format(listevt$startAt, "%A")
listevt$weekDay <- factor (listevt$weekDay, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))

# Calcul du code postal et du dÃ©partementn à partir de l'adresse
r <- regexpr('[0-9]{5}',listevt$node.fullAddress)
r2 <- r +5
listevt$CP[r != -1] <- substr(listevt$node.fullAddress[r != -1], r[r != -1], r2[r != -1])
listevt$DEP <- substr(listevt$CP, 0, 2)
listevt$Commune[r != -1] <- toupper(substr(listevt$node.fullAddress[r != -1], r2[r != -1]+1, nchar(listevt$node.fullAddress[r != -1])))

# Read data commune & CP
#require(data.table)
#repcommune <- fread("data/correspondance-code-insee-code-postal.csv")
#repcommune <- repcommune %>%
#  rename('CP' = 'Code Postal') %>%
# select (CP, Commune)

# Ajout du nom de la commune
#test <- listevt %>%
#  left_join(repcommune, by = "CP")

# Ne pas charger trop tôt le module tm car fonction "content" masqué
require(tm)

# Construction du corpus de mot sur les titres
docs <- Corpus(VectorSource(listevt$node.title))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("french"))
# Supprimer votre propre liste de mots non désirés
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
mmots <- as.matrix(dtm)
tmots <- sort(rowSums(mmots),decreasing=TRUE)
dfmots <- data.frame(word = names(tmots),freq=tmots)


save(listevt,totalcount, dfmots, file=filecache)

