require(httr)
require(jsonlite)
require(dplyr)

filecache <- "cache/listevt.Rdata" # fichier de données en cache

if (file.exists(filecache)) {        # chargement des données en cache si elles existent
  load(filecache)
  } else {
    listevt <- data.frame()
    cursor <- 'null'
  }


# Définition des paramétres --------------------------------------------------

url <- "https://granddebat.fr/graphql/internal"
query <- "query EventRefetchRefetchQuery($cursor: String, $count: Int, $theme: ID, $project: ID, $userType: ID, $search: String, $isFuture: Boolean) { events(first: $count, after: $cursor, theme: $theme, project: $project, search: $search, userType: $userType, isFuture: $isFuture) { totalCount edges { node { id lat lng url fullAddress createdAt startAt endAt title fullAddress url themes { id title url } __typename } cursor } pageInfo { hasPreviousPage hasNextPage startCursor endCursor } } }"

count <- 100
i <- 0
nextPage <- TRUE
liste <- data.frame()
while (nextPage && i<20) {
  query_variables <- paste0('{"cursor": "', cursor, '","count": ', count, ',"theme": null,"project": null,"userType": null,"search": null,"isFuture": null}')
  i<-i+1
  print(i)
  print(query_variables)
  pbody <- list(query = query, variables = query_variables)
  res <- POST(url, body = pbody, encode="json")
  
  rparsed <- httr::content(res, as = "parsed", encoding = "UTF-8")
  totalcount <- rparsed$data$events$totalCount
  cursor <- rparsed$data$events$pageInfo$endCursor
  nextPage <- rparsed$data$events$pageInfo$hasNextPage

  rtext <- httr::content(res, as = "text", encoding = "UTF-8")
  x <- fromJSON (rtext, flatten = TRUE)
  liste <- rbind(liste, bind_rows(x$data$events$edges))
}
liste <- filter(liste, !is.na(cursor))


# Mise au format date
liste[,"node.createdAt"] <- as.POSIXct(liste[,"node.createdAt"], format="%Y-%m-%d %H:%M:%S")
liste[,"node.startAt"] <- as.POSIXct(liste[,"node.startAt"], format="%Y-%m-%d %H:%M:%S")
liste[,"node.endAt"] <- as.POSIXct(liste[,"node.endAt"], format="%Y-%m-%d %H:%M:%S")

# Ajout d'une variable createdAt pour avoir le jour de création de l'event
liste$createdAt <- as.character(round(liste[,"node.createdAt"], "day"))
liste$createdAt <- as.Date(liste$createdAt, "%Y-%m-%d")

# Ajout d'une variable startAt pour avoir le jour de création de l'event
liste$startAt <- as.character(round(liste[,"node.startAt"], "day"))
liste$startAt <- as.Date(liste$startAt, "%Y-%m-%d")

# Ajout d'une variable weekDay pour avoir le jour de création de l'event
liste$weekDay <- format(liste$startAt, "%A")
liste$weekDay <- factor (liste$weekDay, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))

# Calcul du code postal et du département à partir de l'adresse
r <- regexpr('[0-9]{5}',liste$node.fullAddress)
r2 <- r +5
liste$CP[r != -1] <- substr(liste$node.fullAddress[r != -1], r[r != -1], r2[r != -1])
liste$DEP <- substr(liste$CP, 0, 2)
liste$Commune[r != -1] <- toupper(substr(liste$node.fullAddress[r != -1], r2[r != -1]+1, nchar(liste$node.fullAddress[r != -1])))

listevt <- rbind(listevt,liste)

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

# Sauvegarde dans un fichier "cache"
save(listevt,cursor,totalcount, dfmots, file=filecache)

