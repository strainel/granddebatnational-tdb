require(httr)
require(jsonlite)
require(dplyr)

# Définition des paramètres --------------------------------------------------

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

# Ajout d'une variable startAt pour avoir le jour de création de l'event
listevt$startAt <- as.character(round(listevt[,"node.startAt"], "day"))
listevt$startAt <- as.Date(listevt$startAt, "%Y-%m-%d")

# Ajout d'une variable weekDay pour avoir le jour de création de l'event
listevt$weekDay <- format(listevt$startAt, "%A")
listevt$weekDay <- factor (listevt$weekDay, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))

# Calcul du code postal et du départementn à partir de l'adresse
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
