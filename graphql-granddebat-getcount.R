# Grand débat, 2019
# Exemple de requête GraphQL pour récupérer le nombre d'événements organisés en France

# Modules nécessaires 
require(httr)
require(jsonlite)

# Principales variables

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
query_variables <- '{"cursor": null,"count": 3000,"theme": null,"project": null,"userType": null,"search": null,"isFuture": null}'
pbody <- list(query = query, variables = query_variables)

# Appel à l'API
res <- POST(url, body = pbody, encode="json", verbose())

# Vérification de la réponse
http_type(res)

# Lecture du résultat
rparsed <- content(res, as = "parsed", encoding = "UTF-8")

# Affichage du compteur
rparsed$data$events$totalCount
