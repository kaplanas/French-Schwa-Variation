cities = c("Abidjan", "Bamako", "Burkina Faso", "Cameroun", "RCA Bangui", "Senegal Dakar", "Chlef", "Ile de la Reunion", "Gembloux", "Liege", "Tournai", "Peace River", "Quebec ville (universite)", "Biarritz", "Dijon", "Nantes", "Vendee", "Aveyronnais a Paris", "Brunoy", "Paris Centre ville", "Puteaux-Courbevoie", "Douzens", "Ogaviller", "Lacaune", "Rodez", "Toulouse", "Bracey", "Domfrontais", "Aix-Marseille", "Marseille Centre Ville", "Grenoble", "Lyon", "Roanne", "Geneve", "Neuchatel", "Nyon")
regions = c("Afrique subsaharienne", "Cote d'Ivoire", "Maghreb", "Maurice - Ile de la Reunion", "Bruxelles", "Wallonie", "Alberta", "Colombie-Brittanique", "Quebec", "Alsace", "Aquitaine", "Bourgogne", "Bretagne", "Centre", "Champagne-Ardennes", "Hautes-Alpes", "Ile de France", "Languedoc-Roussillon", "Lorraine", "Midi-Pyrenees", "Nord", "Normandie", "Provence Cote d'Azur", "Pyrenees Atlantiques", "Rhone Alpes", "Liban", "Geneve", "Neuchatel", "Conton de Vaud", "Louisiane")
zones = c("Afrique", "Antilles - Ocean indien", "Belgique", "Canada", "France", "Moyen-Orient", "Suisse", "USA")

city_to_region <- function(cs) {
  return(
    ifelse(is.element(cs, c("Abidjan", "Bamako", "Burkina Faso", "Cameroun", "RCA Bangui", "Senegal Dakar")), "Afrique subsaharienne",
      ifelse(is.element(cs, c("Chlef")), "Maghreb",
        ifelse(is.element(cs, c("Ile de la Reunion")), "Maurice - Ile de la Reunion",
          ifelse(is.element(cs, c("Gembloux", "Liege", "Tournai")), "Wallonie",
            ifelse(is.element(cs, c("Peace River")), "Alberta",
              ifelse(is.element(cs, c("Quebec ville (universite)")), "Quebec",
                ifelse(is.element(cs, c("Biarritz")), "Aquitaine",
                  ifelse(is.element(cs, c("Dijon")), "Bourgogne",
                    ifelse(is.element(cs, c("Nantes")), "Bretagne",
                      ifelse(is.element(cs, c("Vendee")), "Centre",
                        ifelse(is.element(cs, c("Aveyronnais a Paris", "Brunoy", "Paris Centre ville", "Puteaux-Courbevoie")), "Ile de France",
                          ifelse(is.element(cs, c("Douzens")), "Languedoc-Roussillon",
                            ifelse(is.element(cs, c("Ogaviller")), "Lorraine",
                              ifelse(is.element(cs, c("Lacaune", "Rodez", "Toulouse")), "Midi-Pyrenees",
                                ifelse(is.element(cs, c("Bracey", "Domfrontais")), "Normandie",
                                  ifelse(is.element(cs, c("Aix-Marseille", "Marseille Centre Ville")), "Provence Cote d'Azur",
                                    ifelse(is.element(cs, c("Grenoble", "Lyon", "Roanne")), "Rhone Alpes",
                                      ifelse(is.element(cs, c("Geneve")), "Geneve",
                                        ifelse(is.element(cs, c("Neuchatel")), "Neuchatel",
                                          ifelse(is.element(cs, c("Nyon")), "Conton de Vaud", "OTHER"))))))))))))))))))))
  )
}

region_to_cities <- function(rs) {
  return(cities[is.element(city_to_region(cities), rs)])
}

region_to_zone <- function(rs) {
  return(
    ifelse(is.element(rs, c("Afrique subsaharienne", "Cote d'Ivoire", "Maghreb")), "Afrique",
      ifelse(is.element(rs, c("Maurice - Ile de la Reunion")), "Antilles - Ocean indien",
        ifelse(is.element(rs, c("Bruxelles", "Wallonie")), "Belgique",
          ifelse(is.element(rs, c("Alberta", "Colombie-Brittanique", "Quebec")), "Canada",
            ifelse(is.element(rs, c("Alsace", "Aquitaine", "Bourgogne", "Bretagne", "Centre", "Champagne-Ardennes", "Hautes-Alpes", "Ile de France", "Languedoc-Roussillon", "Lorraine", "Midi-Pyrenees", "Nord", "Normandie", "Provence Cote d'Azur", "Pyrenees Atlantiques", "Rhone Alpes")), "France",
              ifelse(is.element(rs, c("Liban")), "Moyen-Orient",
                ifelse(is.element(rs, c("Geneve", "Neuchatel", "Conton de Vaud")), "Suisse",
                  ifelse(is.element(rs, c("Louisiane")), "USA", "OTHER"))))))))
  )
}

zone_to_regions <- function(zs) {
  return(regions[is.element(region_to_zone(regions), zs)])
}

zone_to_cities <- function(zs) {
  return(region_to_cities(zone_to_regions(zs)))
}

load_data <- function() {
  speakers <- read.csv("5 Speaker Info.csv")
  speakers$Region = city_to_region(speakers$City)
  speakers$Zone = region_to_zone(speakers$Region)
  speakers <<- speakers
  read_1 <- read.csv("1 Texte lu cleaned.csv")
  read_2 <- read.csv("1 Texte lu part 2 cleaned.csv")
  read <- rbind(read_1, read_2)
  read$Discourse = "read"
  guided_1 <- read.csv("3 Discussion guidee cleaned.csv")
  guided_2 <- read.csv("3 Discussion guidee part 2 cleaned.csv")
  guided <- rbind(guided_1, guided_2)
  guided$Discourse = "guided"
  free <- read.csv("4 Discussion libre cleaned.csv")
  free$Discourse = "free"
  tokens <- rbind(read, guided, free)
  tokens$Codes = as.character(tokens$Codes)
  tokens$Schwa = "X"
  tokens$Word = "X"
  tokens$Left = "X"
  tokens$Right = "X"
  tokens$Codes = ifelse(nchar(tokens$Codes) == 3, paste("0", tokens$Codes, sep = ""), tokens$Codes)
  temp = substr(tokens$Codes, start = 1, stop = 1)
  tokens$Schwa = ifelse(temp == "0", "no", ifelse(temp == "1", "yes", ifelse(temp == "2", "unclear", "NA")))
  temp = substr(tokens$Codes, start = 2, stop = 2)
  tokens$Word = ifelse(temp == "1", "monosyl", ifelse(temp == "2", "polysyl1", ifelse(temp == "3", "polysyl2", ifelse(temp == "4", "polysylend", ifelse(temp == "5", "metathesis", "NA")))))
  temp = substr(tokens$Codes, start = 3, stop = 3)
  tokens$Left = ifelse(temp == "1", "V", ifelse(temp == "2", "C", ifelse(temp == "3", "boundary", ifelse(temp == "4", "uncertain", ifelse(temp == "5", "CC", "NA")))))
  temp = substr(tokens$Codes, start = 4, stop = 4)
  tokens$Right = ifelse(temp == "1", "V", ifelse(temp == "2", "C", ifelse(temp == "3", "boundarystrong", ifelse(temp == "4", "boundaryweak", "NA"))))
  tokens <<- tokens
}
