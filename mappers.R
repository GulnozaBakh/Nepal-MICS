"Mappers
Renaming several features to make them consistent across the datasets"

# Ethnicity Mapping for 2014 Data
ethnicity_mapping1 <- list(
  "Brahman or Chhetri" = c("Brahman - Hill", "Brahman - Tarai", "Chame", "Chhetree",
        "Dev", "Hajam/Thakur", "Kayastha", "Rajput", "Sanyasi/Dashnami", "Thakuri"),
  "Tarai or Madhesi Other Castes" = c("Badhaee", "Bantaba", "Bantar/Sardar", 
        "Baraee ", "Bin", "Dhankar/Kharikar", "Haluwai", "Kahar", "Kalwar", "Kanu", 
        "Kathbaniyan", "Kewat", "Khaling", "Koiri/Kushwaha", "Kumhar", "Kurmi", 
        "Lodh", "Lohar", "Mali", "Mallaha", "Nuniya", "Rajbhar", "Sonar", "Sudhi", 
        "Teli", "Terai Others", "Yadav" ),
  "Dalits" = c("Chamar/Harijan/Ram", "Dalit Others", "Damai/Dholi", "Dhobi", 
        "Dusadh/Pasawan/Pasi", "Gaderi/Bhedhar", "Kami", "Khatwe", "Kori", "Musahar", 
        "Rajdhob", "Sarki", "Tatma/Tatwa"),
  "Newar" = c("Newar"),
  "Janajati" = c("Bhote", "Chamling", "Chepang/Praja ", "Chhantyal/Chhantel", "Danuwar ", 
        "Dhanuk", "Dhimal", "Gangai", "Ghale", "Gharti/Bhujel", "Gurung", "Janajati Others",
        "Jhangad/Dhagar", "Jirel", "Kumal", "Kulung", "Limbu", "Lhomi", "Magar", "Majhi", 
        "Mewahang Bala", "Nachhiring", "Rai", "Rajbansi", "Samgpang", "Satar/Santhal", 
        "Sherpa", "Sunuwar", "Tajpuriya", "Tamang", "Surel", "Thakali", "Thami", "Tharu", 
        "Yakkha", "Thulung", "Yamphu" ),
  "Muslim" = c("Churaute", "Musalman"),
  "Other" = c("Undefined Others")
)

map_ethnicity1 <- function(ethnicity) {
  if (is.na(ethnicity)) {
    return("Missing")
  }
  for (group in names(ethnicity_mapping1)) {
    if (ethnicity %in% ethnicity_mapping1[[group]]) {
      return(group)
    }
  }
  return("Other")  # Default to "Other" if no match is found
}

# Ethnicity Mapping for 2019 data
ethnicity_mapping2 <- list(
  "Brahman or Chhetri" = c("Amat", "Brahman - Hill", "Brahman - Tarai", "Chhetree", 
                          "Hajam/Thakur", "Rajput", "Sanyasi/Dashnami", "Thakuri"),
  "Tarai or Madhesi Other Castes" = c("Bantar/Sardar", "Bantaba", "Baraee", "Haluwai",
                         "Kahar", "Kalwar", "Kanu", "Kathbaniyan", "Kewat", "Koiri/Kushwaha", 
                         "Kumhar", "Kurmi", "Lodh", "Lohar", "Mali", "Mallaha", "Nuniya", 
                         "Rajbhar", "Sonar", "Sudhi", "Teli", "Terai Others", "Yadav"),
  "Dalits" = c("Badi", "Chamar/Harijan/Ram", "Dalit Others", "Damai/Dholi", "Dhobi", "Dom",
                         "Dusadh/Pasawan/Pasi", "Gaderi/Bhedhar", "Kami", "Kori", "Musahar", 
               "Sarki", "Tatma/Tatwa"),
  "Newar" = c("Newar"),
  "Janajati" = c("Chhantyal/Chhantel", "Danuwar", "Dhanuk", "Gangai", "Ghale", "Gharti/Bhujel", 
                          "Gurung", "Janajati Others", "Jhangad/Dhagar", "Kisan", "Kumal",
                          "Kusunda", "Limbu", "Magar", "Majhi", "Mewahang Bala", "Pahari", 
                          "Rai", "Rajbansi", "Raute", "Satar/Santhal", "Sherpa", "Sunuwar",
                          "Tamang", "Thakali", "Tharu" ),
  "Muslim" = c("Musalman"),
  "Other" = c("Others")
)

# Function to map ethnicities to groups, including handling NAs
map_ethnicity2 <- function(ethnicity) {
  if (is.na(ethnicity)) {
    return("Missing")
  }
  for (group in names(ethnicity_mapping2)) {
    if (ethnicity %in% ethnicity_mapping2[[group]]) {
      return(group)
    }
  }
  return("Other")  
}