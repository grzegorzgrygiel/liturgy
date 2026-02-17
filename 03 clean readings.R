
# the function to extract individual verses

extract_verses <- function(df) {
  
  verses <- sapply(seq_along(df$readings), \(i) {
    readings <- df$readings[i]
    str_split(readings, "; ")
  }
  ) 
  unlist(verses)
}

# helper functions to inspect the output

view2 <- function(vector) {
  View(as.matrix(vector))
}

names2 <- function(df) {
  as.matrix(names(df))
}

# *********** EXTRACT VERSES ********************

verses_raw <- extract_verses(yrs_abc)
verses_raw <- verses_raw[verses_raw != ""]
verses_raw <- verses_raw[verses_raw != "-"] 
view2(verses_raw)

# *********** CLEANING STEPS ********************

verses <- verses_raw |> 
  # remove  (R.: por. 1bc) or (R: por, 12)
  str_remove("\\(R[.:][^)]+\\)") |> 
  # remove Por. 
  str_remove("^[Pp]or.\\s*") |> 
  # remove numbers in brackets e.g. (54)
  str_remove("\\(\\d+[A-Z]?\\)") 
  
view2(verses)
# identify strings without the book name: 
indeksy <- which(str_detect(verses, "^\\d+,"))
verses[indeksy]

# examples
yrs_abc[30,5]
yrs_abc[64,5]
yrs_abc[92,5]

c(verses[indeksy[1]-1], verses[indeksy[1]])
c(verses[indeksy[2]-1], verses[indeksy[2]])
c(verses[indeksy[3]-1], verses[indeksy[3]])

# extract the book name from the preceding verse and combine it with the following string where it is missing
for (i in indeksy) {
  verses[i] <- paste(str_extract(verses[i-1], "^[\\p{L}\\d\\s]+?(?=\\s\\d+,)"), verses[i]) 
}
verses[indeksy]

verses <- verses |> 
  # remove letters after numbers e.g. 1b => 1
  str_replace_all("(\\d)([A-Za-z]+)", "\\1") |>       
  str_replace_all(" i ", " ") |>                   
  str_replace_all("\\.", ", ") |> 
  str_replace_all("\\s+i\\s+", " ")  |> 
  str_replace_all(",\\s+", ", ") |> 
  str_squish() |> 
  str_trim() |> 
  str_replace_all("\\s+,", ",") |> 
  str_replace_all("(?<=\\d)\\s+(?=\\d)", ",")

indeksy <- which(str_detect(verses, "^\\d+,"))
# 2 verses left: they will be corrected later: "1,3, 11-21"  "12, 1-6, 10"
verses[indeksy]

# identify strings without the book name before a new chapter

indeksy <- which(str_detect(verses, " – "))

verses[indeksy]

verses_2 <- c(verses[indeksy])

new_verses <- c()

# remove the part after --

for (i in indeksy) {
  
  verses[i] <-  str_remove_all(verses[i], " –.*")
}

verses[indeksy]

# combine the book name with the verses info and save it as a separate string

for (i in seq_along(verses_2)) {
  
  new_verses[i] <- paste(str_extract(verses_2[i], "^[\\p{L}0-9\\s]+?(?=\\s\\d+,)"), 
                         str_extract(verses_2[i], "(?<= – ).*")) 
}

verses <- c(verses, new_verses)
verses <- sort(verses)
verses <- unique(verses)
view2(verses)

# correct problems
# missing book name: "1,3, 11-21"  "12, 1-6, 10"

indeksy <- which(str_detect(verses, "^\\d+,"))
verses[indeksy]
# remove "12, 1-6, 10"
verses[169]
verses <- verses[-169]
# replace 1,3, 11-21 with 1 J 3, 11-21
yrs_abc[40,5]
verses[168] <- "1 J 3, 11-21"

# reformat books with only one chapter
verses[169] <- "2 J 1, 4-9"
verses[253] <- "3 J 1, 5-8"
verses[464] <- "Flm 1, 7-20"
verses[465] <- "Flm 1, 9-10, 12-17"

# Est 4, 17, l-m, r-u
verses[432] <- "Est 4, 17"

# J 8, 1-11/J 8, 12-20
verses[778] <- "J 8, 1-11, 12-20"

# PnP 2, 8-14
verses[1347]  <- "Pnp 2, 8-14"

# aklamacja
verses[257]
verses <- verses[-257] 

view2(verses)

rm(i, indeksy, new_verses, verses_raw, verses_2)