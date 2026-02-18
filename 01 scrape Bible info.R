library(XML)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

# the function to scrape info from BT 

get_verse_numbers <- function(n) {
  
  url <- paste0("https://biblia.deon.pl/rozdzial.php?id=", n)
  
  webpage <- tryCatch(readLines(url, warn = FALSE),
                      error = function(e) return("Page not found"))
  if (is.null(webpage)) {
    message("Failed to read page: ", n)
    return(NULL)
  }
  
  doc <- htmlParse(webpage, asText = TRUE)
<<<<<<< HEAD
=======
  
>>>>>>> 33a1c7c9265e820aba9a9251352c41e0120b3e7c
  book <- xpathSApply(doc,"//div[@class='book-label']", xmlValue)
  chapter <- xpathSApply(doc, "//div[@class='initial-letter']", xmlValue)
  verses <- xpathSApply(doc, "//a[starts-with(@name,'W')]", xmlGetAttr, "name")
  
  if (length(book) == 0 || length(chapter) == 0) {
    message("Missing book/chapter on page ID: ", n)
    return(NULL)
  }
  
  res <- data.frame(book = book, chapter = chapter, verses = length(verses))
  if (dim(res)[1] > 1 )
    res <- res[1,]
  return(res)
}

# BT urls https://biblia.deon.pl/rozdzial.php?id= range from 1:1401
# but 76 numbers in this range don't correspond to valid pages

ids <- c(1:1401)

res <- lapply(ids, get_verse_numbers)
nulls <- which(sapply(res, is.null)) 
res <- res[-nulls]

bt_by_chapter <- do.call(rbind, res) 
bt_by_chapter$chapter <- as.integer(bt_by_chapter$chapter)
names(bt_by_chapter)[3] <- "v_count"

# html with Bible books names and abbreviations

'<div class="miedzytytul1">ST - Stary Testament</div>
  <table cellspacing=0 border=0 cellpadding=1 width=100%> 
  <tr>
    <td><B>Rdz</B></td>
    <td>Ks. Rodzaju</td>
    <td><B>Prz</B></td>
    <td>Ks. Przysłów</td>
  </tr>
  
<div class="miedzytytul1">NT - Nowy Testament</div><br />
  <table cellspacing=0 border=0 cellpadding=1 width=100%> <tr><td><B>Mt</B></td><td>Ew. wg św. Mateusza</td><td><B>1 Tm</B></td><td>1 List do Tymoteusza</td></tr> <tr><td><B>Mk</B></td><td>Ew. wg św. Marka</td><td><B>2 Tm</B></td><td>2 List do Tymoteusza</td></tr> <tr><td><B>Łk</B></td><td>Ew. wg św. Łukasza</td><td><B>Tt</B></td><td>List do Tytusa</td></tr> <tr><td><B>J</B></td><td>Ew. wg św. Jana</td><td><B>Flm</B></td><td>List do Filemona</td></tr> <tr><td><B>Dz</B></td><td>Dzieje Apostolskie</td><td><B>Hbr</B></td><td>List do Hebrajczyków</td></tr> <tr><td><B>Rz</B></td><td>List do Rzymian</td><td><B>Jk</B></td><td>List św. Jakuba</td></tr> <tr><td><B>1 Kor</B></td><td>1 List do Koryntian</td><td><B>1 P</B></td><td>1 List św. Piotra</td></tr> <tr><td><B>2 Kor</B></td><td>2 List do Koryntian</td><td><B>2 P</B></td><td>2 List św. Piotra</td></tr> <tr><td><B>Ga</B></td><td>List do Galatów</td><td><B>1 J</B></td><td>1 List św. Jana</td></tr> <tr><td><B>Ef</B></td><td>List do Efezjan</td><td><B>2 J</B></td><td>2 List św. Jana</td></tr> <tr><td><B>Flp</B></td><td>List do Filipian</td><td><B>3 J</B></td><td>3 List św. Jana</td></tr> <tr><td><B>Kol</B></td><td>List do Kolosan</td><td><B>Jud</B></td><td>List św. Judy</td></tr> <tr><td><B>1 Tes</B></td><td>1 List do Tesaloniczan</td><td><B>Ap</B></td><td>Apokalipsa św. Jana</td></tr> <tr><td><B>2 Tes</B></td><td>2 List do Tesaloniczan</td><td><P>&nbsp;</td><td><P>&nbsp;</td></tr> </table><br />
'

url <- "https://biblia.deon.pl/menu.php?st_id=4"
<<<<<<< HEAD
page <- read_html(url, encoding = "ISO-8859-2")  
=======
page <- read_html(url, encoding = "ISO-8859-2")  # Polish Latin-2 encoding
>>>>>>> 33a1c7c9265e820aba9a9251352c41e0120b3e7c
rows <- page |> html_elements("table tr") 

res2 <- lapply(rows[1:37], function(row) {
  cells <- row |> html_elements("td") |> html_text(trim = TRUE)
  if (length(cells) == 4) {
    rbind(
      c(book = cells[1], full_name = cells[2]),
      c(book = cells[3], full_name = cells[4])
    )
  }
})

df <- do.call(rbind, res2)
df <- as.data.frame(df)
df <- df[1:73,]

# but rows are not in the right order: 1,3,2,4,5... etc
# first df has to be divided into ST (up to id: 46) and NT: 47:73 

ids <- seq(1,45, 2)
st <- rbind(df[ids,], df[ids+1,])
st$part <- "ST"
ids <- seq(47,73, 2)
nt <- rbind(df[ids,], df[ids+1,])
nt <- nt[-28,]
nt$part <- "NT"
bt <- rbind(st, nt)
bt$id <- 1:73
bt <- bt[, c(4,3,1,2)]

bt_by_chapter$book <- str_replace_all(bt_by_chapter$book, "Księga", "Ks.")
bt_by_chapter$book <- str_replace_all(bt_by_chapter$book, "Ewangelia", "Ew.")

bt_by_chapter <- left_join(bt_by_chapter, select(bt, id, part, book, full_name), join_by(book == full_name))
bt_by_chapter <- bt_by_chapter[,c(4,5,6,2,3)]
names(bt_by_chapter)[3] <- "book"
bt_by_chapter <- bt_by_chapter |> arrange(id)

# Some chapters are missing due to bad HTML code (no "W" identifier)

check_missing_chapters <- function(df) {
  
  groups <- split(df$chapter, df$book)
  
  missing <- lapply(groups, function(book) {
    expected <- 1:max(book) 
    found <- sort(unique(book))
    setdiff(expected, found) 
  })
  
  missing_books <- missing[sapply(missing, length) > 0]
  
  if(length(missing_books) == 0) {
    cat("All chapters complete\n")
  } else {
    cat("Missing chapters\n")
    for(book in names(missing_books)) {
      cat(sprintf("  %s: chapters %s\n", book, paste(missing_books[[book]], collapse = ", ")))
    }
  }
}

check_missing_chapters(bt_by_chapter )

<<<<<<< HEAD
# add missing rows manually
=======
# add missing rows
>>>>>>> 33a1c7c9265e820aba9a9251352c41e0120b3e7c
# Hi 9, 35
# Hi 28, 28
# Syr 6, 37

bt_by_chapter |> filter(book %in% c("Hi", "Syr") & chapter == 1)

missing_verses <- data.frame(id = c(22, 22, 28), 
                             part = rep("ST", 3), 
                             book = c("Hi", "Hi", "Syr"),
                             chapter = c(9,28,6),
                             v_count = c(35,28,37))

bt_by_chapter <- rbind(bt_by_chapter, missing_verses)
bt_by_chapter <- bt_by_chapter |> arrange(id, chapter)

# add chapter and verse counts

bt <- bt_by_chapter |> group_by(book) |> 
                       summarise(ch_count = n(),
                                 v_count = sum(v_count), .groups = "drop") |> 
                        left_join(bt |> select(id, part, book, full_name)) |>  
                        arrange(id) |> select(4,5,1,6,2,3)

rm(df, url, missing_verses, st, nt, res, res2, page, ids, nulls, rows)
<<<<<<< HEAD
=======


save(compare_by_book, compare_by_chpt, file = "outputs_github/compare_files.RData")
>>>>>>> 33a1c7c9265e820aba9a9251352c41e0120b3e7c
