# the parsing function

parse_ranges <- function(str) {
  ranges <- strsplit(str, ",\\s*")[[1]] 
  vec <- unlist(lapply(ranges, function(r) eval(parse(text = r))))
  as.numeric(vec)
}

# df with counts of liturgical verses per chapter

books <- character(length(verses))
chapters <- integer(length(verses))
verses_list <- vector("list", length(verses))

for (i in seq_along(verses)) {
  str <- verses[i]
  books[i] <- str_extract(str, "^[\\p{L}\\d\\s]+?(?=\\s\\d+,)")
  str <- str_replace(str, "^[\\p{L}\\d\\s]+?(?=\\s\\d+,)", "")
  str <- str_trim(str)
  chapters[i] <- str_extract(str, "^[\\d]+")
  str <- str_replace_all(str, "^[\\d]+,", "")
  str <- str_trim(str)
  str <- str_replace_all(str, "-", ":")
  verses_list[[i]] <- as.integer(parse_ranges(str))
}

liturgy_verses <- data.frame(
  book = books,
  chapter = chapters,
  verses = I(verses_list), 
  stringsAsFactors = FALSE,
  check.names = FALSE
)

liturgy_verses <- liturgy_verses |> mutate(v.count = lengths(verses)) 
liturgy_verses$chapter <- as.integer(liturgy_verses$chapter)
liturgy_verses <- left_join(liturgy_verses, select(bt, id, part, book))
liturgy_verses <- liturgy_verses |>  arrange(id, chapter) |> as.data.frame() |> select(id, book, part, chapter, verses, v.count)

#  ************  COMPARE BY CHAPTER ************ 

lit_ver_by_chpt <- liturgy_verses %>%
  group_by(id, part, book, chapter) %>%
  summarise(
    verses = list(sort(unique(unlist(verses)))),
    v_count = map_int(verses, length), 
    .groups = "drop" 
  )

# combine with bt_by_chapter and calculate percentages

compare_by_chpt <-  bt_by_chapter  |> 
  left_join(lit_ver_by_chpt|> select(book, chapter, verses, v_count), join_by(book, chapter)) |> 
  rename(v_Bible = v_count.x, v_liturgy = v_count.y) |> select(id, part, book, chapter, v_Bible, v_liturgy, verses)

compare_by_chpt <- compare_by_chpt  |> mutate(v_liturgy = replace_na(v_liturgy, 0),
                                                   perc.vers = round(v_liturgy/v_Bible*100,1)) |> select(1:6,8,7) |> 
                                      arrange()

# ************ COMPARE BY BOOK ************ 

# df with combined counts of chapters and verses in the liturgy

lit_ver_by_book <- liturgy_verses |>  group_by(id, book) |> 
  summarise(ch_count = n_distinct(chapter)) |> arrange(id)

lit_ver_by_book  <- lit_ver_by_chpt |> group_by(book) |> summarise(v_count = sum(v_count), .groups = "drop") |> 
  left_join(lit_ver_by_book |> select(id, book, ch_count)) |> arrange(id) |> select(3,1,4,2)

# combine counts for the Bible and liturgical readings

compare_by_book <- bt |> select(id, part, book, ch_count, v_count) |> 
                         left_join(lit_ver_by_book |> select(book, ch_count, v_count), by = "book") |> 
                                  rename(ch_Bible = ch_count.x, 
                                  v_Bible = v_count.x, 
                                  ch_liturgy = ch_count.y,
                                  v_liturgy = v_count.y) |> 
                         mutate(ch_liturgy = replace_na(ch_liturgy, 0),
                                v_liturgy = replace_na(v_liturgy, 0),
                                perc_chpt = round(ch_liturgy/ch_Bible*100,1),
                                perc_vers = round(v_liturgy/v_Bible*100,1),) |> 
                        select(1:3,4,6,8,5,7,9)

rm(books, chapters, str, verses_list, i)

# basic stats

compare_by_part <- compare_by_book  |> group_by(part) |> summarise(tot_v_Bible = sum(v_Bible),
                                                tot_v_lit = sum(v_liturgy),
                                                tot_ch_Bible = sum(ch_Bible),
                                                tot_ch_lit = sum(ch_liturgy)
                                                , .groups = "drop") |> 
                                        mutate(perc_vers = round(tot_v_lit/tot_v_Bible*100, 1),
                                               perc_chpt = round(tot_ch_lit/tot_ch_Bible*100, 1)) |> 
                                        as.data.frame() |> select(1,4,5,7,2,3,6) |> arrange(desc(part))
