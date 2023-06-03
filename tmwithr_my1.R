# Text Mining with R - A tidy approach
# Julia Silge & David Robinson
# https://github.com/juliasilge/tidytext
# 
library(tidytext)


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text


library(dplyr)
text_df <- tibble(col_line = 1:4, col_txt = text)

head(text_df)


library(tidytext)

text_df %>%
  unnest_tokens(col_tkn, col_txt)



library(janeaustenr)
library(dplyr)
library(stringr)


austen_books()

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(col_linenumber = row_number(),
         col_chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()
original_books




library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(col_word, text)

tidy_books




data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words, by = c("col_word"="word"))

tidy_books


tidy_books %>%
  count(col_word, sort = TRUE) 



library(ggplot2)

tidy_books %>%
  count(col_word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(col_word = reorder(col_word, n)) %>%
  ggplot(aes(n, col_word)) +
  geom_col() +
  labs(y = NULL)



############
############
############
############
############
############
############


library(gutenbergr)

# H.G. Wells
hgwells <- gutenberg_download(c(35, 36, 5230, 159))


tidy_hgwells <- hgwells %>%
  unnest_tokens(col_word, text) %>%
  anti_join(stop_words, by = c("col_word"="word"))

tidy_hgwells %>%
  count(col_word, sort = TRUE)


# Brontë sisters
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(col_word, text) %>%
  anti_join(stop_words, by = c("col_word"="word"))


tidy_bronte %>%
  count(col_word, sort = TRUE)



# Now, let’s calculate the frequency for each word for the works of Jane Austen, the Brontë sisters, and H.G. Wells by binding the data frames together. 
# 
# 
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(col_word = str_extract(col_word, "[a-z']+")) %>%
  count(author, col_word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

frequency

head(frequency)



library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = col_word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)
