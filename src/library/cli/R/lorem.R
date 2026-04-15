lorem_words <- c(
  "ad",
  "adipisicing",
  "aliqua",
  "aliquip",
  "amet",
  "anim",
  "aute",
  "cillum",
  "commodo",
  "consectetur",
  "consequat",
  "culpa",
  "cupidatat",
  "deserunt",
  "do",
  "dolor",
  "dolore",
  "duis",
  "ea",
  "eiusmod",
  "elit",
  "enim",
  "esse",
  "est",
  "et",
  "eu",
  "ex",
  "excepteur",
  "exercitation",
  "fugiat",
  "id",
  "in",
  "incididunt",
  "ipsum",
  "irure",
  "labore",
  "laboris",
  "laborum",
  "Lorem",
  "magna",
  "minim",
  "mollit",
  "nisi",
  "non",
  "nostrud",
  "nulla",
  "occaecat",
  "officia",
  "pariatur",
  "proident",
  "qui",
  "quis",
  "reprehenderit",
  "sint",
  "sit",
  "sunt",
  "tempor",
  "ullamco",
  "ut",
  "velit",
  "veniam",
  "voluptate"
)

lorem_ipsum <- function(
  paragraphs = 1,
  par_sentence_range = 5:10,
  sentence_word_range = 5:15
) {
  vcapply(
    1:paragraphs,
    function(x, ...) lorem_paragraph(...),
    par_sentence_range = par_sentence_range,
    sentence_word_range = sentence_word_range
  )
}

lorem_paragraph <- function(par_sentence_range, sentence_word_range) {
  num <- sample(par_sentence_range, 1)
  paste(
    collapse = " ",
    vcapply(
      1:num,
      function(x, ...) lorem_sentence(...),
      sentence_word_range = sentence_word_range
    )
  )
}

lorem_sentence <- function(sentence_word_range) {
  num <- sample(sentence_word_range, 1)
  words <- sample(lorem_words, num, replace = TRUE)
  words[1] <- capitalize(words[1])
  paste0(paste(words, collapse = " "), ".")
}

capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
