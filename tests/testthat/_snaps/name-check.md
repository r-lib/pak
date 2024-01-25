# pkg_name_check

    Code
      pkg_name_check("tools")
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Wikipedia -------------------------------------------------------------------+
      | Tool (from Tools) A tool is an object that can extend an individual's        |
      | ability to modify features of the surrounding environment or help them       |
      | accomplish a particular task. Although many animals use simple tools, only   |
      | human beings, whose use of stone tools dates back hundreds of millennia,     |
      | have been observed using tools to make other tools.                          |
      |                                                                              |
      | ...                                                                          |
      +------------------------------------------ https://en.wikipedia.org/wiki/Tool +
      + Wiktionary ------------------------------------------------------------------+
      | tools Noun: tools                                                            |
      | plural of tool                                                               |
      | Verb: tools                                                                  |
      | third-person singular simple present indicative form of tool                 |
      | Anagrams: loots, lotos, sloot, sotol, stool, tosol                           |
      +---------------------------------------- https://en.wiktionary.org/wiki/tools +
      +------------------------------------------------------------------------------+
      | Sentiment: :| (0)                                                            |
      +------------------------------------------------------------------------------+

---

    Code
      pkg_name_check("tools", "urban")
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Urban dictionary ------------------------------------------------------------+
      | slang for the [hypodermic] [needles] used to [inject] drugs.                 |
      +-------------------------------------------- http://tools.urbanup.com/1443484 +

# format.pak_pkg_name_check, print.pak_pkg_name_check

    Code
      print(ret[[1]])
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Wikipedia -------------------------------------------------------------------+
      | Tool (from Tools) A tool is an object that can extend an individual's        |
      | ability to modify features of the surrounding environment or help them       |
      | accomplish a particular task. Although many animals use simple tools, only   |
      | human beings, whose use of stone tools dates back hundreds of millennia,     |
      | have been observed using tools to make other tools.                          |
      |                                                                              |
      | ...                                                                          |
      +------------------------------------------ https://en.wikipedia.org/wiki/Tool +
      + Wiktionary ------------------------------------------------------------------+
      | tools Noun: tools                                                            |
      | plural of tool                                                               |
      | Verb: tools                                                                  |
      | third-person singular simple present indicative form of tool                 |
      | Anagrams: loots, lotos, sloot, sotol, stool, tosol                           |
      +---------------------------------------- https://en.wiktionary.org/wiki/tools +
      +------------------------------------------------------------------------------+
      | Sentiment: :| (0)                                                            |
      +------------------------------------------------------------------------------+

---

    Code
      print(ret[[2]])
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Urban dictionary ------------------------------------------------------------+
      | slang for the [hypodermic] [needles] used to [inject] drugs.                 |
      +-------------------------------------------- http://tools.urbanup.com/1443484 +

