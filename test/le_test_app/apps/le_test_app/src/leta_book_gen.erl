-module(leta_book_gen).

-export([word/1, words/2, words_to_binary/1, book/2]).

-define(WORD_SIZE, 10).

word(Size) -> word(Size, []).

word(0, Acc) -> Acc;
word(Size, Acc) ->
  Char = 96 + rand:uniform(25),
  word(Size-1, [Char | Acc]).

words(MaxWordSize, Number) -> words(MaxWordSize, Number, []).

words(_, 0, Acc) -> Acc;
words(MaxWordSize, Number, Acc) ->
  WordSize = rand:uniform(MaxWordSize),
  words(MaxWordSize, Number - 1, [word(WordSize) | Acc]).

words_to_binary(Words) -> iolist_to_binary(lists:join(" ", Words)).

book(ChapterCount, WordsPerChapter) -> book(ChapterCount, WordsPerChapter, []).

book(0, _, Acc) -> Acc;
book(ChapterCount, WordsPerChapter, Acc) ->
  Chapter = words_to_binary(words(?WORD_SIZE, WordsPerChapter)),
  ChapterName = "# Chapter" ++ integer_to_list(ChapterCount) ++ "\n",
  Chapters = book(ChapterCount - 1, WordsPerChapter, [ChapterName, Chapter | Acc]),
  iolist_to_binary(Chapters).
