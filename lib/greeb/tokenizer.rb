# encoding: utf-8
require 'json'
# Greeb's tokenization facilities. Use 'em with love. 
#
# Unicode character categories been obtained from
# <http://www.fileformat.info/info/unicode/category/index.htm>.
#
module Greeb::Tokenizer extend self
  LI = /([\p{L}]+ ли)\b/u
  NI = /(н[ие] [\p{L}]+)\b/u
  Bb1 = /([\p{L}]+ бы?)\b/u
  GEG = /([\p{L}]+ жеж?)\b/u
  KA = /([\p{L}]+[\- ]ка)\b/u
  TO = /([\p{L}]+[\- ]то)\b/u
  TAKI = /(вс[её] ?- ?таки)\b/u
  DATE = /(\d?\d[\.\/]\d?\d[\/ \.]\d{4}|\d?\d[\/ \.]\d?\d[\/ \.]\d{2})/
  DATE2 = /(\d?\d[\/ \.](янв|фев|мар|апр|май|июн|июл|авг|сент|окт|ноя|дек)[\p{L}]{0,9}[\/ \.](\d{4}|\d{2}))/
  TIME = /\d?\d[\.\:]\d?\d/
  HAPPINESS = /\:\)+/u
  SAD = /\:\(+/u
  HAPPINESS2 = /\:D+/u

  # English and Russian letters.
  #
  LETTERS = /[\p{L}]+/u

  # Floating point values.
  #
  FLOATS = /(\d+)[.,](\d+)/u

  # Integer values.
  #
  INTEGERS = /\d+/u

  # In-sentence punctuation character (i.e.: "," or "-").
  #
  SENTENCE_PUNCTUATIONS = /(\,|\-|:|;|\p{Ps}|\p{Pi}|\p{Pf}|\p{Pe})+/u

  # Punctuation character (i.e.: "." or "!").
  #
  PUNCTUATIONS = /[(\.|\!|\?)]+/u

  # In-subsentence seprator (i.e.: "*" or "=").
  #
  SEPARATORS = /[\p{Nl}\p{No}\p{Pd}\p{Pc}\p{Po}\p{Sm}\p{So}\p{Sc}\p{Zl}\p{Zp}]+/u

  # Spaces (i.e.: " " or &nbsp).
  #
  SPACES = /[\p{Zs}\t]+/u

  # Line breaks.
  #
  BREAKS = /(\r\n|\n|\r)+/u

  # Residuals.
  #
  RESIDUALS = /([\p{C}\p{M}\p{Sk}]|[\p{Nd}&&[^\d]])+/u

  # Perform the tokenization process.
  #
  # @param text [String] a text to be tokenized.
  #
  # @return [Array<Greeb::Span>] a set of tokens.
  #
  def tokenize text
    scanner = Greeb::StringScanner.new(text)
    tokens = []
    while !scanner.eos?
      parse! scanner, tokens, LI, :letter or
      parse! scanner, tokens, NI, :letter or
      parse! scanner, tokens, Bb1, :letter or
      parse! scanner, tokens, GEG, :letter or
      parse! scanner, tokens, KA, :letter or
      parse! scanner, tokens, TO, :letter or
      parse! scanner, tokens, TAKI, :letter or
      parse! scanner, tokens, HAPPINESS, :letter or
      parse! scanner, tokens, SAD, :letter or
      parse! scanner, tokens, HAPPINESS2, :letter or
      parse! scanner, tokens, DATE, :letter or
      parse! scanner, tokens, DATE2, :letter or
      parse! scanner, tokens, TIME, :letter or
      parse! scanner, tokens, LETTERS, :letter or
      parse! scanner, tokens, FLOATS, :float or
      parse! scanner, tokens, INTEGERS, :integer or
      split_parse! scanner, tokens, SENTENCE_PUNCTUATIONS, :spunct or
      split_parse! scanner, tokens, PUNCTUATIONS, :punct or
      split_parse! scanner, tokens, SEPARATORS, :separ or
      split_parse! scanner, tokens, SPACES, :space or
      split_parse! scanner, tokens, BREAKS, :break or
      parse! scanner, tokens, RESIDUALS, :residual or
      raise Greeb::UnknownSpan.new(text, scanner.char_pos)
    end
    tokens
  ensure
    scanner.terminate
  end

  # Split one line into characters array, but also combine duplicated
  # characters.
  #
  # For instance, `"a b\n\n\nc"` would be transformed into the following
  # array: `["a", " ", "b", "\n\n\n", "c"]`.
  #
  # @param token [String] a token to be splitted.
  #
  # @return [Array<String>] splitted characters.
  #
  def split(token)
    token.scan(/((.|\n)\2*)/).map!(&:first)
  end

  protected
  # Try to parse one small piece of text that is covered by pattern
  # of necessary type.
  #
  # @param scanner [Greeb::StringScanner] string scanner.
  # @param tokens [Array<Greeb::Span>] result array.
  # @param pattern [Regexp] a regular expression to extract the token.
  # @param type [Symbol] a symbol that represents the necessary token
  #   type.
  #
  # @return [Array<Greeb::Span>] the modified set of extracted tokens.
  #
  def parse! scanner, tokens, pattern, type
    return false unless token = scanner.scan(pattern)
    position = scanner.char_pos
    tokens << Greeb::Span.new(position - token.length,
                                position,
                                type)
  end

  # Try to parse one small piece of text that is covered by pattern
  # of necessary type. This method performs grouping of the same
  # characters.
  #
  # @param scanner [Greeb::StringScanner] string scanner.
  # @param tokens [Array<Greeb::Span>] result array.
  # @param pattern [Regexp] a regular expression to extract the token.
  # @param type [Symbol] a symbol that represents the necessary token
  #   type.
  #
  # @return [Array<Greeb::Span>] the modified set of extracted tokens.
  #
  def split_parse! scanner, tokens, pattern, type
    return false unless token = scanner.scan(pattern)
    position = scanner.char_pos - token.length
    split(token).inject(position) do |before, s|
      tokens << Greeb::Span.new(before, before + s.length, type)
      before + s.length
    end
  end
end
