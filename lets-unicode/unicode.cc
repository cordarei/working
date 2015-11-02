//a minimal program -- this main function is just for compilation purposes for now
int main() {
  return 0;
}



/*
 * The Unicode Standard
 * Version 8.0
 *
 * ``The Unicode Standard specifies a numeric value (code point) and a name for each of its characters.
 *   ...
 *   In addition to character codes and names, other information is crucial to ensure legible text: a
 *   character's case, directionality, and alphabetic properties must be well defined. The Unicode
 *   Standard defines these and other semantic values, and it includes application data such as case
 *   mapping tables and character property tables as part of the Unicode
 *   Character Database.`` §1 p1
 *
 * ``The difference between identifying a character and rendering it on screen or paper is crucial to
 *   understanding the Unicode Standard's role in text processing. The character identified by a
 *   Unicode code point is an abstract entity, such as ``LATIN CAPITAL LETTER A`` or ``BENGALI DIGIT
 *   FIVE``. The mark made on screen or paper, called a glyph, is a visual
 *   representation of the character.
 *
 *   The Unicode Standard does not define glyph images. That is, the standard
 *   defines how characters are interpreted, not how glyphs are rendered. ...
 *   The Unicode Standard does not specifiy the precise shape, size, or
 *   orientation of on-screen characters.`` §1.3 p6
 *
 * ``The Unicode Standard does not define what is and is not a text element in
 *   different processes; instead, it defines elements called /encoded
 *   characters/. An encoded character is represented by a number from 0 to
 *   10FFFF_16, called a code point. A text element, in turn, is represented by
 *   a sequence of one or more encoded characters.`` §1.3 p7
 *
 *  The 10 Unicode Design Principles
 * ``- Universality
 *   - Efficiency
 *   - Characters, not glyphs
 *   - Semantics
 *   - Plain text
 *   - Logical order
 *   - Unification
 *   - Dynamic composition
 *   - Stability
 *   - Convertibility`` §2.2 p14 Table 2-1
 *
 */

/*
 * ``On a computer, abstract characters are encoded internally as numbers. To
 * create a complete character encoding, it is necessary to define the list of
 * all characters to be encoded and to establish systematic rules for how the
 * numbers represent the characters.
 *
 * The range of integers used to code the abstract characters is called the
 * /codespace/. A particular integer in this set is called a /code point/. When
 * an abstract character is mapped or /assigned/ to a particular code point in
 * the code space, it is then referred to as an /encoded character/.``
 */



// Unicode Encoding Forms

// Example: A Ω 語 𐎄
//UTF-32
auto fig211_32 = {{0x00000041},{0x000003A9},{0x00008A9E},{0x00010384}};
//UTF-16
auto fig211_16 = {{0x0041},{0x03A9},{0x8A9E},{0xD800},{0xDF84}};
//UTF-8
auto fig211_8 = {{0x41},{0xCE},{0xA9},{0xE8},{0xAA},{0x9E},{0xF0},{0x90},{0x8E},{0x84}};
