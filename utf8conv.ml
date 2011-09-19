open Printf

let get_len pos opt_len s =
  let slen = String.length s in
  if pos < 0 || (pos >= slen && not (pos = 0)) then
    failwith (sprintf
                "Utf8conv: out-of-bounds pos argument \
                 (string length: %i, pos: %i)"
                slen pos);
  let len =
    match opt_len with
        None -> slen - pos
      | Some n ->
          if n < 0 || pos + n > slen then
            failwith (sprintf
                        "Utf8conv: out-of-bounds len argument \
                         (string length: %i, pos: %i, len: %i)"
                        slen pos n)
          else
            n
  in
  len

let add_hex_char buf c =
  bprintf buf "\\x%02X" (Char.code c)

let escape ?(pos = 0) ?len ?(noquotes = false) s =
  let len = get_len pos len s in
  let buf = Buffer.create (2 * len) in
  if not noquotes then
    Buffer.add_char buf '\"';
  for i = pos to pos + len - 1 do
    let c = s.[i] in
    match c with
        '\\' -> Buffer.add_string buf "\\\\"
      | '\"' -> Buffer.add_string buf "\\\""

      | '\x00'..'\x07' -> add_hex_char buf c
      | '\x08' -> Buffer.add_string buf "\\b"
      | '\x09' -> Buffer.add_string buf "\\t"
      | '\x0a' -> Buffer.add_string buf "\\n"
      | '\x0b'..'\x0c' -> add_hex_char buf c
      | '\x0d' -> Buffer.add_string buf "\\r"
      | '\x0e'..'\x1f' -> add_hex_char buf c
      | '\x20'..'\x7e' -> Buffer.add_char buf c
      | '\x7f'..'\xff' -> add_hex_char buf c
  done;
  if not noquotes then
    Buffer.add_char buf '\"';
  Buffer.contents buf

(* Windows-1252 -> Unicode, UTF-8, name *)
let windows1252 = [|
  (*0x00*) Some 0x0000, Some "\x00",
           "NULL";
  (*0x01*) Some 0x0001, Some "\x01",
           "START OF HEADING";
  (*0x02*) Some 0x0002, Some "\x02",
           "START OF TEXT";
  (*0x03*) Some 0x0003, Some "\x03",
           "END OF TEXT";
  (*0x04*) Some 0x0004, Some "\x04",
           "END OF TRANSMISSION";
  (*0x05*) Some 0x0005, Some "\x05",
           "ENQUIRY";
  (*0x06*) Some 0x0006, Some "\x06",
           "ACKNOWLEDGE";
  (*0x07*) Some 0x0007, Some "\x07",
           "BELL";
  (*0x08*) Some 0x0008, Some "\b",
           "BACKSPACE";
  (*0x09*) Some 0x0009, Some "\t",
           "HORIZONTAL TABULATION";
  (*0x0a*) Some 0x000a, Some "\n",
           "LINE FEED";
  (*0x0b*) Some 0x000b, Some "\x0B",
           "VERTICAL TABULATION";
  (*0x0c*) Some 0x000c, Some "\x0C",
           "FORM FEED";
  (*0x0d*) Some 0x000d, Some "\r",
           "CARRIAGE RETURN";
  (*0x0e*) Some 0x000e, Some "\x0E",
           "SHIFT OUT";
  (*0x0f*) Some 0x000f, Some "\x0F",
           "SHIFT IN";
  (*0x10*) Some 0x0010, Some "\x10",
           "DATA LINK ESCAPE";
  (*0x11*) Some 0x0011, Some "\x11",
           "DEVICE CONTROL ONE";
  (*0x12*) Some 0x0012, Some "\x12",
           "DEVICE CONTROL TWO";
  (*0x13*) Some 0x0013, Some "\x13",
           "DEVICE CONTROL THREE";
  (*0x14*) Some 0x0014, Some "\x14",
           "DEVICE CONTROL FOUR";
  (*0x15*) Some 0x0015, Some "\x15",
           "NEGATIVE ACKNOWLEDGE";
  (*0x16*) Some 0x0016, Some "\x16",
           "SYNCHRONOUS IDLE";
  (*0x17*) Some 0x0017, Some "\x17",
           "END OF TRANSMISSION BLOCK";
  (*0x18*) Some 0x0018, Some "\x18",
           "CANCEL";
  (*0x19*) Some 0x0019, Some "\x19",
           "END OF MEDIUM";
  (*0x1a*) Some 0x001a, Some "\x1A",
           "SUBSTITUTE";
  (*0x1b*) Some 0x001b, Some "\x1B",
           "ESCAPE";
  (*0x1c*) Some 0x001c, Some "\x1C",
           "FILE SEPARATOR";
  (*0x1d*) Some 0x001d, Some "\x1D",
           "GROUP SEPARATOR";
  (*0x1e*) Some 0x001e, Some "\x1E",
           "RECORD SEPARATOR";
  (*0x1f*) Some 0x001f, Some "\x1F",
           "UNIT SEPARATOR";
  (*0x20*) Some 0x0020, Some " ",
           "SPACE";
  (*0x21*) Some 0x0021, Some "!",
           "EXCLAMATION MARK";
  (*0x22*) Some 0x0022, Some "\"",
           "QUOTATION MARK";
  (*0x23*) Some 0x0023, Some "#",
           "NUMBER SIGN";
  (*0x24*) Some 0x0024, Some "$",
           "DOLLAR SIGN";
  (*0x25*) Some 0x0025, Some "%",
           "PERCENT SIGN";
  (*0x26*) Some 0x0026, Some "&",
           "AMPERSAND";
  (*0x27*) Some 0x0027, Some "'",
           "APOSTROPHE";
  (*0x28*) Some 0x0028, Some "(",
           "LEFT PARENTHESIS";
  (*0x29*) Some 0x0029, Some ")",
           "RIGHT PARENTHESIS";
  (*0x2a*) Some 0x002a, Some "*",
           "ASTERISK";
  (*0x2b*) Some 0x002b, Some "+",
           "PLUS SIGN";
  (*0x2c*) Some 0x002c, Some ",",
           "COMMA";
  (*0x2d*) Some 0x002d, Some "-",
           "HYPHEN-MINUS";
  (*0x2e*) Some 0x002e, Some ".",
           "FULL STOP";
  (*0x2f*) Some 0x002f, Some "/",
           "SOLIDUS";
  (*0x30*) Some 0x0030, Some "0",
           "DIGIT ZERO";
  (*0x31*) Some 0x0031, Some "1",
           "DIGIT ONE";
  (*0x32*) Some 0x0032, Some "2",
           "DIGIT TWO";
  (*0x33*) Some 0x0033, Some "3",
           "DIGIT THREE";
  (*0x34*) Some 0x0034, Some "4",
           "DIGIT FOUR";
  (*0x35*) Some 0x0035, Some "5",
           "DIGIT FIVE";
  (*0x36*) Some 0x0036, Some "6",
           "DIGIT SIX";
  (*0x37*) Some 0x0037, Some "7",
           "DIGIT SEVEN";
  (*0x38*) Some 0x0038, Some "8",
           "DIGIT EIGHT";
  (*0x39*) Some 0x0039, Some "9",
           "DIGIT NINE";
  (*0x3a*) Some 0x003a, Some ":",
           "COLON";
  (*0x3b*) Some 0x003b, Some ";",
           "SEMICOLON";
  (*0x3c*) Some 0x003c, Some "<",
           "LESS-THAN SIGN";
  (*0x3d*) Some 0x003d, Some "=",
           "EQUALS SIGN";
  (*0x3e*) Some 0x003e, Some ">",
           "GREATER-THAN SIGN";
  (*0x3f*) Some 0x003f, Some "?",
           "QUESTION MARK";
  (*0x40*) Some 0x0040, Some "@",
           "COMMERCIAL AT";
  (*0x41*) Some 0x0041, Some "A",
           "LATIN CAPITAL LETTER A";
  (*0x42*) Some 0x0042, Some "B",
           "LATIN CAPITAL LETTER B";
  (*0x43*) Some 0x0043, Some "C",
           "LATIN CAPITAL LETTER C";
  (*0x44*) Some 0x0044, Some "D",
           "LATIN CAPITAL LETTER D";
  (*0x45*) Some 0x0045, Some "E",
           "LATIN CAPITAL LETTER E";
  (*0x46*) Some 0x0046, Some "F",
           "LATIN CAPITAL LETTER F";
  (*0x47*) Some 0x0047, Some "G",
           "LATIN CAPITAL LETTER G";
  (*0x48*) Some 0x0048, Some "H",
           "LATIN CAPITAL LETTER H";
  (*0x49*) Some 0x0049, Some "I",
           "LATIN CAPITAL LETTER I";
  (*0x4a*) Some 0x004a, Some "J",
           "LATIN CAPITAL LETTER J";
  (*0x4b*) Some 0x004b, Some "K",
           "LATIN CAPITAL LETTER K";
  (*0x4c*) Some 0x004c, Some "L",
           "LATIN CAPITAL LETTER L";
  (*0x4d*) Some 0x004d, Some "M",
           "LATIN CAPITAL LETTER M";
  (*0x4e*) Some 0x004e, Some "N",
           "LATIN CAPITAL LETTER N";
  (*0x4f*) Some 0x004f, Some "O",
           "LATIN CAPITAL LETTER O";
  (*0x50*) Some 0x0050, Some "P",
           "LATIN CAPITAL LETTER P";
  (*0x51*) Some 0x0051, Some "Q",
           "LATIN CAPITAL LETTER Q";
  (*0x52*) Some 0x0052, Some "R",
           "LATIN CAPITAL LETTER R";
  (*0x53*) Some 0x0053, Some "S",
           "LATIN CAPITAL LETTER S";
  (*0x54*) Some 0x0054, Some "T",
           "LATIN CAPITAL LETTER T";
  (*0x55*) Some 0x0055, Some "U",
           "LATIN CAPITAL LETTER U";
  (*0x56*) Some 0x0056, Some "V",
           "LATIN CAPITAL LETTER V";
  (*0x57*) Some 0x0057, Some "W",
           "LATIN CAPITAL LETTER W";
  (*0x58*) Some 0x0058, Some "X",
           "LATIN CAPITAL LETTER X";
  (*0x59*) Some 0x0059, Some "Y",
           "LATIN CAPITAL LETTER Y";
  (*0x5a*) Some 0x005a, Some "Z",
           "LATIN CAPITAL LETTER Z";
  (*0x5b*) Some 0x005b, Some "[",
           "LEFT SQUARE BRACKET";
  (*0x5c*) Some 0x005c, Some "\\",
           "REVERSE SOLIDUS";
  (*0x5d*) Some 0x005d, Some "]",
           "RIGHT SQUARE BRACKET";
  (*0x5e*) Some 0x005e, Some "^",
           "CIRCUMFLEX ACCENT";
  (*0x5f*) Some 0x005f, Some "_",
           "LOW LINE";
  (*0x60*) Some 0x0060, Some "`",
           "GRAVE ACCENT";
  (*0x61*) Some 0x0061, Some "a",
           "LATIN SMALL LETTER A";
  (*0x62*) Some 0x0062, Some "b",
           "LATIN SMALL LETTER B";
  (*0x63*) Some 0x0063, Some "c",
           "LATIN SMALL LETTER C";
  (*0x64*) Some 0x0064, Some "d",
           "LATIN SMALL LETTER D";
  (*0x65*) Some 0x0065, Some "e",
           "LATIN SMALL LETTER E";
  (*0x66*) Some 0x0066, Some "f",
           "LATIN SMALL LETTER F";
  (*0x67*) Some 0x0067, Some "g",
           "LATIN SMALL LETTER G";
  (*0x68*) Some 0x0068, Some "h",
           "LATIN SMALL LETTER H";
  (*0x69*) Some 0x0069, Some "i",
           "LATIN SMALL LETTER I";
  (*0x6a*) Some 0x006a, Some "j",
           "LATIN SMALL LETTER J";
  (*0x6b*) Some 0x006b, Some "k",
           "LATIN SMALL LETTER K";
  (*0x6c*) Some 0x006c, Some "l",
           "LATIN SMALL LETTER L";
  (*0x6d*) Some 0x006d, Some "m",
           "LATIN SMALL LETTER M";
  (*0x6e*) Some 0x006e, Some "n",
           "LATIN SMALL LETTER N";
  (*0x6f*) Some 0x006f, Some "o",
           "LATIN SMALL LETTER O";
  (*0x70*) Some 0x0070, Some "p",
           "LATIN SMALL LETTER P";
  (*0x71*) Some 0x0071, Some "q",
           "LATIN SMALL LETTER Q";
  (*0x72*) Some 0x0072, Some "r",
           "LATIN SMALL LETTER R";
  (*0x73*) Some 0x0073, Some "s",
           "LATIN SMALL LETTER S";
  (*0x74*) Some 0x0074, Some "t",
           "LATIN SMALL LETTER T";
  (*0x75*) Some 0x0075, Some "u",
           "LATIN SMALL LETTER U";
  (*0x76*) Some 0x0076, Some "v",
           "LATIN SMALL LETTER V";
  (*0x77*) Some 0x0077, Some "w",
           "LATIN SMALL LETTER W";
  (*0x78*) Some 0x0078, Some "x",
           "LATIN SMALL LETTER X";
  (*0x79*) Some 0x0079, Some "y",
           "LATIN SMALL LETTER Y";
  (*0x7a*) Some 0x007a, Some "z",
           "LATIN SMALL LETTER Z";
  (*0x7b*) Some 0x007b, Some "{",
           "LEFT CURLY BRACKET";
  (*0x7c*) Some 0x007c, Some "|",
           "VERTICAL LINE";
  (*0x7d*) Some 0x007d, Some "}",
           "RIGHT CURLY BRACKET";
  (*0x7e*) Some 0x007e, Some "~",
           "TILDE";
  (*0x7f*) Some 0x007f, Some "\x7F",
           "DELETE";
  (*0x80*) Some 0x20ac, Some "\xE2\x82\xAC",
           "EURO SIGN";
  (*0x81*) None, None,
           "UNDEFINED";
  (*0x82*) Some 0x201a, Some "\xE2\x80\x9A",
           "SINGLE LOW-9 QUOTATION MARK";
  (*0x83*) Some 0x0192, Some "\xC6\x92",
           "LATIN SMALL LETTER F WITH HOOK";
  (*0x84*) Some 0x201e, Some "\xE2\x80\x9E",
           "DOUBLE LOW-9 QUOTATION MARK";
  (*0x85*) Some 0x2026, Some "\xE2\x80\xA6",
           "HORIZONTAL ELLIPSIS";
  (*0x86*) Some 0x2020, Some "\xE2\x80\xA0",
           "DAGGER";
  (*0x87*) Some 0x2021, Some "\xE2\x80\xA1",
           "DOUBLE DAGGER";
  (*0x88*) Some 0x02c6, Some "\xCB\x86",
           "MODIFIER LETTER CIRCUMFLEX ACCENT";
  (*0x89*) Some 0x2030, Some "\xE2\x80\xB0",
           "PER MILLE SIGN";
  (*0x8a*) Some 0x0160, Some "\xC5\xA0",
           "LATIN CAPITAL LETTER S WITH CARON";
  (*0x8b*) Some 0x2039, Some "\xE2\x80\xB9",
           "SINGLE LEFT-POINTING ANGLE QUOTATION MARK";
  (*0x8c*) Some 0x0152, Some "\xC5\x92",
           "LATIN CAPITAL LIGATURE OE";
  (*0x8d*) None, None,
           "UNDEFINED";
  (*0x8e*) Some 0x017d, Some "\xC5\xBD",
           "LATIN CAPITAL LETTER Z WITH CARON";
  (*0x8f*) None, None,
           "UNDEFINED";
  (*0x90*) None, None,
           "UNDEFINED";
  (*0x91*) Some 0x2018, Some "\xE2\x80\x98",
           "LEFT SINGLE QUOTATION MARK";
  (*0x92*) Some 0x2019, Some "\xE2\x80\x99",
           "RIGHT SINGLE QUOTATION MARK";
  (*0x93*) Some 0x201c, Some "\xE2\x80\x9C",
           "LEFT DOUBLE QUOTATION MARK";
  (*0x94*) Some 0x201d, Some "\xE2\x80\x9D",
           "RIGHT DOUBLE QUOTATION MARK";
  (*0x95*) Some 0x2022, Some "\xE2\x80\xA2",
           "BULLET";
  (*0x96*) Some 0x2013, Some "\xE2\x80\x93",
           "EN DASH";
  (*0x97*) Some 0x2014, Some "\xE2\x80\x94",
           "EM DASH";
  (*0x98*) Some 0x02dc, Some "\xCB\x9C",
           "SMALL TILDE";
  (*0x99*) Some 0x2122, Some "\xE2\x84\xA2",
           "TRADE MARK SIGN";
  (*0x9a*) Some 0x0161, Some "\xC5\xA1",
           "LATIN SMALL LETTER S WITH CARON";
  (*0x9b*) Some 0x203a, Some "\xE2\x80\xBA",
           "SINGLE RIGHT-POINTING ANGLE QUOTATION MARK";
  (*0x9c*) Some 0x0153, Some "\xC5\x93",
           "LATIN SMALL LIGATURE OE";
  (*0x9d*) None, None,
           "UNDEFINED";
  (*0x9e*) Some 0x017e, Some "\xC5\xBE",
           "LATIN SMALL LETTER Z WITH CARON";
  (*0x9f*) Some 0x0178, Some "\xC5\xB8",
           "LATIN CAPITAL LETTER Y WITH DIAERESIS";
  (*0xa0*) Some 0x00a0, Some "\xC2\xA0",
           "NO-BREAK SPACE";
  (*0xa1*) Some 0x00a1, Some "\xC2\xA1",
           "INVERTED EXCLAMATION MARK";
  (*0xa2*) Some 0x00a2, Some "\xC2\xA2",
           "CENT SIGN";
  (*0xa3*) Some 0x00a3, Some "\xC2\xA3",
           "POUND SIGN";
  (*0xa4*) Some 0x00a4, Some "\xC2\xA4",
           "CURRENCY SIGN";
  (*0xa5*) Some 0x00a5, Some "\xC2\xA5",
           "YEN SIGN";
  (*0xa6*) Some 0x00a6, Some "\xC2\xA6",
           "BROKEN BAR";
  (*0xa7*) Some 0x00a7, Some "\xC2\xA7",
           "SECTION SIGN";
  (*0xa8*) Some 0x00a8, Some "\xC2\xA8",
           "DIAERESIS";
  (*0xa9*) Some 0x00a9, Some "\xC2\xA9",
           "COPYRIGHT SIGN";
  (*0xaa*) Some 0x00aa, Some "\xC2\xAA",
           "FEMININE ORDINAL INDICATOR";
  (*0xab*) Some 0x00ab, Some "\xC2\xAB",
           "LEFT-POINTING DOUBLE ANGLE QUOTATION MARK";
  (*0xac*) Some 0x00ac, Some "\xC2\xAC",
           "NOT SIGN";
  (*0xad*) Some 0x00ad, Some "\xC2\xAD",
           "SOFT HYPHEN";
  (*0xae*) Some 0x00ae, Some "\xC2\xAE",
           "REGISTERED SIGN";
  (*0xaf*) Some 0x00af, Some "\xC2\xAF",
           "MACRON";
  (*0xb0*) Some 0x00b0, Some "\xC2\xB0",
           "DEGREE SIGN";
  (*0xb1*) Some 0x00b1, Some "\xC2\xB1",
           "PLUS-MINUS SIGN";
  (*0xb2*) Some 0x00b2, Some "\xC2\xB2",
           "SUPERSCRIPT TWO";
  (*0xb3*) Some 0x00b3, Some "\xC2\xB3",
           "SUPERSCRIPT THREE";
  (*0xb4*) Some 0x00b4, Some "\xC2\xB4",
           "ACUTE ACCENT";
  (*0xb5*) Some 0x00b5, Some "\xC2\xB5",
           "MICRO SIGN";
  (*0xb6*) Some 0x00b6, Some "\xC2\xB6",
           "PILCROW SIGN";
  (*0xb7*) Some 0x00b7, Some "\xC2\xB7",
           "MIDDLE DOT";
  (*0xb8*) Some 0x00b8, Some "\xC2\xB8",
           "CEDILLA";
  (*0xb9*) Some 0x00b9, Some "\xC2\xB9",
           "SUPERSCRIPT ONE";
  (*0xba*) Some 0x00ba, Some "\xC2\xBA",
           "MASCULINE ORDINAL INDICATOR";
  (*0xbb*) Some 0x00bb, Some "\xC2\xBB",
           "RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK";
  (*0xbc*) Some 0x00bc, Some "\xC2\xBC",
           "VULGAR FRACTION ONE QUARTER";
  (*0xbd*) Some 0x00bd, Some "\xC2\xBD",
           "VULGAR FRACTION ONE HALF";
  (*0xbe*) Some 0x00be, Some "\xC2\xBE",
           "VULGAR FRACTION THREE QUARTERS";
  (*0xbf*) Some 0x00bf, Some "\xC2\xBF",
           "INVERTED QUESTION MARK";
  (*0xc0*) Some 0x00c0, Some "\xC3\x80",
           "LATIN CAPITAL LETTER A WITH GRAVE";
  (*0xc1*) Some 0x00c1, Some "\xC3\x81",
           "LATIN CAPITAL LETTER A WITH ACUTE";
  (*0xc2*) Some 0x00c2, Some "\xC3\x82",
           "LATIN CAPITAL LETTER A WITH CIRCUMFLEX";
  (*0xc3*) Some 0x00c3, Some "\xC3\x83",
           "LATIN CAPITAL LETTER A WITH TILDE";
  (*0xc4*) Some 0x00c4, Some "\xC3\x84",
           "LATIN CAPITAL LETTER A WITH DIAERESIS";
  (*0xc5*) Some 0x00c5, Some "\xC3\x85",
           "LATIN CAPITAL LETTER A WITH RING ABOVE";
  (*0xc6*) Some 0x00c6, Some "\xC3\x86",
           "LATIN CAPITAL LETTER AE";
  (*0xc7*) Some 0x00c7, Some "\xC3\x87",
           "LATIN CAPITAL LETTER C WITH CEDILLA";
  (*0xc8*) Some 0x00c8, Some "\xC3\x88",
           "LATIN CAPITAL LETTER E WITH GRAVE";
  (*0xc9*) Some 0x00c9, Some "\xC3\x89",
           "LATIN CAPITAL LETTER E WITH ACUTE";
  (*0xca*) Some 0x00ca, Some "\xC3\x8A",
           "LATIN CAPITAL LETTER E WITH CIRCUMFLEX";
  (*0xcb*) Some 0x00cb, Some "\xC3\x8B",
           "LATIN CAPITAL LETTER E WITH DIAERESIS";
  (*0xcc*) Some 0x00cc, Some "\xC3\x8C",
           "LATIN CAPITAL LETTER I WITH GRAVE";
  (*0xcd*) Some 0x00cd, Some "\xC3\x8D",
           "LATIN CAPITAL LETTER I WITH ACUTE";
  (*0xce*) Some 0x00ce, Some "\xC3\x8E",
           "LATIN CAPITAL LETTER I WITH CIRCUMFLEX";
  (*0xcf*) Some 0x00cf, Some "\xC3\x8F",
           "LATIN CAPITAL LETTER I WITH DIAERESIS";
  (*0xd0*) Some 0x00d0, Some "\xC3\x90",
           "LATIN CAPITAL LETTER ETH";
  (*0xd1*) Some 0x00d1, Some "\xC3\x91",
           "LATIN CAPITAL LETTER N WITH TILDE";
  (*0xd2*) Some 0x00d2, Some "\xC3\x92",
           "LATIN CAPITAL LETTER O WITH GRAVE";
  (*0xd3*) Some 0x00d3, Some "\xC3\x93",
           "LATIN CAPITAL LETTER O WITH ACUTE";
  (*0xd4*) Some 0x00d4, Some "\xC3\x94",
           "LATIN CAPITAL LETTER O WITH CIRCUMFLEX";
  (*0xd5*) Some 0x00d5, Some "\xC3\x95",
           "LATIN CAPITAL LETTER O WITH TILDE";
  (*0xd6*) Some 0x00d6, Some "\xC3\x96",
           "LATIN CAPITAL LETTER O WITH DIAERESIS";
  (*0xd7*) Some 0x00d7, Some "\xC3\x97",
           "MULTIPLICATION SIGN";
  (*0xd8*) Some 0x00d8, Some "\xC3\x98",
           "LATIN CAPITAL LETTER O WITH STROKE";
  (*0xd9*) Some 0x00d9, Some "\xC3\x99",
           "LATIN CAPITAL LETTER U WITH GRAVE";
  (*0xda*) Some 0x00da, Some "\xC3\x9A",
           "LATIN CAPITAL LETTER U WITH ACUTE";
  (*0xdb*) Some 0x00db, Some "\xC3\x9B",
           "LATIN CAPITAL LETTER U WITH CIRCUMFLEX";
  (*0xdc*) Some 0x00dc, Some "\xC3\x9C",
           "LATIN CAPITAL LETTER U WITH DIAERESIS";
  (*0xdd*) Some 0x00dd, Some "\xC3\x9D",
           "LATIN CAPITAL LETTER Y WITH ACUTE";
  (*0xde*) Some 0x00de, Some "\xC3\x9E",
           "LATIN CAPITAL LETTER THORN";
  (*0xdf*) Some 0x00df, Some "\xC3\x9F",
           "LATIN SMALL LETTER SHARP S";
  (*0xe0*) Some 0x00e0, Some "\xC3\xA0",
           "LATIN SMALL LETTER A WITH GRAVE";
  (*0xe1*) Some 0x00e1, Some "\xC3\xA1",
           "LATIN SMALL LETTER A WITH ACUTE";
  (*0xe2*) Some 0x00e2, Some "\xC3\xA2",
           "LATIN SMALL LETTER A WITH CIRCUMFLEX";
  (*0xe3*) Some 0x00e3, Some "\xC3\xA3",
           "LATIN SMALL LETTER A WITH TILDE";
  (*0xe4*) Some 0x00e4, Some "\xC3\xA4",
           "LATIN SMALL LETTER A WITH DIAERESIS";
  (*0xe5*) Some 0x00e5, Some "\xC3\xA5",
           "LATIN SMALL LETTER A WITH RING ABOVE";
  (*0xe6*) Some 0x00e6, Some "\xC3\xA6",
           "LATIN SMALL LETTER AE";
  (*0xe7*) Some 0x00e7, Some "\xC3\xA7",
           "LATIN SMALL LETTER C WITH CEDILLA";
  (*0xe8*) Some 0x00e8, Some "\xC3\xA8",
           "LATIN SMALL LETTER E WITH GRAVE";
  (*0xe9*) Some 0x00e9, Some "\xC3\xA9",
           "LATIN SMALL LETTER E WITH ACUTE";
  (*0xea*) Some 0x00ea, Some "\xC3\xAA",
           "LATIN SMALL LETTER E WITH CIRCUMFLEX";
  (*0xeb*) Some 0x00eb, Some "\xC3\xAB",
           "LATIN SMALL LETTER E WITH DIAERESIS";
  (*0xec*) Some 0x00ec, Some "\xC3\xAC",
           "LATIN SMALL LETTER I WITH GRAVE";
  (*0xed*) Some 0x00ed, Some "\xC3\xAD",
           "LATIN SMALL LETTER I WITH ACUTE";
  (*0xee*) Some 0x00ee, Some "\xC3\xAE",
           "LATIN SMALL LETTER I WITH CIRCUMFLEX";
  (*0xef*) Some 0x00ef, Some "\xC3\xAF",
           "LATIN SMALL LETTER I WITH DIAERESIS";
  (*0xf0*) Some 0x00f0, Some "\xC3\xB0",
           "LATIN SMALL LETTER ETH";
  (*0xf1*) Some 0x00f1, Some "\xC3\xB1",
           "LATIN SMALL LETTER N WITH TILDE";
  (*0xf2*) Some 0x00f2, Some "\xC3\xB2",
           "LATIN SMALL LETTER O WITH GRAVE";
  (*0xf3*) Some 0x00f3, Some "\xC3\xB3",
           "LATIN SMALL LETTER O WITH ACUTE";
  (*0xf4*) Some 0x00f4, Some "\xC3\xB4",
           "LATIN SMALL LETTER O WITH CIRCUMFLEX";
  (*0xf5*) Some 0x00f5, Some "\xC3\xB5",
           "LATIN SMALL LETTER O WITH TILDE";
  (*0xf6*) Some 0x00f6, Some "\xC3\xB6",
           "LATIN SMALL LETTER O WITH DIAERESIS";
  (*0xf7*) Some 0x00f7, Some "\xC3\xB7",
           "DIVISION SIGN";
  (*0xf8*) Some 0x00f8, Some "\xC3\xB8",
           "LATIN SMALL LETTER O WITH STROKE";
  (*0xf9*) Some 0x00f9, Some "\xC3\xB9",
           "LATIN SMALL LETTER U WITH GRAVE";
  (*0xfa*) Some 0x00fa, Some "\xC3\xBA",
           "LATIN SMALL LETTER U WITH ACUTE";
  (*0xfb*) Some 0x00fb, Some "\xC3\xBB",
           "LATIN SMALL LETTER U WITH CIRCUMFLEX";
  (*0xfc*) Some 0x00fc, Some "\xC3\xBC",
           "LATIN SMALL LETTER U WITH DIAERESIS";
  (*0xfd*) Some 0x00fd, Some "\xC3\xBD",
           "LATIN SMALL LETTER Y WITH ACUTE";
  (*0xfe*) Some 0x00fe, Some "\xC3\xBE",
           "LATIN SMALL LETTER THORN";
  (*0xff*) Some 0x00ff, Some "\xC3\xBF",
           "LATIN SMALL LETTER Y WITH DIAERESIS";
|]

let is_ascii ?(pos = 0) ?len s =
  let len = get_len pos len s in
  try
    for i = pos to pos + len - 1 do
      if Char.code s.[i] >= 0x80 then
        raise Exit
    done;
    true
  with Exit ->
    false

let is_iso88591 ?(pos = 0) ?len s =
  let len = get_len pos len s in
  try
    for i = pos to pos + len - 1 do
      match s.[i] with
          '\x80'..'\x9f' -> raise Exit
        | _ -> ()
    done;
    true
  with Exit ->
    false

let is_windows1252 ?(pos = 0) ?len s =
  let len = get_len pos len s in
  try
    for i = pos to pos + len - 1 do
      match s.[i] with
          '\x81' | '\x8d' | '\x8f' | '\x90' | '\x9d' -> raise Exit
        | _ -> ()
    done;
    true
  with Exit ->
    false

let utf8_of_windows1252
    ?(pos = 0)
    ?len
    ?(undefined =
        fun c ->
          failwith
            (sprintf "Utf8conv.utf8_of_windows1252: \
                      undefined character code 0x%02x" (Char.code c))
     )
    s =

  let len = get_len pos len s in
  let buf = Buffer.create (2 * len) in
  let last_flush = ref (pos - 1) in
  let flush_ascii i =
    let start = !last_flush + 1 in
    let flush_len = i - start in
    if flush_len > 0 then
      Buffer.add_substring buf s start flush_len;
    last_flush := i
  in
  for i = pos to pos + len - 1 do
    let c = s.[i] in
    if Char.code c >= 0x80 then (
      flush_ascii i;
      let x =
        match windows1252.(Char.code c) with
          _, Some x, _ -> x
        | _, None, _ -> undefined c
      in
      Buffer.add_string buf x
    )
  done;
  flush_ascii (pos + len);
  Buffer.contents buf
