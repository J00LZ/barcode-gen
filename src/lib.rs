#![warn(missing_docs)]
//! A simple Code 128 barcode generator
//!
//! Currently supports all features of Code 128.
//!
//! Generating barcodes with [`FNC1`](BarcodeValue::FNC1), [`FNC2`](BarcodeValue::FNC2) and [`FNC3`](BarcodeValue::FNC3)
//! is not possible however, since these are control characters that can't appear in a normal string.
//! [`FNC4`](BarcodeValue::FNC4) does work however and is switched to when needed automatically.

use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    sync::atomic::AtomicUsize,
};

/// The different barcode types, used to keep track of the type used in the encoder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BarcodeType {
    /// Code 128 variant A, uppper case characters and control characters
    CodeA,
    /// Code 128 variant B, upper and lower case characters
    CodeB,
    /// Code 128 variant C, two digit numbers
    CodeC,
}

impl BarcodeType {
    fn other_set(&self) -> Option<Self> {
        match self {
            BarcodeType::CodeA => Some(BarcodeType::CodeB),
            BarcodeType::CodeB => Some(BarcodeType::CodeA),
            // c is always c
            BarcodeType::CodeC => None,
        }
    }
}

/// A value in a barcode, consists of regular characters,
/// digits for Code C, and control codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BarcodeValue {
    /// A regular character, also includes control characters.
    RegularCharacter(char),
    /// A Code C digit, the numbers 0 through 9 are actually 00 through 09.
    Digit(u8),
    /// Used to indicate a GS1-128 barcode
    FNC1,
    /// Indicates that the currently scanned string should be prepended to the next scan
    FNC2,
    /// Initialize, used for programming scanners
    FNC3,
    /// Used for extended ASCII support.
    FNC4,
    /// The code used to signal we start in variant A
    StartA,
    /// The code used to signal we start in variant B
    StartB,
    /// The code used to signal we start in variant C
    StartC,
    /// Not actually the stop code, just the one used at the end.
    Stop,
    /// Use variant A for the next character
    ShiftA,
    /// Use variant B for the next character
    ShiftB,
    /// Switch to variant A from here on out
    CodeA,
    /// Switch to variant B from here on out
    CodeB,
    /// Switch to variant C from here on out
    CodeC,
}

impl From<char> for BarcodeValue {
    fn from(value: char) -> Self {
        BarcodeValue::RegularCharacter(value)
    }
}

impl From<u8> for BarcodeValue {
    fn from(value: u8) -> Self {
        BarcodeValue::Digit(value)
    }
}

/// An entry in the encoding table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TableEntry {
    value: u8,
    a: BarcodeValue,
    b: BarcodeValue,
    c: BarcodeValue,
    latin: char,
}

impl TableEntry {
    /// create a new table entry
    pub fn new(
        value: u8,
        a: impl Into<BarcodeValue>,
        b: impl Into<BarcodeValue>,
        c: impl Into<BarcodeValue>,
        latin: char,
    ) -> Self {
        Self::new_barcode_value(value, a.into(), b.into(), c.into(), latin)
    }

    const fn new_barcode_value(
        value: u8,
        a: BarcodeValue,
        b: BarcodeValue,
        c: BarcodeValue,
        latin: char,
    ) -> Self {
        TableEntry {
            value,
            a,
            b,
            c,
            latin,
        }
    }

    const fn new_barcode_chars(value: u8, a: char, b: char, c: u8, latin: char) -> Self {
        Self::new_barcode_value(
            value,
            BarcodeValue::RegularCharacter(a),
            BarcodeValue::RegularCharacter(b),
            BarcodeValue::Digit(c),
            latin,
        )
    }

    /// the value of the entry, used for calculating the check digit
    pub fn value(&self) -> u8 {
        self.value
    }

    /// the most common character used to represent this value
    pub fn latin(&self) -> char {
        self.latin
    }

    /// the bits used to encode the value on a normal barcode
    pub fn barcode_bits(&self) -> u32 {
        BARCODE_BITS[self.value() as usize].0
    }

    /// the runs for the code
    ///
    /// A run of 1231 would mean 1001110 in binary
    pub fn barcode_runs(&self) -> &'static str {
        BARCODE_BITS[self.value() as usize].1
    }
}

const BARCODE_BITS: [(u32, &str); 109] = [
    (0b11011001100, "212222"),
    (0b11001101100, "222122"),
    (0b11001100110, "222221"),
    (0b10010011000, "121223"),
    (0b10010001100, "121322"),
    (0b10001001100, "131222"),
    (0b10011001000, "122213"),
    (0b10011000100, "122312"),
    (0b10001100100, "132212"),
    (0b11001001000, "221213"),
    (0b11001000100, "221312"),
    (0b11000100100, "231212"),
    (0b10110011100, "112232"),
    (0b10011011100, "122132"),
    (0b10011001110, "122231"),
    (0b10111001100, "113222"),
    (0b10011101100, "123122"),
    (0b10011100110, "123221"),
    (0b11001110010, "223211"),
    (0b11001011100, "221132"),
    (0b11001001110, "221231"),
    (0b11011100100, "213212"),
    (0b11001110100, "223112"),
    (0b11101101110, "312131"),
    (0b11101001100, "311222"),
    (0b11100101100, "321122"),
    (0b11100100110, "321221"),
    (0b11101100100, "312212"),
    (0b11100110100, "322112"),
    (0b11100110010, "322211"),
    (0b11011011000, "212123"),
    (0b11011000110, "212321"),
    (0b11000110110, "232121"),
    (0b10100011000, "111323"),
    (0b10001011000, "131123"),
    (0b10001000110, "131321"),
    (0b10110001000, "112313"),
    (0b10001101000, "132113"),
    (0b10001100010, "132311"),
    (0b11010001000, "211313"),
    (0b11000101000, "231113"),
    (0b11000100010, "231311"),
    (0b10110111000, "112133"),
    (0b10110001110, "112331"),
    (0b10001101110, "132131"),
    (0b10111011000, "113123"),
    (0b10111000110, "113321"),
    (0b10001110110, "133121"),
    (0b11101110110, "313121"),
    (0b11010001110, "211331"),
    (0b11000101110, "231131"),
    (0b11011101000, "213113"),
    (0b11011100010, "213311"),
    (0b11011101110, "213131"),
    (0b11101011000, "311123"),
    (0b11101000110, "311321"),
    (0b11100010110, "331121"),
    (0b11101101000, "312113"),
    (0b11101100010, "312311"),
    (0b11100011010, "332111"),
    (0b11101111010, "314111"),
    (0b11001000010, "221411"),
    (0b11110001010, "431111"),
    (0b10100110000, "111224"),
    (0b10100001100, "111422"),
    (0b10010110000, "121124"),
    (0b10010000110, "121421"),
    (0b10000101100, "141122"),
    (0b10000100110, "141221"),
    (0b10110010000, "112214"),
    (0b10110000100, "112412"),
    (0b10011010000, "122114"),
    (0b10011000010, "122411"),
    (0b10000110100, "142112"),
    (0b10000110010, "142211"),
    (0b11000010010, "241211"),
    (0b11001010000, "221114"),
    (0b11110111010, "413111"),
    (0b11000010100, "241112"),
    (0b10001111010, "134111"),
    (0b10100111100, "111242"),
    (0b10010111100, "121142"),
    (0b10010011110, "121241"),
    (0b10111100100, "114212"),
    (0b10011110100, "124112"),
    (0b10011110010, "124211"),
    (0b11110100100, "411212"),
    (0b11110010100, "421112"),
    (0b11110010010, "421211"),
    (0b11011011110, "212141"),
    (0b11011110110, "214121"),
    (0b11110110110, "412121"),
    (0b10101111000, "111143"),
    (0b10100011110, "111341"),
    (0b10001011110, "131141"),
    (0b10111101000, "114113"),
    (0b10111100010, "114311"),
    (0b11110101000, "411113"),
    (0b11110100010, "411311"),
    (0b10111011110, "113141"),
    (0b10111101110, "114131"),
    (0b11101011110, "311141"),
    (0b11110101110, "411131"),
    (0b11010000100, "211412"),
    (0b11010010000, "211214"),
    (0b11010011100, "211232"),
    (0b11000111010, "233111"),
    (0b11010111000, "211133"),
    (0b1100011101011, "2331112"),
];

/// an encoding table, used to encode barcodes.
#[derive(Debug, Clone)]
pub struct Table {
    entries: [TableEntry; 107],
}

impl Table {
    /// create a new table
    pub const fn new() -> Self {
        Table {
            entries: [
                TableEntry::new_barcode_chars(0, ' ', ' ', 0, ' '),
                TableEntry::new_barcode_chars(1, '!', '!', 1, '!'),
                TableEntry::new_barcode_chars(2, '"', '"', 2, '"'),
                TableEntry::new_barcode_chars(3, '#', '#', 3, '#'),
                TableEntry::new_barcode_chars(4, '$', '$', 4, '$'),
                TableEntry::new_barcode_chars(5, '%', '%', 5, '%'),
                TableEntry::new_barcode_chars(6, '&', '&', 6, '&'),
                TableEntry::new_barcode_chars(7, '\'', '\'', 7, '\''),
                TableEntry::new_barcode_chars(8, '(', '(', 8, '('),
                TableEntry::new_barcode_chars(9, ')', ')', 9, ')'),
                TableEntry::new_barcode_chars(10, '*', '*', 10, '*'),
                TableEntry::new_barcode_chars(11, '+', '+', 11, '+'),
                TableEntry::new_barcode_chars(12, ',', ',', 12, ','),
                TableEntry::new_barcode_chars(13, '-', '-', 13, '-'),
                TableEntry::new_barcode_chars(14, '.', '.', 14, '.'),
                TableEntry::new_barcode_chars(15, '/', '/', 15, '/'),
                TableEntry::new_barcode_chars(16, '0', '0', 16, '0'),
                TableEntry::new_barcode_chars(17, '1', '1', 17, '1'),
                TableEntry::new_barcode_chars(18, '2', '2', 18, '2'),
                TableEntry::new_barcode_chars(19, '3', '3', 19, '3'),
                TableEntry::new_barcode_chars(20, '4', '4', 20, '4'),
                TableEntry::new_barcode_chars(21, '5', '5', 21, '5'),
                TableEntry::new_barcode_chars(22, '6', '6', 22, '6'),
                TableEntry::new_barcode_chars(23, '7', '7', 23, '7'),
                TableEntry::new_barcode_chars(24, '8', '8', 24, '8'),
                TableEntry::new_barcode_chars(25, '9', '9', 25, '9'),
                TableEntry::new_barcode_chars(26, ':', ':', 26, ':'),
                TableEntry::new_barcode_chars(27, ';', ';', 27, ';'),
                TableEntry::new_barcode_chars(28, '<', '<', 28, '<'),
                TableEntry::new_barcode_chars(29, '=', '=', 29, '='),
                TableEntry::new_barcode_chars(30, '>', '>', 30, '>'),
                TableEntry::new_barcode_chars(31, '?', '?', 31, '?'),
                TableEntry::new_barcode_chars(32, '@', '@', 32, '@'),
                TableEntry::new_barcode_chars(33, 'A', 'A', 33, 'A'),
                TableEntry::new_barcode_chars(34, 'B', 'B', 34, 'B'),
                TableEntry::new_barcode_chars(35, 'C', 'C', 35, 'C'),
                TableEntry::new_barcode_chars(36, 'D', 'D', 36, 'D'),
                TableEntry::new_barcode_chars(37, 'E', 'E', 37, 'E'),
                TableEntry::new_barcode_chars(38, 'F', 'F', 38, 'F'),
                TableEntry::new_barcode_chars(39, 'G', 'G', 39, 'G'),
                TableEntry::new_barcode_chars(40, 'H', 'H', 40, 'H'),
                TableEntry::new_barcode_chars(41, 'I', 'I', 41, 'I'),
                TableEntry::new_barcode_chars(42, 'J', 'J', 42, 'J'),
                TableEntry::new_barcode_chars(43, 'K', 'K', 43, 'K'),
                TableEntry::new_barcode_chars(44, 'L', 'L', 44, 'L'),
                TableEntry::new_barcode_chars(45, 'M', 'M', 45, 'M'),
                TableEntry::new_barcode_chars(46, 'N', 'N', 46, 'N'),
                TableEntry::new_barcode_chars(47, 'O', 'O', 47, 'O'),
                TableEntry::new_barcode_chars(48, 'P', 'P', 48, 'P'),
                TableEntry::new_barcode_chars(49, 'Q', 'Q', 49, 'Q'),
                TableEntry::new_barcode_chars(50, 'R', 'R', 50, 'R'),
                TableEntry::new_barcode_chars(51, 'S', 'S', 51, 'S'),
                TableEntry::new_barcode_chars(52, 'T', 'T', 52, 'T'),
                TableEntry::new_barcode_chars(53, 'U', 'U', 53, 'U'),
                TableEntry::new_barcode_chars(54, 'V', 'V', 54, 'V'),
                TableEntry::new_barcode_chars(55, 'W', 'W', 55, 'W'),
                TableEntry::new_barcode_chars(56, 'X', 'X', 56, 'X'),
                TableEntry::new_barcode_chars(57, 'Y', 'Y', 57, 'Y'),
                TableEntry::new_barcode_chars(58, 'Z', 'Z', 58, 'Z'),
                TableEntry::new_barcode_chars(59, '[', '[', 59, '['),
                TableEntry::new_barcode_chars(60, '\\', '\\', 60, '\\'),
                TableEntry::new_barcode_chars(61, ']', ']', 61, ']'),
                TableEntry::new_barcode_chars(62, '^', '^', 62, '^'),
                TableEntry::new_barcode_chars(63, '_', '_', 63, '_'),
                TableEntry::new_barcode_chars(64, '\x00', '`', 64, '`'),
                TableEntry::new_barcode_chars(65, '\x01', 'a', 65, 'a'),
                TableEntry::new_barcode_chars(66, '\x02', 'b', 66, 'b'),
                TableEntry::new_barcode_chars(67, '\x03', 'c', 67, 'c'),
                TableEntry::new_barcode_chars(68, '\x04', 'd', 68, 'd'),
                TableEntry::new_barcode_chars(69, '\x05', 'e', 69, 'e'),
                TableEntry::new_barcode_chars(70, '\x06', 'f', 70, 'f'),
                TableEntry::new_barcode_chars(71, '\x07', 'g', 71, 'g'),
                TableEntry::new_barcode_chars(72, '\x08', 'h', 72, 'h'),
                TableEntry::new_barcode_chars(73, '\x09', 'i', 73, 'i'),
                TableEntry::new_barcode_chars(74, '\x0A', 'j', 74, 'j'),
                TableEntry::new_barcode_chars(75, '\x0B', 'k', 75, 'k'),
                TableEntry::new_barcode_chars(76, '\x0C', 'l', 76, 'l'),
                TableEntry::new_barcode_chars(77, '\x0D', 'm', 77, 'm'),
                TableEntry::new_barcode_chars(78, '\x0E', 'n', 78, 'n'),
                TableEntry::new_barcode_chars(79, '\x0F', 'o', 79, 'o'),
                TableEntry::new_barcode_chars(80, '\x10', 'p', 80, 'p'),
                TableEntry::new_barcode_chars(81, '\x11', 'q', 81, 'q'),
                TableEntry::new_barcode_chars(82, '\x12', 'r', 82, 'r'),
                TableEntry::new_barcode_chars(83, '\x13', 's', 83, 's'),
                TableEntry::new_barcode_chars(84, '\x14', 't', 84, 't'),
                TableEntry::new_barcode_chars(85, '\x15', 'u', 85, 'u'),
                TableEntry::new_barcode_chars(86, '\x16', 'v', 86, 'v'),
                TableEntry::new_barcode_chars(87, '\x17', 'w', 87, 'w'),
                TableEntry::new_barcode_chars(88, '\x18', 'x', 88, 'x'),
                TableEntry::new_barcode_chars(89, '\x19', 'y', 89, 'y'),
                TableEntry::new_barcode_chars(90, '\x1A', 'z', 90, 'z'),
                TableEntry::new_barcode_chars(91, '\x1B', '{', 91, '{'),
                TableEntry::new_barcode_chars(92, '\x1C', '|', 92, '|'),
                TableEntry::new_barcode_chars(93, '\x1D', '}', 93, '}'),
                TableEntry::new_barcode_chars(94, '\x1E', '~', 94, '~'),
                TableEntry::new_barcode_chars(95, '\x1F', '\x7F', 95, 'Ã'),
                TableEntry::new_barcode_value(
                    96,
                    BarcodeValue::FNC3,
                    BarcodeValue::FNC3,
                    BarcodeValue::Digit(96),
                    'Ä',
                ),
                TableEntry::new_barcode_value(
                    97,
                    BarcodeValue::FNC2,
                    BarcodeValue::FNC2,
                    BarcodeValue::Digit(97),
                    'Å',
                ),
                TableEntry::new_barcode_value(
                    98,
                    BarcodeValue::ShiftB,
                    BarcodeValue::ShiftA,
                    BarcodeValue::Digit(98),
                    'Æ',
                ),
                TableEntry::new_barcode_value(
                    99,
                    BarcodeValue::CodeC,
                    BarcodeValue::CodeC,
                    BarcodeValue::Digit(99),
                    'Ç',
                ),
                TableEntry::new_barcode_value(
                    100,
                    BarcodeValue::CodeB,
                    BarcodeValue::FNC4,
                    BarcodeValue::CodeB,
                    'È',
                ),
                TableEntry::new_barcode_value(
                    101,
                    BarcodeValue::FNC4,
                    BarcodeValue::CodeA,
                    BarcodeValue::CodeA,
                    'É',
                ),
                TableEntry::new_barcode_value(
                    102,
                    BarcodeValue::FNC1,
                    BarcodeValue::FNC1,
                    BarcodeValue::FNC1,
                    'Ê',
                ),
                TableEntry::new_barcode_value(
                    103,
                    BarcodeValue::StartA,
                    BarcodeValue::StartA,
                    BarcodeValue::StartA,
                    'Ë',
                ),
                TableEntry::new_barcode_value(
                    104,
                    BarcodeValue::StartB,
                    BarcodeValue::StartB,
                    BarcodeValue::StartB,
                    'Ì',
                ),
                TableEntry::new_barcode_value(
                    105,
                    BarcodeValue::StartC,
                    BarcodeValue::StartC,
                    BarcodeValue::StartC,
                    'Í',
                ),
                TableEntry::new_barcode_value(
                    108,
                    BarcodeValue::Stop,
                    BarcodeValue::Stop,
                    BarcodeValue::Stop,
                    'Î',
                ),
            ],
        }
    }

    /// in case you want to make your own (probably incompatible) table
    ///
    /// Use the [`Self::modify`] function to change values in the current table,
    /// like changing what latin character matches a value
    pub const fn new_from(entries: [TableEntry; 107]) -> Self {
        Table { entries }
    }

    /// Modify a table, allowing you to set custom values for all fields of a table entry.
    ///
    /// (tip: just make a new table entry and assign it to the parameter)
    pub fn modify(&self, mut f: impl FnMut(&mut TableEntry)) -> Self {
        let mut new_entries = self.entries;
        for entry in &mut new_entries {
            f(entry);
        }
        Table::new_from(new_entries)
    }

    /// given a requested value and a specific code set, find the value in the table.
    ///
    /// It'll return None if the value does not exist in the table, for example when you
    /// want to use a lower case character but are in Code A.
    pub fn entry_in_set(
        &self,
        value: impl Into<BarcodeValue>,
        set: BarcodeType,
    ) -> Option<&TableEntry> {
        let value = value.into();
        self.entries.iter().find(|entry| match set {
            BarcodeType::CodeA => entry.a == value,
            BarcodeType::CodeB => entry.b == value,
            BarcodeType::CodeC => entry.c == value,
        })
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

/// A completed barcode.
#[derive(Debug, PartialEq, Eq)]
pub struct Barcode<'table> {
    code: Vec<&'table TableEntry>,
}

impl PartialOrd for Barcode<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Barcode<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.code.len().cmp(&other.code.len())
    }
}

impl Display for Barcode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        for entry in &self.code {
            output.push(entry.latin());
        }
        write!(f, "{output}")
    }
}

impl<'table> IntoIterator for Barcode<'table> {
    type Item = &'table TableEntry;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.code.into_iter()
    }
}

/// const value of the most common table.
pub const TABLE: Table = Table::new();

/// Make a barcode with the fastest available method, currently uses [`make_barcode_custom_table_dp`].
pub fn make_barcode(text: &str) -> Option<Barcode<'_>> {
    make_barcode_custom_table_dp(&TABLE, text)
}

/// Make a barcode using a custom table, you can provide [`TABLE`] to
/// use the normal table.
pub fn make_barcode_custom_table<'table>(
    table: &'table Table,
    text: &str,
) -> Option<Barcode<'table>> {
    let mut stack = VecDeque::new();
    stack.extend(BarcodeBuilder::new(table, text));
    let mut done = vec![];

    let shortest = AtomicUsize::new(usize::MAX);

    while let Some(partial_code) = stack.pop_front() {
        let options = partial_code.create_child_options();

        for option in options {
            if option.remaining.is_empty() {
                let code = option.build();
                shortest.fetch_min(code.code.len(), std::sync::atomic::Ordering::SeqCst);
                done.push(code);
            } else if option.code.len() <= shortest.load(std::sync::atomic::Ordering::SeqCst) {
                stack.push_back(option);
            }
        }
    }

    done.sort_unstable_by_key(|k| k.code.len());

    done.into_iter().next()
}

/// like [`make_barcode_custom_table`], but uses [`rayon`] to be up to 10 times faster.
#[cfg(feature = "parallel")]
pub fn make_barcode_custom_table_par<'table>(
    table: &'table Table,
    text: &str,
) -> Option<Barcode<'table>> {
    use rayon::iter::ParallelIterator;
    let walker = rayon::iter::walk_tree(BarcodeBuilder::new(table, text), |t| {
        t.create_child_options()
    });
    let res = walker.min_by_key(|v| {
        if v.remaining.is_empty() {
            v.code.len()
        } else {
            usize::MAX
        }
    });
    res.map(|r| r.build())
}

/// Find a barcode using dynamic programming. It's very fast.
pub fn make_barcode_custom_table_dp<'table>(
    table: &'table Table,
    mut text: &str,
) -> Option<Barcode<'table>> {
    let mut map = HashMap::<(&str, BarcodeType, bool), BarcodeBuilder>::new();
    for c in BarcodeBuilder::new(table, text) {
        map.insert((c.remaining, c.set, c.fnc4), c);
    }

    while !text.is_empty() {
        let next_char = text.chars().next().unwrap().len_utf8();
        for ty in [BarcodeType::CodeA, BarcodeType::CodeB, BarcodeType::CodeC] {
            for fnc_state in [false, true] {
                if let Some(builder) = map.get(&(text, ty, fnc_state)) {
                    let options = builder.create_child_options();
                    for option in options {
                        if let Some(c) = map.get(&(option.remaining, option.set, option.fnc4)) {
                            if c.cost() > option.cost() {
                                map.insert((option.remaining, option.set, option.fnc4), option);
                            }
                        } else {
                            map.insert((option.remaining, option.set, option.fnc4), option);
                        }
                    }
                }
            }
        }
        text = &text[next_char..];
    }

    map.into_iter()
        .filter_map(|((k, _, _), v)| if k.is_empty() { Some(v.build()) } else { None })
        .min_by_key(|v| v.code.len())
}

#[derive(Debug, Clone)]
struct BarcodeBuilder<'table, 's> {
    table: &'table Table,
    code: Vec<&'table TableEntry>,
    remaining: &'s str,
    set: BarcodeType,
    fnc4: bool,
}

impl<'table, 's> BarcodeBuilder<'table, 's> {
    fn new(table: &'table Table, code: &'s str) -> [BarcodeBuilder<'table, 's>; 3] {
        [
            Self {
                code: vec![
                    table
                        .entry_in_set(BarcodeValue::StartA, BarcodeType::CodeA)
                        .unwrap(),
                ],
                set: BarcodeType::CodeA,
                remaining: code,
                table,
                fnc4: false,
            },
            Self {
                code: vec![
                    table
                        .entry_in_set(BarcodeValue::StartB, BarcodeType::CodeB)
                        .unwrap(),
                ],
                set: BarcodeType::CodeB,
                remaining: code,
                table,
                fnc4: false,
            },
            Self {
                code: vec![
                    table
                        .entry_in_set(BarcodeValue::StartC, BarcodeType::CodeC)
                        .unwrap(),
                ],
                set: BarcodeType::CodeC,
                remaining: code,
                table,
                fnc4: false,
            },
        ]
    }

    fn cost(&self) -> usize {
        self.code.len()
    }

    fn create_child_options(&self) -> Vec<Self> {
        let mut options = Vec::new();
        let old_set = self.set;

        // try to see if set C works, always two numbers
        if self.remaining.len() >= 2 {
            let first_two_characters = self.remaining.chars().take(2).collect::<String>();
            if first_two_characters.chars().all(|c| c.is_ascii_digit()) {
                if let Ok(v) = first_two_characters.parse::<u8>() {
                    if let Some(s) = self.try_push(|mut s| {
                        if !matches!(old_set, BarcodeType::CodeC) {
                            s.code.push(s.in_set(BarcodeValue::CodeC)?);
                            s.set = BarcodeType::CodeC;
                        }
                        s.code.push(s.in_set(v)?);

                        s.remaining = &s.remaining[first_two_characters.len()..];
                        Some(s)
                    }) {
                        options.push(s)
                    };
                }
            }
        }

        // single character, so set A or B
        if let Some(fc) = self.remaining.chars().next() {
            // does it exist in the current set?
            if let Some(s) = self.try_push(|mut s| {
                s.code.push(s.in_set(fc)?);
                s.remaining = &s.remaining[fc.len_utf8()..];
                Some(s)
            }) {
                options.push(s);
            }

            // FNC4 support
            if let Some(s) = self.try_push(|mut s| {
                s.code.push(s.in_set(BarcodeValue::FNC4)?);
                s.fnc4 = !s.fnc4;
                s.code.push(s.in_set(fc)?);
                s.fnc4 = !s.fnc4;
                s.remaining = &s.remaining[fc.len_utf8()..];
                Some(s)
            }) {
                options.push(s);
            }
            if let Some(s) = self.try_push(|mut s| {
                s.code.push(s.in_set(BarcodeValue::FNC4)?);
                s.code.push(s.in_set(BarcodeValue::FNC4)?);
                s.fnc4 = !s.fnc4;
                s.code.push(s.in_set(fc)?);
                s.remaining = &s.remaining[fc.len_utf8()..];
                Some(s)
            }) {
                options.push(s);
            }

            // For A it's B and for B it's A, not like you can switch from A to A...
            if let Some(new_set) = old_set.other_set() {
                let shift_to = match new_set {
                    BarcodeType::CodeA => BarcodeValue::ShiftA,
                    BarcodeType::CodeB => BarcodeValue::ShiftB,
                    BarcodeType::CodeC => unreachable!(),
                };
                if let Some(s) = self.try_push(|mut s| {
                    s.code.push(s.in_set(shift_to)?);
                    s.set = new_set;
                    s.code.push(s.in_set(fc)?);
                    s.set = old_set;
                    s.remaining = &s.remaining[fc.len_utf8()..];
                    Some(s)
                }) {
                    options.push(s);
                }

                let force_to = match new_set {
                    BarcodeType::CodeA => BarcodeValue::CodeA,
                    BarcodeType::CodeB => BarcodeValue::CodeB,
                    BarcodeType::CodeC => unreachable!(),
                };

                if let Some(s) = self.try_push(|mut s| {
                    s.code.push(s.in_set(force_to)?);
                    s.set = new_set;
                    s.code.push(s.in_set(fc)?);
                    s.remaining = &s.remaining[fc.len_utf8()..];
                    Some(s)
                }) {
                    options.push(s);
                }
            }
        }

        options
    }

    fn try_push(&self, to_try: impl FnOnce(Self) -> Option<Self>) -> Option<Self> {
        to_try(self.clone())
    }

    fn in_set(&self, v: impl Into<BarcodeValue>) -> Option<&'table TableEntry> {
        let v = v.into();
        if self.fnc4 {
            if let BarcodeValue::RegularCharacter(c) = v {
                let c = (c as u32).checked_sub(128)?;
                let c = char::from_u32(c)?;
                return self.table.entry_in_set(c, self.set);
            }
        }
        self.table.entry_in_set(v, self.set)
    }

    fn build(mut self) -> Barcode<'table> {
        let mut total = 0;
        for (mut idx, character) in self.code.iter().enumerate() {
            if idx == 0 {
                idx = 1;
            }
            total += character.value() as usize * idx
        }
        total %= 103;
        self.code.push(
            self.table
                .entries
                .iter()
                .find(|v| v.value() as usize == total)
                .unwrap(),
        );
        self.code.push(
            self.table
                .entry_in_set(BarcodeValue::Stop, self.set)
                .unwrap(),
        );
        Barcode { code: self.code }
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;

    #[test]
    fn test_equality() {
        let mut rng = rand::rng();
        let len = rng.random_range(1..20);
        let random_string = rng
            .sample_iter(rand::distr::Alphanumeric)
            .take(len)
            .map(|c| c as char)
            .collect::<String>();

        let code_one = super::make_barcode_custom_table(&super::TABLE, &random_string);
        #[cfg(feature = "parallel")]
        let code_two = super::make_barcode_custom_table_par(&super::TABLE, &random_string);
        let code_three = super::make_barcode_custom_table_dp(&super::TABLE, &random_string);
        #[cfg(feature = "parallel")]
        assert_eq!(code_one, code_two);
        assert_eq!(code_one, code_three);
        #[cfg(feature = "parallel")]
        assert_eq!(code_two, code_three);
    }
}
