use std::{collections::VecDeque, fmt::Display, sync::atomic::AtomicUsize};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BarcodeType {
    CodeA,
    CodeB,
    CodeC,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BarcodeValue {
    RegularCharacter(char),
    Digit(u8),
    FNC1,
    FNC2,
    FNC3,
    FNC4,
    StartA,
    StartB,
    StartC,
    Stop,
    ShiftA,
    ShiftB,
    CodeA,
    CodeB,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TableEntry {
    value: u8,
    a: BarcodeValue,
    b: BarcodeValue,
    c: BarcodeValue,
    latin: char,
}

impl TableEntry {
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

    pub fn value(&self) -> u8 {
        self.value
    }

    pub fn latin(&self) -> char {
        self.latin
    }
}

#[derive(Debug)]
pub struct Table {
    entries: [TableEntry; 107],
}

impl Table {
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
                    106,
                    BarcodeValue::Stop,
                    BarcodeValue::Stop,
                    BarcodeValue::Stop,
                    'Î',
                ),
            ],
        }
    }

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
static TABLE: Table = Table::new();

pub fn make_barcode(text: &str) -> Option<Barcode<'_>> {
    #[cfg(feature = "parallel")]
    let res = make_barcode_custom_table_par(&TABLE, text);
    #[cfg(not(feature = "parallel"))]
    let res = make_barcode_custom_table(&TABLE, text);
    res
}

pub fn make_barcode_custom_table<'table>(
    table: &'table Table,
    text: &str,
) -> Option<Barcode<'table>> {
    let mut stack = VecDeque::new();
    stack.push_back(BarcodeBuilder::new(table, text));
    let mut done = vec![];

    let shortest = AtomicUsize::new(usize::MAX);

    while let Some(partial_code) = stack.pop_front() {
        let options = partial_code.create_child_options();

        for option in options {
            if option.remaining.is_empty() {
                let code = option.build();
                shortest.fetch_min(code.code.len(), std::sync::atomic::Ordering::SeqCst);
                done.push(code);
            } else {
                if option.code.len() <= shortest.load(std::sync::atomic::Ordering::SeqCst) {
                    stack.push_back(option);
                }
            }
        }
    }

    done.sort_unstable_by_key(|k| k.code.len());

    done.into_iter().next()
}

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

#[derive(Debug, Clone)]
struct BarcodeBuilder<'table, 's> {
    table: &'table Table,
    code: Vec<&'table TableEntry>,
    remaining: &'s str,
    set: BarcodeType,
}

impl<'table, 's> BarcodeBuilder<'table, 's> {
    fn new(table: &'table Table, code: &'s str) -> Self {
        Self {
            table,
            code: Vec::new(),
            remaining: code,
            set: BarcodeType::CodeA,
        }
    }

    fn create_child_options(&self) -> Vec<Self> {
        if self.code.is_empty() {
            return vec![
                Self {
                    code: vec![
                        self.table
                            .entry_in_set(BarcodeValue::StartA, BarcodeType::CodeA)
                            .unwrap(),
                    ],
                    ..self.clone()
                },
                Self {
                    code: vec![
                        self.table
                            .entry_in_set(BarcodeValue::StartB, BarcodeType::CodeB)
                            .unwrap(),
                    ],
                    set: BarcodeType::CodeB,
                    ..self.clone()
                },
                Self {
                    code: vec![
                        self.table
                            .entry_in_set(BarcodeValue::StartC, BarcodeType::CodeC)
                            .unwrap(),
                    ],
                    set: BarcodeType::CodeC,
                    ..self.clone()
                },
            ];
        }
        let mut options = Vec::new();
        let old_set = self.set;

        // try to see if set C works, always two numbers

        if self.remaining.len() >= 2 {
            let first_two_characters = &self.remaining[..2];
            if first_two_characters.chars().all(|c| c.is_ascii_digit()) {
                if let Ok(v) = first_two_characters.parse::<u8>() {
                    if let Some(s) = self.try_push(|mut s| {
                        if !matches!(old_set, BarcodeType::CodeC) {
                            s.code.push(s.in_set(BarcodeValue::CodeC)?);
                            s.set = BarcodeType::CodeC;
                        }
                        s.code.push(s.in_set(v)?);
                        s.remaining = &s.remaining[2..];
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
            // shift A or B?
            for new_set in [BarcodeType::CodeA, BarcodeType::CodeB] {
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


