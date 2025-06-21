pub enum BarcodeType {
    CodeA,
    CodeB,
    CodeC,
}

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
    ShiftC,
}

trait ConstInto {
    fn const_into(self) -> BarcodeValue;
}

impl ConstInto for char {
    fn const_into(self) -> BarcodeValue {
        BarcodeValue::RegularCharacter(self)
    }
}

impl ConstInto for u8 {
    fn const_into(self) -> BarcodeValue {
        BarcodeValue::Digit(self)
    }
}

impl ConstInto for BarcodeType {
    fn const_into(self) -> BarcodeValue {
        match self {
            BarcodeType::CodeA => BarcodeValue::StartA,
            BarcodeType::CodeB => BarcodeValue::StartB,
            BarcodeType::CodeC => BarcodeValue::StartC,
        }
    }
}
impl ConstInto for BarcodeValue {
    fn const_into(self) -> BarcodeValue {
        self
    }
}

struct TableEntry {
    a: BarcodeValue,
    b: BarcodeValue,
    c: BarcodeValue,
    latin: char,
}

impl TableEntry {
    fn new(a: BarcodeValue, b: BarcodeValue, c: BarcodeValue, latin: char) -> Self {
        TableEntry { a, b, c, latin }
    }
}
