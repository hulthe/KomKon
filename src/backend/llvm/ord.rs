use std::fmt::{self, Display, Formatter};

/// This enum defines the various ways in which two integers can be compared.
#[derive(Clone, Copy, Debug)]
pub enum LLVMIOrd {
    EQ, //equal
    NE, //not equal
    UGT, //unsigned greater than
    UGE, //unsigned greater or equal
    ULT, //unsigned less than
    ULE, //unsigned less or equal
    SGT, //signed greater than
    SGE, //signed greater or equal
    SLT, //signed less than
    SLE, //signed less or equal
}

/// This enum defines the various ways in which two floats can be compared.
#[derive(Clone, Copy, Debug)]
pub enum LLVMFOrd {
    OEQ, //ordered and equal
    OGT, //ordered and greater than
    OGE, //ordered and greater than or equal
    OLT, //ordered and less than
    OLE, //ordered and less than or equal
    ONE, //ordered and not equal
    ORD, //ordered (no nans)
    UEQ, //unordered or equal
    UGT, //unordered or greater than
    UGE, //unordered or greater than or equal
    ULT, //unordered or less than
    ULE, //unordered or less than or equal
    UNE, //unordered or not equal
    UNO, //unordered (either nans)
}

impl Display for LLVMIOrd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl Display for LLVMFOrd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

