module Domain

type Domain<'T> =
    abstract Less : 'T * 'T -> bool
    (*abstract LessEqual : 'T * 'T -> bool
    abstract Equal : 'T * 'T -> bool
    abstract GreaterEqual : 'T * 'T -> bool
    abstract Greater : 'T * 'T -> bool
    abstract NotEqual : 'T * 'T -> bool

    abstract Sum : 'T * 'T -> 'T
    abstract Minus : 'T * 'T -> 'T
    abstract Multiply : 'T * 'T -> 'T
    abstract Division : 'T * 'T -> 'T
    abstract Modulo : 'T * 'T -> 'T
    abstract Negate : 'T -> 'T*)
