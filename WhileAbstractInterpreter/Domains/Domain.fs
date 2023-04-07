module Domain

type Domain<'T> =
    abstract Less : 'T -> bool
    //abstract LessEqual : 'T -> bool
    //abstract Equal : 'T -> bool
    //abstract GreaterEqual : 'T -> bool
    //abstract Greater : 'T -> bool
    //abstract NotEqual: 'T -> bool

    //abstract Sum : 'T -> 'T
    //abstract Minus : 'T -> 'T
    //abstract Multiply : 'T -> 'T
    //abstract Division : 'T -> 'T
    //abstract Modulo : 'T -> 'T
    //abstract Negate : unit -> 'T
