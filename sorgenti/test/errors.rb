=begin
Testing the errors
=end

# Error: RedeclaredParam
def int error(char ch, ref float f, const int m, bool m)

    # Error: InvalidInfixOp
    int x = 5
    float y = 2.0
    x % y

    # Error: InvalidAssgnOp
    float z = 27
    z %= 5

    # Error: IncompatibleTypes
    string s = "test"
    bool b = true
    s = s - b

    # Error: InvalidUnaryOp
    char c = 'a'
    c = ~c

    # Error: UnexpectedType
    int num = 3.4

    # Error: ExpectedRefType
    int* p = &x
    p[0] = 12

    # Error: InvalidDerefOp
    bool d = false
    *d = true

    # Error: UnexpectedRetType
    return "error"

    # Error: UndeclaredVarIdent
    var = 2.0 * 3

    # Error: UnInitVarIdent
    float u

    # Error: UndeclaredFunIdent
    foo()

    # Error: VarIdentNotModif
    m = 10

    # Error: InvalidIncDecrVC
    (++x)++

    # Error: InvalidIncDecrOp
    ++ch

    # Error: InvalidRefVC
    f = &(x++)

    # Error: WrongArrayLitDim
    int[2][3] arr = [[1,2,3],[4,5]]

    # Error: ExpectedArrayLit
    string[2] as = "a string"

    # Error: UnexpectedArrayLit
    [42] + 15

    # Error: WrongScalarInit
    int wsi = [2,0,1,8]

    # Error: WrongScalarAssgn
    ch = ['a','r','r']

    # Error: InvalidAssgnVC
    f++ = 4.6

    # Error: InvalidAssgnArray
    arr = [[1,2,3],[4,5,6]]

    # Error: RedeclaredVar
    string arr = "ops"

    # Error: InvalidGuard
    if 0.3
        return 1
    end

    # Error: InvalidRangeOp
    for x in 1..3.7
        writeString("ok")
    end

    # Error: InvalidJumpStmt
    break

    # Error: RetUnitFromVal
    return

    # Error: NilDeclaration
    nil v = 'n'

    # Error: SizeArrayNonPositive
    int[-3] npArr = [7, 10, 14]

    # Error: SizeArrayNonInteger
    int[2.9] niArr = [7, 10, 14]

    # Error: VarSizeArray
    int[x] vsArr = [7, 10, 14]

    # Error: InvalidRetArrayLit
    return ["array","initializer"]

end

# Error: RedeclaredFun
def error()

    # Error: UnexpectedArgType
    float lv = 5.1
    int t = 3 + 4 * 6
    error(false, lv, t, true)

    # Error: UnexpectedArgVC
    error('b', 5.1, t, false)

    # Error: WrongArgNum
    error()

end

# Errors: ResNotInit, ReturnNotPresent
def bool test(res int i)

    # Error: CaptureOfResParam
    def int AshKetchum()

        return i

    end

    # Error: UnInitVarIdent
    i += 5

    if true
        i = 2
    elsif false
        int i = 4
        i = 8
    else
        i = 9
    end

end

# Errors: RetValFromUnit, VarSizeObjectArg
def nil bar(string s)

    return 0

end

# Errors: InvalidArrayRetType, InvalidArrayResMod
def float[5] invalid(res char[2] rc)

    float[5] fArr = [1.2,2.3,3.4,4.5,5.6]

    return fArr
end

# Error: UnexpectedStmt
while true

end

# Error: RedeclaredFun
def writeString()
end

# Error: MissingMain
