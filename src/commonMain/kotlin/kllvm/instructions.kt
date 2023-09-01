package kllvm

interface Instruction {
    fun IRCode() : String
    fun type() : Type?
}

class ReturnInt(val value: Int) : Instruction {
    override fun type() = null

    override fun IRCode() = "ret i32 $value"
}

class Return(val value: Value) : Instruction {
    override fun type() = null

    override fun IRCode() = "ret ${value.type().IRCode()} ${value.IRCode()}"
}

class ReturnVoid : Instruction {
    override fun IRCode() = "ret void"

    override fun type() = VoidType
}

class Load(val value: Value) : Instruction {
    override fun IRCode() = "load ${type().IRCode()}, ${value.type().IRCode()} ${value.IRCode()}"
    override fun type() = (value.type() as Pointer).element
}

class IfInstruction(val condition: Value, val yesLabel: BlockBuilder, val noLabel: BlockBuilder) : Instruction {
    override fun IRCode() = "br ${condition.type().IRCode()} ${condition.IRCode()}, label %${yesLabel.name}, label %${noLabel.name}"
    override fun type() = null
}

class JumpInstruction(val label: Label) : Instruction {
    override fun IRCode() = "br label %${label.name}"
    override fun type() = null
}

enum class ComparisonType(val code: String) {
    Equal("eq"),
    NotEqual("ne")
}

data class Comparison(val comparisonType: ComparisonType, val left: Value, val right: Value) : Instruction {
    override fun IRCode() = "icmp ${comparisonType.code} ${left.type().IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = BooleanType
}

class TempValue(val name: String, val value: Instruction) : Instruction {
    override fun IRCode(): String = "%$name = ${value.IRCode()}"
    fun reference() = LocalValueRef(name, value.type()!!)
    override fun type() = value.type()!!
}

class Store(val value: Value, val destination: Value) : Instruction {
    override fun IRCode(): String {
        return "store ${value.type().IRCode()} ${value.IRCode()}, ${destination.type().IRCode()} ${destination.IRCode()}"
    }
    override fun type() = null
}

class GetElementPtr(val elemType: Type, val pointer: Value, val index: Value) : Instruction {
    override fun IRCode() = "getelementptr inbounds ${elemType.IRCode()}, ${pointer.type().IRCode()} ${pointer.IRCode()}, i64 ${index.IRCode()}"
    override fun type() = Pointer(elemType)
}

class Call(val returnType: Type, val name: String, vararg params: Value) : Instruction {
    private val _params = mutableListOf<Value>()
    init {
        params.forEach { _params.add(it) }
    }
    override fun IRCode(): String {
        return "call ${returnType.IRCode()} @$name(${_params.map {"${it.type().IRCode()} ${it.IRCode()}"}.joinToString(separator = ", ")})"
    }
    override fun type() = returnType
}

class CallWithBitCast(val declaration: FunctionDeclaration, vararg params: Value) : Instruction {
    private val _params = mutableListOf<Value>()
    init {
        params.forEach { _params.add(it) }
    }
    override fun IRCode(): String {
        val argTypesStrs = mutableListOf<String>()
        _params.forEach { argTypesStrs.add(it.type().IRCode()) }
        if (declaration.varargs) {
            argTypesStrs.add("...")
        }
        val adaptedSignature = "${declaration.returnType.IRCode()} (${argTypesStrs.joinToString(separator = ", ")})"
        val paramsStr = _params.map { "${it.type().IRCode()} ${it.IRCode()}" }.joinToString(separator = ", ")
        return "call $adaptedSignature bitcast (${declaration.ptrSignature()} @${declaration.name} to $adaptedSignature*)($paramsStr)"
    }
    override fun type() = declaration.returnType
}

class Printf(val stringFormat: Value, vararg params: Value) : Instruction {
    private val _params = params

    override fun IRCode(): String {
        var paramsString = ""
        _params.forEach { paramsString += ", ${it.type().IRCode()} ${it.IRCode()}" }
        return "call i32 (i8*, ...) @printf(i8* ${stringFormat.IRCode()}$paramsString)"
    }
    override fun type() = null
}

class SignedIntDivision(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "sdiv ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class UnsignedIntDivision(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "udiv ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class FloatDivision(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "fdiv ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class IntMultiplication(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "mul ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class FloatMultiplication(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "fmul ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class IntAddition(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "add ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class FloatAddition(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "fadd ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class IntSubtraction(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "sub ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class FloatSubtraction(val left: Value, val right: Value) : Instruction {
    val elemType = left.type()
    override fun IRCode() = "fsub ${elemType.IRCode()} ${left.IRCode()}, ${right.IRCode()}"
    override fun type() = elemType
}

class ConversionFloatToSignedInt(val value: Value, val targetType: Type) : Instruction {
    override fun IRCode() = "fptosi ${value.type().IRCode()} ${value.IRCode()} to ${targetType.IRCode()}"

    override fun type() = targetType
}

class ConversionFloatToUnsignedInt(val value: Value, val targetType: Type) : Instruction {
    override fun IRCode() = "fptoui ${value.type().IRCode()} ${value.IRCode()} to ${targetType.IRCode()}"

    override fun type() = targetType
}

class ConversionSignedIntToFloat(val value: Value, val targetType: Type) : Instruction {
    override fun IRCode() = "sitofp ${value.type().IRCode()} ${value.IRCode()} to ${targetType.IRCode()}"

    override fun type() = targetType
}

class ConversionUnsignedIntToFloat(val value: Value, val targetType: Type) : Instruction {
    override fun IRCode() = "uitofp ${value.type().IRCode()} ${value.IRCode()} to ${targetType.IRCode()}"

    override fun type() = targetType
}
