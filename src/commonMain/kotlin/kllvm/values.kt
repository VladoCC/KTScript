package kllvm

interface Value {
    fun IRCode() : String
    fun type() : Type
}

class StringReference(val stringConst: StringConst) : Value {
    override fun type() = Pointer(I8Type)

    override fun IRCode() = "getelementptr inbounds ([${stringConst.lengthInBytes()} x i8], [${stringConst.lengthInBytes()} x i8]* @${stringConst.id}, i32 0, i32 0)"
}

data class LocalValueRef(val name: String, val elemType: Type) : Value {
    override fun type() = elemType

    override fun IRCode() = "%$name"
}

data class GlobalValueRef(val name: String, val elemType: Type) : Value {
    override fun type() = Pointer(elemType)

    override fun IRCode() = "@$name"
}

class IntConst(val value: Int, val elemType: Type) : Value {
    override fun type() = elemType

    override fun IRCode(): String = "$value"
}

class FloatConst(val value: Float, val elemType: Type) : Value {
    override fun type() = elemType

    override fun IRCode(): String = "$value"
}

data class Null(val elemType: Type) : Value {
    override fun IRCode() = "null"

    override fun type() = elemType
}