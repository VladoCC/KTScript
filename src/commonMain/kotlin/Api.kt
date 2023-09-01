import kotlinx.serialization.json.*

fun printErr(message: String) {
}

fun processApi(apiJson: String) {
    val elem = Json.parseToJsonElement(apiJson)

    val sizeMap = mutableMapOf<String, Int>()
    val classSizes = elem.jsonObject["builtin_class_sizes"]!!
        .jsonArray[3]
        .jsonObject["sizes"]!!
        .jsonArray
    for (obj in classSizes) {
        val name = obj.jsonObject["name"].content()
        val value = obj.jsonObject["size"].content().toInt()
        if (sizeMap.containsKey(name)) {
            printErr("Name already exists: $name")
        } else {
            sizeMap[name] = value
        }
    }

    val classOffsets = elem.jsonObject["builtin_class_member_offsets"]!!
        .jsonArray[3]
        .jsonObject["classes"]!!
        .jsonArray
        .map { obj ->
            val name = obj.jsonObject["name"].content()
            val members = obj.jsonObject["members"]!!.jsonArray.map {
                val member = it.jsonObject["member"].content()
                val offset = it.jsonObject["offset"].content().toInt()
                return@map Offset(member, offset)
            }
            return@map name to members
        }.toMap()

    val enums = elem.jsonObject["global_enums"]!!.jsonArray.map { it.toEnum() }
    val functions = elem.jsonObject["utility_functions"]!!.jsonArray.map { it.toFunction() }
    val builtins = elem.jsonObject["builtin_classes"]!!.jsonArray
        .map { cl ->
            with(cl.jsonObject) {
                val name = get("name").content()
                val members =
                    if (containsKey("members")) get("members")!!
                        .jsonArray
                        .map { it.toProperty() }
                    else emptyList()
                val constants =
                    if (containsKey("constants")) get("constants")!!
                        .jsonArray
                        .map { it.toDefinition() }
                    else emptyList()
                val methods =
                    if (containsKey("methods")) get("methods")!!
                        .jsonArray
                        .map { it.toFunction() }
                    else emptyList()
                val operators =
                    if (containsKey("operators")) get("operators")!!
                        .jsonArray
                        .map { it.toOperator() }
                    else emptyList()
                val constructors =
                    if (containsKey("constructors")) get("constructors")!!
                        .jsonArray
                        .map { it.toConstructor() }
                    else emptyList()
                return@map Builtin(name = name,
                    keyed = get("is_keyed").content().toBoolean(),
                    size = sizeMap[name]!!,
                    offsets = classOffsets[name]?: emptyList(),
                    hasDestructor = get("has_destructor").content().toBoolean(),
                    indexingRetType = get("indexing_return_type")?.jsonPrimitive?.contentOrNull,
                    members = members,
                    constants = constants,
                    methods = methods,
                    operators = operators,
                    constructors = constructors
                )
            }
        }
    val classes = elem.jsonObject["classes"]!!.jsonArray
        .map { cl ->
            with(cl.jsonObject) {
                return@map Class(
                    name = get("name").content(),
                    apiType = get("api_type").content(),
                    refCounted = get("is_refcounted").content().toBoolean(),
                    instantiable = get("is_instantiable").content().toBoolean(),
                    inherits = getString("inherits"),
                    enums = get("enums")?.jsonArray?.map { it.toEnum() }?: emptyList(),
                    properties = get("properties")?.jsonArray?.map { it.toClassProperty() }?: emptyList()
                )
            }
        }
    val classMap = classes.associateBy { it.name }
    classes.forEach {
        it.inherits?.let { name ->
            it.superClass = classMap[name]
        }
    }
    val singletons = elem.jsonObject["singletons"]!!.jsonArray
        .map { ton ->
            with(ton.jsonObject) {
                return@map Singleton(get("name").content(), get("type").content())
            }
        }
    val structures = elem.jsonObject["native_structures"]!!.jsonArray
        .map { ton ->
            with(ton.jsonObject) {
                return@map Structure(
                    get("name").content(),
                    get("format")
                        .content()
                        .split(";")
                )
            }
        }
    val api = Api(
        enums = enums,
        functions = functions,
        builtins = builtins,
        classes = classes,
        singletons = singletons,
        structures = structures
    )

    val set = mutableSetOf<String>()
    api.enums.forEach {
        set.add(it.name)
    }
    api.builtins.forEach {
        set.add(it.name)
    }
    api.classes.forEach {
        set.add(it.name)
        it.enums.forEach {
            set.add(it.name)
        }
    }
    val check = { str: String -> if (str !in set) println(str) }
    val funcCheck = { fn: Function ->
        fn.returnType?.let { check(it) }
        fn.arguments.forEach {
            check(it.type)
        }
    }
    api.functions.forEach {
        funcCheck(it)
    }
    api.builtins.forEach {
        it.constants.forEach {
            it.type?.let { it1 -> check(it1) }
        }
        it.constructors.forEach {
            it.arguments.forEach { check(it.type) }
        }
        it.members.forEach {
            check(it.type)
        }
        it.methods.forEach { funcCheck(it) }
        it.operators.forEach {
            check(it.returnType)
            it.rightType?.let { it1 -> check(it1) }
        }
    }
    api.classes.forEach {
        it.methods.forEach { funcCheck(it) }
        it.constants.forEach {
            it.type?.let { it1 -> check(it1) }
        }
        it.properties.forEach { check(it.type) }
        it.signals.forEach {
            it.arguments.forEach {
                check(it.type)
            }
        }
    }
    println()
}

fun JsonElement.toProperty(): Property {
    with(jsonObject) {
        return Property(get("name").content(), get("type").content())
    }
}

fun JsonElement.toClassProperty(): ClassProperty {
    with(jsonObject) {
        return ClassProperty(get("name").content(),
            get("type").content(),
            getString("getter"),
            getString("setter"),
            getString("index")?.toInt()?: -1)
    }
}

fun JsonElement.toDefinition(): Definition {
    with(jsonObject) {
        return Definition(get("name").content(),
            getString("value"),
            getString("type"))
    }
}

fun JsonElement.toFunction(): Function {
    with(jsonObject) {
        return Function(get("name").content(),
            get("hash").content().toLong(),
            getString("category"),
            getString("return_type"),
            get("arguments")?.jsonArray?.map { it.toProperty() }?: emptyList(),
            getString("is_vararg")?.toBoolean()?: false,
            getString("is_const")?.toBoolean()?: false,
            getString("is_static")?.toBoolean()?: false)
    }
}

fun JsonElement.toOperator(): Operator {
    with(jsonObject) {
        return Operator(get("name").content(),
            get("return_type").content(),
            getString("right_type"))
    }
}

fun JsonElement.toConstructor(): Constructor {
    with(jsonObject) {
        return Constructor(get("index").content().toInt(),
            get("arguments")?.jsonArray?.map { it.toProperty() }?: emptyList())
    }
}

fun JsonElement.toEnum(): Enum {
    with(jsonObject) {
        val values = get("values")?.jsonArray?.map {
            return@map it.jsonObject["name"]
                .content() to it.jsonObject["value"]
                .content().toInt()
        }?: emptyList()
        return Enum(get("name").content(), values.toMap())
    }
}

fun JsonElement?.content(): String {
    return this?.jsonPrimitive?.content?: ""
}

fun JsonObject.getString(key: String): String? {
    return if (containsKey(key)) get(key)?.jsonPrimitive?.content else null
}

data class Api(val enums: List<Enum> = emptyList(),
               val functions: List<Function> = emptyList(),
               val builtins: List<Builtin> = emptyList(),
               val classes: List<Class> = emptyList(),
               val singletons: List<Singleton> = emptyList(),
               val structures: List<Structure> = emptyList())
data class Class(val name: String,
                 val apiType: String,
                 val refCounted: Boolean,
                 val instantiable: Boolean,
                 val inherits: String? = null,
                 val enums: List<Enum> = emptyList(),
                 val properties: List<ClassProperty> = emptyList(),
                 val constants: List<Definition> = emptyList(),
                 val methods: List<Function> = emptyList(),
                 val signals: List<Signal> = emptyList()) {
    var superClass: Class? = null
}
data class Offset(val member: String, val offset: Int)
data class Enum(val name: String, val values: Map<String, Int>)
data class Builtin(val name: String,
                   val keyed: Boolean,
                   val size: Int,
                   val offsets: List<Offset> = emptyList(),
                   val hasDestructor: Boolean = false,
                   val indexingRetType: String? = null,
                   val members: List<Property> = emptyList(),
                   val constants: List<Definition> = emptyList(),
                   val methods: List<Function> = emptyList(),
                   val operators: List<Operator> = emptyList(),
                   val constructors: List<Constructor> = emptyList())
data class Function(val name: String,
                    val hash: Long,
                    val category: String? = null,
                    val returnType: String? = null,
                    val arguments: List<Property> = emptyList(),
                    val vararg: Boolean = false,
                    val const: Boolean = false,
                    val static: Boolean = false)
data class Signal(val name: String, val arguments: List<Property> = emptyList())
data class Constructor(val index: Int, val arguments: List<Property> = emptyList())
data class Property(val name: String, val type: String)
data class ClassProperty(val name: String,
                         val type: String,
                         val getter: String? = null,
                         val setter: String? = null, val index: Int = -1)
data class Definition(val name: String, val value: String? = null, val type: String? = null)
data class Operator(val name: String, val returnType: String, val rightType: String? = null)
data class Singleton(val name: String, val type: String)
data class Structure(val name: String, val format: List<String>)