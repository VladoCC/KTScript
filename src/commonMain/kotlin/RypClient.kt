import language.*

fun run(code: String, task: Task) {
    compile(code, task, cryptParser())
}

fun cryptParser(): Compiler {
    val regexSkip = object: Skip {
        private val regex = Regex("[;\\s]+")

        override fun consume(code: String, pos: Int): Int {
            val result = regex.matchAt(code, pos)
            if (result != null) {
                return result.value.length
            }
            return 0
        }
    }
    val singleLineCommentSkip = object: Skip {
        override fun consume(code: String, pos: Int): Int {
            if (code.length > pos + 1 && code[pos] == '/' && code[pos + 1] == '/') {
                var move = 2
                while (code.length > pos + move && code[pos + move] != '\n') {
                    move += 1
                }
                return move + 1
            }
            return 0
        }
    }

    val multiLineCommentSkip = object: Skip {
        override fun consume(code: String, pos: Int): Int {
            if (code.length > pos + 1 && code[pos] == '/' && code[pos + 1] == '*') {
                var counter = 1
                var move = 3
                while (true) {
                    if (pos + move >= code.length) {
                        return code.length - pos
                    }

                    if (code[pos + move - 1] == '*' && code[pos + move] == '/') {
                        counter--
                        if (counter == 0) {
                            break
                        }
                        move++
                    } else if (code[pos + move - 1] == '/' && code[pos + move] == '*') {
                        counter++
                        move++
                    }
                    move++
                }
                return move + 1
            }
            return 0
        }
    }

    return parser(listOf(regexSkip, singleLineCommentSkip, multiLineCommentSkip)) {
        val newline = regTerm(
            "\\n\\s*",
            "newline",
            listOf(singleLineCommentSkip, multiLineCommentSkip)
        )

        val file = nonTerm("file")
        val packageHeader = nonTerm("packageHeader")
        val importHeader = nonTerm("importHeader")
        val topLevelObject = nonTerm("topLevelObject")

        val declaration = nonTerm("declaration")
        val genericVariableDeclaration = nonTerm("genericVariableDeclaration")
        val variableDeclaration = nonTerm("variableDeclaration")
        val multiVariableDeclaration = nonTerm("multiVariableDeclaration")

        val classDeclaration = nonTerm("classDeclaration")
        val objectDeclaration = nonTerm("objectDeclaration")
        val funDeclaration = nonTerm("funDeclaration")
        val propertyDeclaration = nonTerm("propertyDeclaration")
        val typeAlias = nonTerm("typeAlias")

        val classBody = nonTerm("classBody")
        val classMemberDeclaration = nonTerm("classMemberDeclaration")
        val companionObject = nonTerm("companionObject")
        val anonymousInitializer = nonTerm("anonymousInitializer")
        val secondaryConstructor = nonTerm("secondaryConstructor")
        val enumClassBody = nonTerm("enumClassBody")
        val enumEntries = nonTerm("enumEntries")
        val enumEntry = nonTerm("enumEntry")
        val functionBody = nonTerm("functionBody")
        val controlStructureBody = nonTerm("controlStructureBody")
        val block = nonTerm("block")

        val modifier = nonTerm("modifier")
        val classModifier = nonTerm("classModifier")
        val memberModifier = nonTerm("memberModifier")
        val typeModifier = nonTerm("typeModifiers")
        val functionModifier = nonTerm("functionModifier")
        val propertyModifier = nonTerm("propertyModifier")
        val inheritanceModifier = nonTerm("inheritanceModifier")
        val parameterModifier = nonTerm("parameterModifier")
        val typeProjectionModifier = nonTerm("typeProjectionModifier")
        val typeParameterModifier = nonTerm("typeParameterModifier")
        val visibilityModifier = nonTerm("visibilityModifier")

        val annotation = nonTerm("annotation")
        val inlineAnnotation = nonTerm("inlineAnnotation")
        val annotationPrefix = nonTerm("annotationPrefix")
        val annotationUseSiteTarget = nonTerm("annotationUseSiteTarget")
        val useSite = nonTerm("useSite")
        val annotationBody = nonTerm("annotationBody")
        val singleAnnotation = nonTerm("singleAnnotation")
        val multiAnnotation = nonTerm("multiAnnotation")
        val fileAnnotation = nonTerm("fileAnnotation")

        val classKeyword = nonTerm("classKeyword")
        val typeParams = nonTerm("typeParams")
        val typeParam = nonTerm("typeParam")
        val typeArguments = nonTerm("typeArguments")
        val typeProjection = nonTerm("typeProjection")

        val type = nonTerm("type")
        val typeSpec = nonTerm("typeSpec")
        val functionType = nonTerm("functionType")
        val parenthesizedType = nonTerm("parenthesizedType")
        val nullableType = nonTerm("nullableType")
        val typeReference = nonTerm("typeReference")

        val receiverType = nonTerm("receiverType")
        val functionTypeParams = nonTerm("functionTypeParams")
        val functionTypeParamsBody = nonTerm("functionTypeParamsBody")
        val functionValueParameters = nonTerm("functionValueParameters")
        val functionValueParameter = nonTerm("functionValueParameter")
        val functionValueParameterWithOptionalType = nonTerm("functionValueParameterWithOptionalType")
        val param = nonTerm("param")
        val paramOptions = nonTerm("paramOptions")
        val parameterWithOptionalType = nonTerm("parameterWithOptionalType")

        val primaryConstructor = nonTerm("primaryConstructor")
        val classParameters = nonTerm("classParameters")
        val classParameter = nonTerm("classParameter")

        val constructorInvocation = nonTerm("constructorInvocation")
        val constructorDelegationCall = nonTerm("constructorDelegationCall")
        val userType = nonTerm("userType")
        val simpleUserType = nonTerm("simpleUserType")
        val valueArguments = nonTerm("valueArguments")
        val valueArgument = nonTerm("valueArgument")

        val assignment = nonTerm("assignment")
        val assignableExpression = nonTerm("assignableExpression")
        val parenthesizedAssignableExpression = nonTerm("parenthesizedAssignableExpression")
        val assignmentAndOperator = nonTerm("assignmentAndOperator")
        val directlyAssignableExpression = nonTerm("directlyAssignableExpression")
        val parenthesizedDirectlyAssignableExpression = nonTerm("parenthesizedDirectlyAssignableExpression")
        val assignableSuffix = nonTerm("assignableSuffix")
        val propertyInit = nonTerm("propertyInit")
        val propertyAccess = nonTerm("propertyAccess")
        val getter = nonTerm("getter")
        val setter = nonTerm("setter")

        val loopStatement = nonTerm("loopStatement")
        val forStatement = nonTerm("forStatement")
        val whileStatement = nonTerm("whileStatement")
        val doWhileStatement = nonTerm("doWhileStatement")

        val semi = nonTerm("semi")
        val semis = nonTerm("semis")
        val semiInternal = nonTerm("semiInternal")

        val expression = nonTerm("expression")
        val disjunction = nonTerm("disjunction")
        val conjunction = nonTerm("conjunction")
        val equality = nonTerm("equality")
        val comparison = nonTerm("comparison")
        val genericCallLikeComparison = nonTerm("genericCallLikeComparison")
        val infixOperation = nonTerm("infixOperation")
        val elvisExpression = nonTerm("elvisExpression")
        val infixFunctionCall = nonTerm("infixFunctionCall")
        val rangeExpression = nonTerm("rangeExpression")
        val additiveExpression = nonTerm("additiveExpression")
        val multiplicativeExpression = nonTerm("multiplicativeExpression")
        val asExpression = nonTerm("asExpression")
        val prefixUnaryExpression = nonTerm("prefixUnaryExpression")
        val postfixUnaryExpression = nonTerm("postfixUnaryExpression")
        val primaryExpression = nonTerm("primaryExpression")

        val equalityOperator = nonTerm("equalityOperator")
        val comparisonOperator = nonTerm("comparisonOperator")
        val annotatedLambda = nonTerm("annotatedLambda")
        val lambdaLiteral = nonTerm("lambdaLiteral")
        val lambdaParams = nonTerm("lambdaParams")
        val lambdaParam = nonTerm("lambdaParam")
        val callSuffix = nonTerm("callSuffix")
        val infixOperator = nonTerm("infixOperator")
        val inOperator = nonTerm("inOperator")
        val isOperator = nonTerm("isOperator")
        val asOperator = nonTerm("asOperator")
        val elvis = nonTerm("elvis")
        val additiveOperator = nonTerm("additiveOperator")
        val multiplicativeOperator = nonTerm("multiplicativeOperator")
        val unaryPrefix = nonTerm("unaryPrefix")
        val prefixUnaryOperator = nonTerm("prefixUnaryOperator")
        val postfixUnarySuffix = nonTerm("postfixUnarySuffix")
        val indexingSuffix = nonTerm("indexingSuffix")
        val navigationSuffix = nonTerm("navigationSuffix")
        val postfixUnaryOperator = nonTerm("postfixUnaryOperator")

        val memberAccessOperator = nonTerm("memberAccessOperator")
        val safeNav = nonTerm("safeNav")

        val parenthesizedExpression = nonTerm("parenthesizedExpression")
        val literalConstant = nonTerm("literalConstant")
        val callableReference = nonTerm("callableReference")
        val objectLiteral = nonTerm("objectLiteral")
        val collectionLiteral = nonTerm("collectionLiteral")
        val ifExpression = nonTerm("ifExpression")
        val whenExpression = nonTerm("whenExpression")
        val tryExpression = nonTerm("tryExpression")
        val jumpExpression = nonTerm("jumpExpression")

        val booleanLiteral = nonTerm("booleanLiteral")
        val realLiteral = nonTerm("realLiteral")
        val longLiteral = nonTerm("longLiteral")
        val unsignedLiteral = nonTerm("unsignedLiteral")
        val numberLiteral = nonTerm("numberLiteral")

        val delegationSpecifiers = nonTerm("delegationSpecifiers")
        val annotatedDelegationSpecifier = nonTerm("annotatedDelegationSpecifier")
        val delegationSpecifier = nonTerm("delegationSpecifier")
        val explicitDelegation = nonTerm("explicitDelegation")
        val whenSubject = nonTerm("whenSubject")
        val whenEntry = nonTerm("whenEntry")
        val whenCondition = nonTerm("whenCondition")
        val rangeTest = nonTerm("rangeTest")
        val typeTest = nonTerm("typeTest")
        val catchBlock = nonTerm("catchBlock")
        val finallyBlock = nonTerm("finallyBlock")

        val statements = nonTerm("statements")
        val statement = nonTerm("statement")

        val identBasic = regTerm("[A-Za-z_][A-Za-z_0-9]*", "identBasic")
        val hexTerm = regTerm("0[Xx][0-9A-Fa-f]([0-9A-Fa-f_]*[0-9A-Fa-f])?", "hex")
        val binTerm = regTerm("0[Bb][01]([01_]*[01])?", "bin")
        val floatTerm = regTerm("((([0-9]([0-9_]*[0-9])?)?.[0-9]([0-9_]*[0-9])?([eE][+-]?([0-9]([0-9_]*[0-9])?))?|([0-9]([0-9_]*[0-9])?)([eE][+-]?([0-9]([0-9_]*[0-9])?)))|([0-9]([0-9_]*[0-9])?))[fF]", "float")
        val doubleTerm = regTerm("([0-9]([0-9_]*[0-9])?)?\\.[0-9]([0-9_]*[0-9])?([eE][+-]?([0-9]([0-9_]*[0-9])?))?|([0-9]([0-9_]*[0-9])?)([eE][+-]?([0-9]([0-9_]*[0-9])?))", "double")
        val integerTerm = regTerm("([1-9][\\d_]*\\d)|(\\d)", "integer")
        val characterTerm = regTerm("'((\\\\((u[0-9A-Fa-f]{4})|t|b|r|n|\'|\"|\\|\$))|([^\\n]|[^\\r]|[^\\\\]|[^\']))'","character")
        val longMarker = regTerm("[lL]", "longMarker")
        val unsignedMarker = regTerm("[uU]", "unsignedMarker")
        /*val lineStrEscapedChar = regTerm(Regex("((\\\\((u[0-9A-Fa-f]{4})|t|b|r|n|'|\"|\\|\$)))"), "escapedIdentifier")*/
        val identStr = regTerm("`[^\\r\\n`]+`", "identStr")
        /*val lineStrText = regTerm("[^\\\"\$]+|\$", "lineStrText")
        val multiLineStrText = regTerm("[^\"\$]+|\$", "multiLineStrText")*/

        val declarationKeyword = nonTerm("declarationKeyword")

        val stringLiteral = registerTerm(
            StringLexeme(
                identBasic,
                identStr,
                skip
            ).apply {
                addOnLanguageConstructedCallback {
                    setExpressionLanguage(it.subgrammar(expression))
                }
            }
        )

        val constructorTerm = wordTerm("constructor")
        val crossinlineTerm = wordTerm("crossinline")
        val annotationTerm = wordTerm("annotation")
        val companionTerm = wordTerm("companion")
        val interfaceTerm = wordTerm("interface")
        val typealiasTerm = wordTerm("typealias")
        val protectedTerm = wordTerm("protected")
        val continueTerm = wordTerm("continue")
        val overrideTerm = wordTerm("override")
        val lateinitTerm = wordTerm("lateinit")
        val internalTerm = wordTerm("internal")
        val operatorTerm = wordTerm("operator")
        val externalTerm = wordTerm("external")
        val abstractTerm = wordTerm("abstract")
        val noinlineTerm = wordTerm("noinline")
        val propertyTerm = wordTerm("property")
        val receiverTerm = wordTerm("receiver")
        val setparamTerm = wordTerm("setparam")
        val delegateTerm = wordTerm("delegate")
        val dynamicTerm = wordTerm("dynamic")
        val packageTerm = wordTerm("package")
        val finallyTerm = wordTerm("finally")
        val suspendTerm = wordTerm("suspend")
        val privateTerm = wordTerm("private")
        val tailrecTerm = wordTerm("tailrec")
        val reifiedTerm = wordTerm("reified")
        val objectTerm = wordTerm("object")
        val returnTerm = wordTerm("return")
        val publicTerm = wordTerm("public")
        val importTerm = wordTerm("import")
        val sealedTerm = wordTerm("sealed")
        val inlineTerm = wordTerm("inline")
        val varargTerm = wordTerm("vararg")
        val classTerm = wordTerm("class")
        val falseTerm = wordTerm("false")
        val superTerm = wordTerm("super")
        val catchTerm = wordTerm("catch")
        val throwTerm = wordTerm("throw")
        val breakTerm = wordTerm("break")
        val whileTerm = wordTerm("while")
        val innerTerm = wordTerm("inner")
        val infixTerm = wordTerm("infix")
        val constTerm = wordTerm("const")
        val finalTerm = wordTerm("final")
        val fieldTerm = wordTerm("field")
        val paramTerm = wordTerm("param")
        val trueTerm = wordTerm("true")
        val fileTerm = wordTerm("file")
        val nullTerm = wordTerm("null")
        val thisTerm = wordTerm("this")
        val elseTerm = wordTerm("else")
        val whenTerm = wordTerm("when")
        val initTerm = wordTerm("init")
        val enumTerm = wordTerm("enum")
        val dataTerm = wordTerm("data")
        val openTerm = wordTerm("open")
        val asNullableTerm = wordTerm("as?")
        val notInTerm = wordTerm("!in")
        val notIsTerm = wordTerm("!is")
        val valTerm = wordTerm("val")
        val varTerm = wordTerm("var")
        val funTerm = wordTerm("fun")
        val outTerm = wordTerm("out")
        val tryTerm = wordTerm("try")
        val forTerm = wordTerm("for")
        val getTerm = wordTerm("get")
        val setTerm = wordTerm("set")
        val doTerm = wordTerm("do")
        val asTerm = wordTerm("as")
        val inTerm = wordTerm("in")
        val isTerm = wordTerm("is")
        val byTerm = wordTerm("by")
        val ifTerm = wordTerm("if")

        val exactEqual = term("===")
        val exactNotEqual = term("!==")

        val singleRightArrow = term("->")
        val disjuncTerm = term("||")
        val conjuncTerm = term("&&")
        val equal = term("==")
        val notEqual = term("!=")
        val lessOrEquals = term("<=")
        val moreOrEquals = term(">=")

        val plusAssign = term("+=")
        val minusAssign = term("-=")
        val multAssign = term("*=")
        val divAssign = term("/=")
        val moduloAssign = term("%=")
        val doublePlus = term("++")
        val doubleMinus = term("--")
        val doubleDot = term("..")
        val doubleColon = term("::")
        val less = term("<")
        val more = term(">")

        val plus = term("+")
        val minus = term("-")
        val mult = term("*")
        val div = term("/")
        val modulo = term("%")
        val leftBracket = term("(")
        val rightBracket = term(")")
        val leftSquareBracket = term("[")
        val rightSquareBracket = term("]")
        val leftCurlyBracket = term("{")
        val rightCurlyBracket = term("}")
        val leftArrow = term("<")
        val rightArrow = term(">")
        val eq = term("=")
        val dot = term(".")
        val at = term("@")
        val dollar = term("$")
        val colon = term(":")
        val quotation = term('"'.toString())

        val semicolon = term(";")
        val comma = term(",")
        val excl = term("!")
        val quest = term("?")

        val importEnding = nonTerm("importEnding")

        val chainIdent = nonTerm("chainIdent")
        val ident = nonTerm("identifier")

        rule(root, file)
        rule(file, zeroOrMore(fileAnnotation)
                + optional(packageHeader)
                + zeroOrMore(importHeader)
                + oneOrMore(topLevelObject)
        )

        rule(packageHeader, packageTerm + chainIdent + semi)
        rule(importHeader, importTerm + chainIdent + optional(importEnding) + semi)
        rule(topLevelObject, declaration)
        rule(declaration, classDeclaration or objectDeclaration or
                funDeclaration or propertyDeclaration or typeAlias)

        rule(classDeclaration, zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + classKeyword
                + ident
                + optional(typeParams)
                + optional(primaryConstructor)
                + optional(colon + delegationSpecifiers)
                + optional(classBody)
                or zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + classKeyword
                + ident
                + optional(typeParams)
                + optional(primaryConstructor)
                + optional(colon + delegationSpecifiers)
                + optional(enumClassBody)
        )
        rule(objectDeclaration, zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + objectTerm
                + ident
                + optional(colon + delegationSpecifiers)
                + optional(classBody)
        )
        rule(funDeclaration, zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + funTerm
                + optional(typeParams)
                + optional(receiverType + dot)
                + ident
                + functionValueParameters
                + optional(colon + type)
                + functionBody
        )
        rule(propertyDeclaration, zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + declarationKeyword
                + optional(typeParams)
                + genericVariableDeclaration
                + optional(propertyInit)
                + optional(semicolon)
                + optional(propertyAccess)
        )
        rule(typeAlias, zeroOrMore(annotation)
                + zeroOrMore(modifier)
                + typealiasTerm
                + ident
                + optional(typeParams)
                + eq
                + type
        )

        rule(classMemberDeclaration,declaration
                or companionObject
                or anonymousInitializer
                or secondaryConstructor
        )
        rule(companionObject, zeroOrMore(modifier)
                + companionTerm
                + objectTerm
                + optional(ident)
                + optional(colon + delegationSpecifiers)
                + optional(classBody)
        )
        rule(anonymousInitializer, initTerm + block)
        rule(secondaryConstructor, zeroOrMore(modifier)
                + constructorTerm
                + functionValueParameters
                + optional(colon + constructorDelegationCall)
                + optional(block)
        )
        rule(constructorDelegationCall, thisTerm + valueArguments
                or superTerm + valueArguments
        )

        rule(propertyInit, eq + expression or byTerm + expression)
        rule(propertyAccess, getter + optional(optional(semi) + setter)
                or setter + optional(optional(semi) + getter)
        )
        rule(getter, optional(visibilityModifier)
                + getTerm
                + optional(leftBracket + rightBracket + optional(colon + type) + functionBody)
        )
        rule(setter,
            optional(visibilityModifier)
                    + setTerm
                    + optional(leftBracket
                    + functionValueParameterWithOptionalType
                    + rightBracket
                    + optional(colon + type)
                    + functionBody
            )
        )
        rule(functionValueParameterWithOptionalType, zeroOrMore(parameterModifier)
                + parameterWithOptionalType
                + optional(eq + expression)
        )
        rule(parameterWithOptionalType, ident + optional(colon + type))

        rule(annotation, inlineAnnotation + newline)
        rule(inlineAnnotation, annotationPrefix + annotationBody)
        rule(fileAnnotation, annotationPrefix
                + fileTerm
                + colon
                + annotationBody
                + newline
        )
        rule(annotationPrefix, at or annotationUseSiteTarget)
        rule(annotationUseSiteTarget, at + useSite + colon)
        rule(annotationBody, singleAnnotation or multiAnnotation)
        rule(multiAnnotation, leftSquareBracket
                + oneOrMore(singleAnnotation)
                + rightSquareBracket)
        rule(singleAnnotation, constructorInvocation or userType)
        rule(constructorInvocation, userType + valueArguments)
        rule(userType, simpleUserType + optional(dot + simpleUserType))
        rule(simpleUserType, ident + optional(typeArguments))
        rule(typeArguments, leftArrow
                + typeProjection
                + zeroOrMore(comma + typeProjection)
                + optional(comma)
                + rightArrow
        )
        rule(typeProjection, mult or zeroOrMore(typeProjectionModifier) + type)
        rule(useSite, fieldTerm or propertyTerm or getTerm or setTerm
                or receiverTerm or paramTerm or setparamTerm or delegateTerm)

        rule(type, optional(typeModifier) + typeSpec)
        rule(typeSpec, functionType or parenthesizedType or nullableType or typeReference)
        rule(functionType, optional(receiverType + dot)
                + functionTypeParams
                + singleRightArrow
                + type
        )
        rule(functionTypeParams, leftBracket
                + optional(functionTypeParamsBody)
                + rightBracket
        )
        rule(functionTypeParamsBody, paramOptions
                + zeroOrMore(comma + paramOptions)
                + optional(comma)
        )
        rule(paramOptions, type or param)
        rule(param, ident + colon + type)
        rule(parenthesizedType, leftBracket + type + rightBracket)
        rule(nullableType, typeReference + quest or parenthesizedType + quest)
        rule(typeReference, userType or dynamicTerm)
        rule(valueArguments, leftBracket +
                optional(valueArgument
                        + zeroOrMore(comma + valueArgument)
                        + optional(comma))
                + rightBracket
        )
        rule(valueArgument, optional(inlineAnnotation)
                + optional(ident + eq)
                + optional(mult)
                + expression
        )
        rule(expression, disjunction)
        rule(disjunction, conjunction + zeroOrMore(disjuncTerm + conjunction))
        rule(conjunction, equality + zeroOrMore(conjuncTerm + equality))
        rule(equality, comparison + zeroOrMore(equalityOperator + comparison))
        rule(comparison, genericCallLikeComparison
                + zeroOrMore(comparisonOperator + genericCallLikeComparison)
        )
        rule(genericCallLikeComparison, infixOperation + zeroOrMore(callSuffix))
        rule(infixOperation, elvisExpression + zeroOrMore(infixOperator))
        rule(elvisExpression, infixFunctionCall + zeroOrMore(elvis + infixFunctionCall))
        rule(infixFunctionCall, rangeExpression + zeroOrMore(ident + rangeExpression))
        rule(rangeExpression, additiveExpression + zeroOrMore(doubleDot + additiveExpression))
        rule(additiveExpression, multiplicativeExpression
                + zeroOrMore(additiveOperator + multiplicativeExpression)
        )
        rule(multiplicativeExpression, asExpression
                + zeroOrMore(multiplicativeOperator + asExpression)
        )
        rule(asExpression, prefixUnaryExpression + zeroOrMore(asOperator + type))
        rule(prefixUnaryExpression, zeroOrMore(unaryPrefix) + postfixUnaryExpression)
        rule(postfixUnaryExpression, primaryExpression + zeroOrMore(postfixUnarySuffix))
        rule(primaryExpression, parenthesizedExpression
                or ident
                or literalConstant
                or stringLiteral
                or callableReference
                or lambdaLiteral
                or objectLiteral
                or collectionLiteral
                or thisTerm
                or superTerm
                or ifExpression
                or whenExpression
                or tryExpression
                or jumpExpression
        )
        rule(parenthesizedExpression, leftBracket + expression + rightBracket)
        rule(literalConstant, booleanLiteral
                or integerTerm
                or hexTerm
                or binTerm
                or characterTerm
                or realLiteral
                or nullTerm
                or longLiteral
                or unsignedLiteral
        )
        rule(callableReference, optional(receiverType) + doubleColon + ident
                or optional(receiverType) + doubleColon + classTerm
        )
        rule(objectLiteral, objectTerm
                + optional(colon + delegationSpecifiers)
                + optional(classBody)
        )
        rule(collectionLiteral, leftSquareBracket
                + optional(expression + zeroOrMore(comma + expression) + optional(comma))
                + rightSquareBracket
        )
        rule(ifExpression, ifTerm
                + leftBracket
                + expression
                + rightBracket
                + controlStructureBody
                or ifTerm
                + leftBracket
                + expression
                + rightBracket
                + semicolon
                or ifTerm
                + leftBracket
                + expression
                + rightBracket
                + optional(controlStructureBody)
                + optional(semicolon)
                + elseTerm
                + controlStructureBody
                or ifTerm
                + leftBracket
                + expression
                + rightBracket
                + optional(controlStructureBody)
                + optional(semicolon)
                + elseTerm
                + semicolon
        )
        rule(whenExpression, whenTerm
                + optional(whenSubject)
                + leftCurlyBracket
                + zeroOrMore(whenEntry)
                + rightCurlyBracket
        )
        rule(tryExpression, tryTerm + block + oneOrMore(catchBlock) + optional(finallyBlock)
                or tryTerm + block + finallyBlock
        )
        rule(catchBlock, catchTerm
                + leftBracket
                + zeroOrMore(inlineAnnotation)
                + ident
                + colon
                + type
                + rightBracket
                + block
        )
        rule(finallyBlock, finallyTerm + block)
        rule(jumpExpression, throwTerm + expression
                or returnTerm + optional(at + ident) + optional(expression)
                or continueTerm
                or breakTerm
        )

        rule(booleanLiteral, trueTerm or falseTerm)
        rule(realLiteral, floatTerm or doubleTerm)
        rule(longLiteral, numberLiteral + longMarker)
        rule(unsignedLiteral, numberLiteral + unsignedMarker + optional(longMarker))
        rule(numberLiteral, integerTerm or hexTerm or binTerm)
        rule(delegationSpecifiers, annotatedDelegationSpecifier
                + zeroOrMore(comma + annotatedDelegationSpecifier)
        )
        rule(annotatedDelegationSpecifier, zeroOrMore(inlineAnnotation) + delegationSpecifier)
        rule(delegationSpecifier, constructorInvocation
                or userType
                or optional(suspendTerm) + functionType
                or explicitDelegation
        )
        rule(explicitDelegation, userType + byTerm + expression
                or functionType + byTerm + expression
        )

        rule(equalityOperator, equal or notEqual or exactEqual or exactNotEqual)
        rule(comparisonOperator, less or more or lessOrEquals or moreOrEquals)
        rule(callSuffix, optional(typeArguments) + valueArguments
                or optional(typeArguments) + optional(valueArguments) + annotatedLambda)
        rule(annotatedLambda, zeroOrMore(inlineAnnotation) + lambdaLiteral)
        rule(lambdaLiteral, leftCurlyBracket
                + optional(optional(lambdaParams) + singleRightArrow)
                + optional(statements)
                + rightCurlyBracket
        )
        rule(lambdaParams, lambdaParam
                + zeroOrMore(comma + lambdaParam)
                + optional(comma)
        )
        rule(lambdaParam, variableDeclaration or multiVariableDeclaration + optional(colon + type))

        rule(infixOperator, inOperator + elvisExpression or isOperator + type)
        rule(inOperator, inTerm or notInTerm)
        rule(isOperator, isTerm or notIsTerm)
        rule(elvis, quest + colon)
        rule(additiveOperator, plus or minus)
        rule(multiplicativeOperator, mult or div or modulo)
        rule(asOperator, asTerm or asNullableTerm)
        rule(unaryPrefix, inlineAnnotation or prefixUnaryOperator)
        rule(prefixUnaryOperator, plus or minus or doublePlus or doubleMinus or excl)
        rule(postfixUnarySuffix, typeArguments
                or callSuffix
                or indexingSuffix
                or navigationSuffix
                or postfixUnaryOperator
        )
        rule(indexingSuffix, leftSquareBracket
                + expression
                + zeroOrMore(comma + expression)
                + optional(comma)
                + rightSquareBracket
        )
        rule(navigationSuffix, memberAccessOperator + ident
                or doubleColon + classTerm
        )
        rule(memberAccessOperator, dot or safeNav)
        rule(safeNav, quest + dot)
        rule(postfixUnaryOperator, doublePlus or doubleMinus or excl + excl)
        rule(classKeyword, classTerm or optional(funTerm) + interfaceTerm)
        rule(receiverType, zeroOrMore(inlineAnnotation) + optional(suspendTerm) + parenthesizedType
                or zeroOrMore(inlineAnnotation) + optional(suspendTerm) + nullableType
                or zeroOrMore(inlineAnnotation) + optional(suspendTerm) + typeReference
        )

        rule(modifier, classModifier
                or memberModifier
                or visibilityModifier
                or functionModifier
                or propertyModifier
                or inheritanceModifier
                or parameterModifier
        )
        rule(typeModifier, suspendTerm)
        rule(classModifier, enumTerm or sealedTerm or annotationTerm or dataTerm or innerTerm)
        rule(visibilityModifier, publicTerm or privateTerm or internalTerm or protectedTerm)
        rule(memberModifier, overrideTerm or lateinitTerm)
        rule(functionModifier, tailrecTerm or operatorTerm or infixTerm
                or inlineTerm or externalTerm or suspendTerm)
        rule(propertyModifier, constTerm)
        rule(inheritanceModifier, abstractTerm or finalTerm or openTerm)
        rule(parameterModifier, varargTerm or noinlineTerm or crossinlineTerm)
        rule(typeProjectionModifier, inTerm or outTerm)
        rule(typeParameterModifier, typeProjectionModifier or reifiedTerm)

        rule(statements, semis or
                statement + zeroOrMore(semis + statement))
        rule(statement, declaration or assignment or expression or loopStatement)

        rule(classBody, leftCurlyBracket
                + zeroOrMore(classMemberDeclaration)
                + rightCurlyBracket
                + semis
        )
        rule(enumClassBody, leftCurlyBracket
                + optional(enumEntries)
                + zeroOrMore(classMemberDeclaration)
                + rightCurlyBracket
                + semis
        )
        rule(enumEntries, enumEntry + zeroOrMore(comma + enumEntry) + optional(comma))
        rule(enumEntry, zeroOrMore(modifier) + ident + optional(valueArguments) + optional(classBody))
        rule(functionBody, block or eq + expression)

        rule(controlStructureBody, block or statement)
        rule(block, leftCurlyBracket + statements + rightCurlyBracket)
        rule(whenSubject, leftBracket
                + optional(zeroOrMore(inlineAnnotation) + valTerm + variableDeclaration + eq)
                + expression
                + rightBracket
        )
        rule(whenEntry, whenCondition
                + zeroOrMore(comma + whenCondition)
                + optional(comma)
                + singleRightArrow + controlStructureBody + optional(semi)
                or elseTerm
                + singleRightArrow
                + controlStructureBody
                + optional(semi))
        rule(whenCondition, expression or rangeTest or typeTest)
        rule(rangeTest, inOperator + expression)
        rule(typeTest, isOperator + type)
        rule(primaryConstructor, optional(zeroOrMore(modifier) + constructorTerm) + classParameters)
        rule(classParameters, leftBracket
                + optional(classParameter + zeroOrMore(comma + classParameter) + optional(comma))
                + rightBracket
        )
        rule(classParameter, zeroOrMore(modifier)
                + optional(declarationKeyword)
                + ident
                + colon
                + type
                + optional(eq + expression)
        )
        rule(functionValueParameters,
            leftBracket
                    + optional(
                functionValueParameter
                        + zeroOrMore(comma + functionValueParameter)
                        + optional(comma)
                    )
                    + rightBracket
        )
        rule(functionValueParameter, zeroOrMore(parameterModifier)
                + param
                + optional(eq + expression)
        )
        rule(declarationKeyword, valTerm or varTerm)

        rule(genericVariableDeclaration, variableDeclaration or multiVariableDeclaration)
        rule(variableDeclaration, zeroOrMore(inlineAnnotation) + ident + optional(colon + type))
        rule(multiVariableDeclaration, leftBracket
                + variableDeclaration
                + zeroOrMore(comma + variableDeclaration)
                + optional(comma)
                + rightBracket
        )

        rule(assignment, directlyAssignableExpression + eq + expression
                or assignableExpression + assignmentAndOperator + expression
        )
        rule(assignmentAndOperator, plusAssign or minusAssign or multAssign
                or divAssign or moduloAssign)
        rule(assignableExpression, prefixUnaryExpression or parenthesizedAssignableExpression)
        rule(parenthesizedAssignableExpression, leftBracket + assignableExpression + rightBracket)
        rule(directlyAssignableExpression, postfixUnaryExpression + assignableSuffix
                or ident
                or parenthesizedDirectlyAssignableExpression
        )
        rule(parenthesizedDirectlyAssignableExpression, leftBracket + directlyAssignableExpression + rightBracket)
        rule(assignableSuffix, typeArguments or indexingSuffix or navigationSuffix)

        rule(loopStatement, forStatement or whileStatement or doWhileStatement)
        rule(forStatement, forTerm
                + leftBracket
                + zeroOrMore(inlineAnnotation)
                + genericVariableDeclaration
                + inTerm
                + expression
                + rightBracket
                + optional(controlStructureBody)
        )
        rule(whileStatement, whileTerm
                + leftBracket
                + expression
                + rightBracket
                + optional(controlStructureBody)
        )
        rule(doWhileStatement, doTerm
                + optional(controlStructureBody)
                + whileTerm
                + leftBracket
                + expression
                + rightBracket
        )

        rule(chainIdent, ident + optional(dot + chainIdent))
        rule(importEnding, dot + mult or asTerm + ident)
        rule(typeParams, leftArrow
                + typeParam
                + zeroOrMore(comma + typeParams)
                + optional(comma)
                + rightArrow
        )
        rule(typeParam, zeroOrMore(typeParameterModifier) + ident + optional(colon + type))

        rule(ident, identBasic or identStr)
        rule(semi, semiInternal)
        rule(semis, semiInternal)
        rule(semiInternal, newline or semicolon or end)
    }
}

class StringLexeme(identBasic: TerminalLexeme,
                   identStr: TerminalLexeme, skipList: List<Skip>): TerminalLexeme("string", skipList) {
    private val expressionMatcher = ExpressionMatcher()
    private val identBasicMatcher = TerminalMatcher(identBasic)
    private val identStrMatcher = TerminalMatcher(identStr)

    fun setExpressionLanguage(language: Language) {
        expressionMatcher.compiler = Compiler(language)
    }

    override fun token(code: Code): StringToken? {
        val trimmed = skip(code)
        var text = trimmed.current()
        val matchers: Array<Matcher>
        val quot: String
        val postprocessor: (String) -> Unit
        val elements = mutableListOf<Element>()
        if (text.startsWith("\"\"\"")) {
            matchers = arrayOf(
                expressionMatcher,
                identBasicMatcher,
                identStrMatcher,
                RegexMatcher(Regex("""\$""")),
                RegexMatcher(Regex("""\\\$""")),
                RegexMatcher(Regex("""([^$]+?(?=\"\"\"))|([^$]+?(?=\$))""")),
            )
            quot = "\"\"\""
            postprocessor = {
                val quots = it.takeWhile { it == '"' }
                it.drop(quots.length)
                if (quots.isNotEmpty()) {
                    elements.add(TextElement(quots))
                }
            }
        } else if (text.startsWith("\"")) {
            matchers = arrayOf(
                expressionMatcher,
                identBasicMatcher,
                identStrMatcher,
                RegexMatcher(Regex("""\$""")),
                RegexMatcher(Regex("""\\[tbrn'"\\$]""")),
                RegexMatcher(Regex("\\\\(u[0-9A-Fa-f]{4})")),
                RegexMatcher(Regex("""[^\\"]+""")),
                RegexMatcher(Regex("""\""""))
            )
            quot = "\""
            postprocessor = {}
        } else {
            return null
        }
        text = text.drop(quot.length)
        var success = false
        while (!text.startsWith(quot)) {
            success = false
            for (matcher in matchers ) {
                val result = matcher.match(text)
                if (result != null){
                    elements.add(result)
                    text = text.drop(result.content.length)
                    success = true
                    break
                }
            }
            if (!success) {
                throw RuntimeException("Unable to parse String. Content: $text")
            }
        }
        text = text.drop(quot.length)
        postprocessor(text)
        val content = elements.joinToString("") { it.content }
        return StringToken(quot + content + quot, trimmed.position(), setOf("string"), elements)
    }

    override fun match(token: Token) = token.tags.contains("string")

    class StringToken(content: String, position: Position, tags: Set<String>, elements: List<Element>):
        Token(content, position, tags)

    interface Matcher {
        fun match(input: String): Element?
    }

    class RegexMatcher(private val regex: Regex): Matcher {
        override fun match(input: String): Element? {
            return regex.find(input)?.let { if (it.range.first == 0) TextElement(it.value) else null }
        }
    }

    class TerminalMatcher(private val term: TerminalLexeme): Matcher {
        override fun match(input: String): ParsableElement? {
            return input.let { if (it[0] == '$') it.drop(1) else null }
                ?.let {
                    val expInput = input.drop(1)
                    term.token(Code(expInput))?.let {
                        if (expInput.startsWith(it.content)) {
                            ParsableElement(
                                Compiler.AST(term),
                                "$" + it.content
                            )
                        } else {
                            null
                        }
                    }
                }
        }
    }

    class ExpressionMatcher: Matcher {
        var compiler: Compiler? = null
        override fun match(input: String): ParsableElement? {
            if (!input.startsWith("\${")) {
                return null
            }
            var counter = 0
            val exp = input.takeWhile {
                if (it == '{') {
                    counter++
                } else if (it == '}') {
                    counter--
                }
                counter > 0 || it != '}'
            }.drop(2)

            if (compiler == null) {
                throw RuntimeException("Unable to parse String. Expression compiler is null.")
            }
            return compiler!!.process(exp)?.let { ParsableElement(it, "\${$exp}") }
        }
    }

    sealed interface Element {
        val content: String
    }
    class TextElement(override val content: String): Element
    class ParsableElement(val tree: Compiler.AST, override val content: String): Element
}