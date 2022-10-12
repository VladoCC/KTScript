import kotlin.math.min
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

enum class Task {
    COMPILE, AST_DOT
}

fun compile(code: String, task: Task) {
    val parser = parser {
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
        val doubleTerm = regTerm("([0-9]([0-9_]*[0-9])?)?.[0-9]([0-9_]*[0-9])?([eE][+-]?([0-9]([0-9_]*[0-9])?))?|([0-9]([0-9_]*[0-9])?)([eE][+-]?([0-9]([0-9_]*[0-9])?))", "double")
        val integerTerm = regTerm("([1-9][\\d_]*\\d)|(\\d)", "integer")
        val characterTerm = regTerm("'((\\\\((u[0-9A-Fa-f]{4})|t|b|r|n|\'|\"|\\|\$))|([^\\n]|[^\\r]|[^\\\\]|[^\']))'","character")
        val longMarker = regTerm("[lL]", "longMarker")
        val unsignedMarker = regTerm("[uU]", "unsignedMarker")
        /*val lineStrEscapedChar = regTerm(Regex("((\\\\((u[0-9A-Fa-f]{4})|t|b|r|n|'|\"|\\|\$)))"), "escapedIdentifier")*/
        val identStr = regTerm("`[^\\r\\n`]+`", "identStr")
        /*val lineStrText = regTerm("[^\\\"\$]+|\$", "lineStrText")
        val multiLineStrText = regTerm("[^\"\$]+|\$", "multiLineStrText")*/

        val declarationKeyword = nonTerm("declarationKeyword")

        val stringLiteral = registerTerm(StringLexeme(
            subgrammar(expression),
            expression,
            identBasic,
            identStr)
        )

        val constructorTerm = term("constructor")
        val crossinlineTerm = term("crossinline")
        val annotationTerm = term("annotation")
        val companionTerm = term("companion")
        val interfaceTerm = term("interface")
        val typealiasTerm = term("typealias")
        val protectedTerm = term("protected")
        val continueTerm = term("continue")
        val overrideTerm = term("override")
        val lateinitTerm = term("lateinit")
        val internalTerm = term("internal")
        val operatorTerm = term("operator")
        val externalTerm = term("external")
        val abstractTerm = term("abstract")
        val noinlineTerm = term("noinline")
        val dynamicTerm = term("dynamic")
        val packageTerm = term("package")
        val finallyTerm = term("finally")
        val suspendTerm = term("suspend")
        val privateTerm = term("private")
        val tailrecTerm = term("tailrec")
        val reifiedTerm = term("reified")
        val objectTerm = term("object")
        val returnTerm = term("return")
        val publicTerm = term("public")
        val import = term("import")
        val sealedTerm = term("sealed")
        val inlineTerm = term("inline")
        val varargTerm = term("vararg")
        val classTerm = term("class")
        val falseTerm = term("false")
        val superTerm = term("super")
        val catchTerm = term("catch")
        val throwTerm = term("throw")
        val breakTerm = term("break")
        val whileTerm = term("while")
        val innerTerm = term("inner")
        val infixTerm = term("infix")
        val constTerm = term("const")
        val finalTerm = term("final")
        val trueTerm = term("true")
        val fileTerm = term("file")
        val nullTerm = term("null")
        val thisTerm = term("this")
        val elseTerm = term("else")
        val whenTerm = term("when")
        val initTerm = term("init")
        val enumTerm = term("enum")
        val dataTerm = term("data")
        val openTerm = term("open")
        val asNullableTerm = term("as?")
        val notInTerm = term("!in")
        val notIsTerm = term("!is")
        val valTerm = term("val")
        val varTerm = term("var")
        val funTerm = term("fun")
        val outTerm = term("out")
        val tryTerm = term("try")
        val forTerm = term("for")
        val getTerm = term("get")
        val setTerm = term("set")
        val doTerm = term("do")
        val asTerm = term("as")
        val inTerm = term("in")
        val isTerm = term("is")
        val byTerm = term("by")
        val ifTerm = term("if")

        val tripleQuotation = term("\"\"\"")
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
        rule(importHeader, import + chainIdent + optional(importEnding) + semi)
        rule(topLevelObject, declaration + optional(semis))
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
                + comma
                + rightArrow
        )
        rule(typeProjection, mult or zeroOrMore(typeProjectionModifier) + type)

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
                or returnTerm + optional(at + ident) + optional(ident)
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
        rule(callSuffix, optional(typeArguments) + valueArgument
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
                statement + zeroOrMore(semis + statement) + optional(semis))
        rule(statement, declaration or assignment or expression or loopStatement)

        rule(classBody, leftCurlyBracket
                + zeroOrMore(classMemberDeclaration + optional(semis))
                + rightCurlyBracket
        )
        rule(enumClassBody, leftCurlyBracket
                + optional(enumEntries)
                + zeroOrMore(classMemberDeclaration + optional(semis))
                + rightCurlyBracket
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
                + rightArrow + controlStructureBody + optional(semi)
                or elseTerm
                + rightArrow
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
        rule(semi, semiInternal + optional(newline))
        rule(semis, oneOrMore(semiInternal))
        rule(semiInternal, newline or semicolon or eofTerm)
    }
    when (task) {
        Task.COMPILE -> parser.process(code)
        Task.AST_DOT -> {
            save(parser.astDotString(code), "ast.dot")
        }
    }
}

class Code(private val text: String) {
    private val prevText = mutableListOf<String>()
    private val spaceRegex = Regex("[^\\S\\r\\n]+")
    var consumed = 0
        private set
    var position = Position()
        private set(value) {
            prevText.add(current)
            field = value
            current = text.lines().drop(field.line).joinToString("\n").drop(field.column)
        }
    var current = text
        private set
    fun consume(text: String) {
        if (!current.startsWith(text)) {
            throw IllegalArgumentException("Unable to consume, not part of the code")
        }
        val lines = text.lines()
        if (lines.size > 1) {
            position = position.copy(
                line = position.line + lines.size -1,
                column = lines.last().length
            )
        } else {
            position = position.copy(column = position.column + text.length)
        }
        consumed++
    }
    fun consumeSpaces() {
        val result = spaceRegex.find(current)
        if (result != null && result.range.first == 0) {
            consume(result.value)
        }
    }
    fun consumeChar() {
        if (!isConsumed()) {
            consume(current.first().toString())
        }
    }
    fun isConsumed() = current.isBlank()

    fun at(index: Int): String {
        return if (index >= prevText.size) {
            current
        } else {
            val prev = prevText[index]
            prev
        }
    }
    fun part(from: Position, to: Position): String {
        val lines = text.lines().drop(from.line).take(to.line - from.line + 1)
        return lines.joinToString("\n").drop(from.column).dropLast(lines.last().length - to.column + 1)
    }
}

fun parser(init: KTParser.() -> Unit): KTParser {
    with(KTParser()) {
        init()
        return this
    }
}

class StringLexeme(expressionGrammar: Grammar,
                   expressionLexeme: Parser.Lexeme,
                   identBasic: Parser.TerminalLexeme,
                   identStr: Parser.TerminalLexeme): Parser.TerminalLexeme("string") {
    private val expressionMatcher = ExpressionMatcher(expressionGrammar, expressionLexeme)
    private val identBasicMatcher = TerminalMatcher(identBasic)
    private val identStrMatcher = TerminalMatcher(identStr)

    override fun token(code: Code): StringToken? {
        var text = code.current
        val matchers: Array<Matcher>
        val quot: String
        val elements = mutableListOf<Element>()
        if (text.startsWith("\"\"\"")) {
            matchers = arrayOf(
                expressionMatcher,
                identBasicMatcher,
                identStrMatcher,
                RegexMatcher(Regex("""\$""")),
                RegexMatcher(Regex("""([^$]+(?=\"\"\"))|([^$]+(?=\$))""")),
            )
            quot = "\"\"\""
        } else if (text.startsWith("\"")) {
            matchers = arrayOf(
                expressionMatcher,
                identBasicMatcher,
                identStrMatcher,
                RegexMatcher(Regex("""\$""")),
                RegexMatcher(Regex("""\\[tbrn'"\\$]""")),
                RegexMatcher(Regex("\\\\(u[0-9A-Fa-f]{4})")),
                RegexMatcher(Regex("""[^\\"]+""")),
            )
            quot = "\""
        } else {
            return null
        }
        text = text.drop(quot.length)
        var success = false
        while (!text.startsWith(quot)) {
            success = false
            matchers.forEach { matcher ->
                val result = matcher.match(text)
                if (result != null){
                    elements.add(result)
                    text = text.drop(result.content.length)
                    success = true
                    return@forEach
                }
            }
            if (!success) {
                throw RuntimeException("Unable to parse String. Content: $text")
            }
        }
        text = text.drop(quot.length)
        val content = elements.joinToString("") { it.content }
        return StringToken(quot + content + quot, code.position, setOf("string"), code.consumed, elements)
    }

    override fun match(token: Parser.Token) = token.tags.contains("string")

    class StringToken(content: String, position: Position, tags: Set<String>, index: Int, elements: List<Element>):
        Parser.Token(content, position, tags, index)

    interface Matcher {
        fun match(input: String): Element?
    }

    class RegexMatcher(private val regex: Regex): Matcher {
        override fun match(input: String): Element? {
            return regex.find(input)?.let { if (it.range.first == 0) TextElement(it.value) else null }
        }
    }

    class TerminalMatcher(private val term: Parser.TerminalLexeme): Matcher {
        override fun match(input: String): ParsableElement? {
            return input.let { if (it[0] == '$') it.drop(1) else null }
                ?.let {
                    val expInput = input.drop(1)
                    term.token(Code(expInput))?.let {
                        if (expInput.startsWith(it.content)) {
                            ParsableElement(
                                Parser.AST(term, null, 0, it),
                                "$" + it.content
                            )
                        } else {
                            null
                        }
                    }
                }
        }
    }

    class ExpressionMatcher(private val grammar: Grammar,
                            private val expression: Parser.Lexeme): Matcher {
        var parser: Parser? = null
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

            if (parser == null) {
                parser = Parser(grammar, expression)
            }
            return parser!!.process(exp)?.let { ParsableElement(it, "\${$exp}") }
        }
    }

    sealed interface Element {
        val content: String
    }
    class TextElement(override val content: String): Element
    class ParsableElement(val tree: Parser.AST, override val content: String): Element {

        fun produce(): String {
            TODO()
        }
    }
}

class KTParser: Parser() {
    val eofTerm = object: TerminalLexeme("EOF") {
        override fun token(code: Code) = null
        override fun match(token: Token) = token.tags.contains("EOF")
    }

    override fun tokenize(code: Code): List<Token> {
        val result = super.tokenize(code)
        return result + Token("", code.position, setOf("EOF"), code.consumed, "EOF")
    }
}

// TODO rename correctly
open class Parser(parserGrammar: Grammar, val root: Lexeme, ) {
    var grammar = parserGrammar.toMutableList()

    private lateinit var table: Table
    private val terminals = mutableSetOf<TerminalLexeme>()
    private val forest = mutableMapOf<State, MutableList<Pair<Tree<State>, Int>>>()

    val newline = regTerm("\\n\\s*", "newline")

    constructor(): this(emptyList(), Lexeme())

    // TODO not working for lexems added later
    init {
        val terms = mutableSetOf<TerminalLexeme>()
        grammar.forEach {
            it.right.forEach {
                if (it is TerminalLexeme) {
                    terms.add(it)
                }
            }
        }
        terminals.addAll(terms.sorted())
    }

    fun process(code: String): AST? {
        val tokens = tokenize(Code(code))
        if (tokens.isNotEmpty()) {
            return parse(tokens)
        }
        return null
    }

    fun astDotString(code: String): String {
        val tokens = tokenize(Code(code))
        if (tokens.isNotEmpty()) {
            return parse(tokens)?.dotGraph()?: ""
        }
        return ""
    }

    // returns lazy grammar to make sure that subgrammar calls made
    // before all the rules are described, would result in a correct subgrammar
    fun subgrammar(root: Lexeme): Grammar = LazyGrammar {
        val ruleSet = mutableMapOf<Lexeme, MutableList<Rule>>()
        grammar.forEach {
            if (!ruleSet.containsKey(it.left)) {
                ruleSet[it.left] = mutableListOf()
            }
            ruleSet[it.left]!!.add(it)
        }
        val current = mutableListOf<Lexeme>()
        current.add(root)
        val result = mutableMapOf<Lexeme, List<Rule>>()
        while (current.isNotEmpty()) {
            val left = current[0]
            ruleSet[left]?.let { rules ->
                result[left] = rules
                rules.forEach {rule ->
                    rule.right.forEach {
                        // TODO it !in current is O(N) and can be optimized
                        if (it !in result && it !in current) {
                            current.add(it)
                        }
                    }
                }
            }
            current.removeAt(0)
        }
        return@LazyGrammar result.values.flatten()
    }

    protected open fun tokenize(code: Code): List<Token> {
        val tokens = mutableListOf<Token>()
        var error = false
        var unexpectedPosition: Position? = null
        while (!code.isConsumed()){
            var found = false
            for (lex in terminals) {
                val token = lex.token(code)
                if (token != null) {
                    code.consume(token.content)
                    code.consumeSpaces()
                    tokens.add(token)
                    println("Created token `${token.content}` for $lex at ${token.position}")
                    found = true
                    break
                }
            }
            if (!found) {
                if (unexpectedPosition == null) {
                    unexpectedPosition = code.position
                    error = true
                }
                code.consumeChar()
            } else if (unexpectedPosition != null) {
                val unexpectedText = code.part(unexpectedPosition, code.position)
                println("Unresolved token: `$unexpectedText` at $unexpectedPosition")
                unexpectedPosition = null
            }
        }
        return if (error) emptyList() else tokens
    }

    fun parse(tokens: List<Token>): AST? {
        table = Table(root, tokens.size)
        table.indices.forEach { col ->
            println("Col: $col")
            val iter = table[col].iterator()
            while (iter.hasNext()) {
                val state = iter.next()
                if (state.isComplete()) {
                    completer(state, col)
                } else {
                    if (state.next() is TerminalLexeme) {
                        if (tokens.size > col) {
                            scanner(state, col, tokens[col])
                        }
                    } else {
                        predictor(state, col)
                    }
                }
            }
        }
        table.last().firstOrNull {
            it.rule.right.size == 1 && it.rule.right[0] == root && it.isComplete()
        }.let { state ->
            if (state == null) {
                return null
            }
            val tree = state.tree
            val ast = tree?.toAST(tokens.toMutableList())
            ast?.print()
            return ast
        }
    }

    private fun Tree<State>.toAST(tokens: MutableList<Token>, parent: AST? = null): AST {
        return AST(value.rule.left, parent, children.size).also {
            children.forEachIndexed { index, tree ->
                it.children[index] = if (tree == null) {
                    val leaf = AST(value.rule.right[index], it, 0, tokens.firstOrNull())
                    if (tokens.isNotEmpty()) {
                        tokens.removeAt(0)
                    }
                    leaf
                } else {
                    tree.toAST(tokens, it)
                }
            }
        }
    }

    private fun completer(state: State, col: Int) {
        if (!state.isComplete()) {
            throw IllegalArgumentException()
        }
        table[state.origin].mapIndexed { index: Int, state: State ->
            index to state
        }.filter {
            it.second.next()?.equals(state.rule.left)?: false
        }.forEach { pair ->
            val (index, it) = pair
            val new = it.copy(dot = it.dot + 1, parent = it)
            new.tree = it.tree.copy()
            new.tree.children[it.dot] = state.tree
            println("Complete[$col][${table[col].size}]\n" +
                    "  From [${table[col].indexOf(state)}] ${state.rule}\n" +
                    "  And state[${state.origin}][$index] ${it.rule}")
            table[col].add(new)
            new.connect(forest[state]!!.first { it.second == col }, it.dot)
        }
    }
    private fun scanner(state: State, col: Int, token: Token) {
        val next = state.next()
        if (next !is TerminalLexeme) {
            throw IllegalArgumentException()
        }
        val result = next.match(token)
        println("Match ${token.content} to $next, res: $result")
        if (result && col + 1 < table.size) {
            val new = state.copy(dot = state.dot + 1)
            new.tree = state.tree.copy()
            println("Scan[${col + 1}][${table[col+1].size}]\n" +
                    "  From state[$col][${table[col].indexOf(state)}] ${state.rule}")
            println(new)
            table[col + 1].add(new)
            createTree(new, col + 1)
        }
    }
    private fun predictor(state: State, col: Int) {
        grammar.mapIndexed { index, rule -> index to rule }
            .filter { it.second.left == state.next() }
            .forEach { pair ->
                val (index, it) = pair
                val new = State(it, col)
                println("Predicted[$col][${table[col].size}]\n" +
                        "  For [${table[col].indexOf(state)}] ${state.rule}\n" +
                        "  From [$index] $it")
                println(new)
                table[col].add(new)
            }
    }
    private fun State.connect(child: Pair<Tree<State>, Int>, pos: Int) {
        val state = this
        val prevState = state.copy(dot = state.dot - 1)
        val parent = if (forest.containsKey(prevState)) {
            if (!forest.containsKey(state)) {
                forest[state] = mutableListOf()
            }

            val prev = forest[prevState]!!.first { it.second < child.second }
            val new = Tree(state, state.rule.right.size)
            prev.first.children.forEachIndexed { index, tree ->
                new.children[index] = tree
            }
            val newPair = new to child.second
            forest[state]!!.add(newPair)
            new
        } else {
            createTree(state, child.second)
        }
        parent.children[pos] = child.first
    }
    private fun createTree(state: State, token: Int): Tree<State> {
        if (!forest.containsKey(state)) {
            forest[state] = mutableListOf()
        }

        val tree = Tree(state, state.rule.right.size)
        forest[state]!!.add(tree to token)
        return tree
    }

    fun nonTerm(desc: String = "") = Lexeme(desc)
    fun term(content: String, desc: String? = null): TerminalLexeme {
        val token = ExactLexeme(content, desc?: content)
        terminals.add(token)
        return token
    }
    fun regTerm(regex: Regex, name: String): TerminalLexeme {
        val token = RegexLexeme(regex, name)
        terminals.add(token)
        return token
    }
    fun regTerm(regex: String, name: String): TerminalLexeme = regTerm(Regex(regex), name)
    fun customTerm(token: (Code) -> Token?, match: (Token) -> Boolean): CustomLexeme {
        val lex = CustomLexeme(token, match)
        terminals.add(lex)
        return lex
    }
    fun registerTerm(lex: TerminalLexeme) = terminals.add(lex).let { lex }
    fun optional(product: OptionalProduct) = product.produce().toTypedArray()
    fun optional(product: Product): Optional = arrayOf(product)
    fun optional(lexeme: Lexeme): Optional = optional(Product(lexeme))
    fun oneOrMore(product: Product): Lexeme {
        val tokens = product.getTokens()
        val name = "(${tokens.joinToString(" ") { it.desc }})+"
        val list = nonTerm(name)
        rule(list, product + optional(list))
        return list
    }
    fun oneOrMore(lexeme: Lexeme) = oneOrMore(Product(lexeme))
    fun oneOrMore(product: OptionalProduct): Lexeme {
        val prods = product.produce().map { it.getTokens() }.joinToString(" | ") { it.joinToString(" ") { it.desc } }
        val name = "(${prods})+"
        val list = nonTerm(name)
        product.produce().forEach {
            rule(list, it + optional(list))
        }
        return list
    }
    fun zeroOrMore(product: Product) = optional(oneOrMore(product))
    fun zeroOrMore(lexeme: Lexeme) = zeroOrMore(Product(lexeme))
    fun zeroOrMore(product: OptionalProduct) = optional(oneOrMore(product))

    fun rule(left: Lexeme, right: ProductProducer): List<Rule> {
        return right.produce().map {
            val rule = Rule(left, it)
            grammar.add(rule)
            rule
        }
    }
    fun rule(left: Lexeme, right: Lexeme): Rule {
        return rule(left, Product(right)).first()
    }

    open class Lexeme(val desc: String = ""): Comparable<Lexeme> {
        protected val id = getCount()

        operator fun plus(lexeme: Lexeme): Product {
            return Product(this).also { it + lexeme }
        }
        operator fun plus(optional: Optional): OptionalProduct {
            return OptionalProduct(Product(this), optional)
        }
        infix fun or(product: Product): ProductList {
            return Product(this) or product
        }
        infix fun or(lexeme: Lexeme): ProductList {
            return Product(this) or lexeme
        }
        infix fun or(product: OptionalProduct): ProductList {
            return Product(this) or product
        }

        override fun compareTo(other: Lexeme): Int {
            if (this !is TerminalLexeme) {
                return 1
            }
            if (other !is TerminalLexeme) {
                return -1
            }
            if (this is RegexLexeme) {
                return -1
            }
            if (other is RegexLexeme) {
                return 1
            }
            if (this is CustomLexeme) {
                return -1
            }
            if (other is CustomLexeme) {
                return 1
            }
            return (this as ExactLexeme).content.length - (other as ExactLexeme).content.length
        }

        override fun toString(): String {
            return "Lexeme(id=$id, desc='$desc')"
        }

    }
    abstract class TerminalLexeme(desc: String): Lexeme(desc) {
        abstract fun token(code: Code): Token?
        abstract fun match(token: Token): Boolean
    }
    class ExactLexeme(val content: String, desc: String = content): TerminalLexeme(desc) {
        override fun token(code: Code): Token? {
            return if (code.current.startsWith(content)) {
                Token(content, code.position, emptySet(), code.consumed, desc)
            } else {
                null
            }
        }

        override fun match(token: Token): Boolean {
            return token.content == content
        }

        override fun toString(): String {
            return "ExactLexeme(content='$desc', id='$id')"
        }
    }
    class RegexLexeme(private val regex: Regex, private val name: String): TerminalLexeme(name) {
        override fun token(code: Code): Token? {
            val result = regex.find(code.current)
            if (result == null || result.range.first != 0) {
                return null
            }
            return RegexToken(result.value, code.position, setOf(name, "regex"), code.consumed, result)
        }

        override fun match(token: Token): Boolean {
            return token.tags.contains(name)
        }

        override fun toString(): String {
            return "RegexLexeme(regex='$regex', id='$id', name='$name')"
        }
    }
    class CustomLexeme(val tokenLambda: (Code) -> Token?,
                       val matchLambda: (Token) -> Boolean): TerminalLexeme("custom") {
        override fun token(code: Code): Token? = tokenLambda(code)
        override fun match(token: Token): Boolean = matchLambda(token)
    }

    open class Token(val content: String, val position: Position, val tags: Set<String>, val index: Int, private val desc: String = content) {
        override fun toString(): String {
            val descStr = desc.map { if (it.code == 10) "\\n" else it.toString() }.joinToString("")
            return "Token(content='$descStr', position=$position, tags=$tags)"
        }
    }
    class RegexToken(content: String, position: Position, tags: Set<String>, index: Int, val result: MatchResult, desc: String = content):
        Token(content, position, tags, index, desc)

    interface ProductProducer {
        fun produce(): List<Product>
    }

    class Product(vararg lexemes: Lexeme): List<Lexeme>, ProductProducer {
        private val lexemes = mutableListOf<Lexeme>()
        override val size
            get() = lexemes.size

        init {
            if (lexemes.isEmpty()) {
                throw IllegalArgumentException("Product must contain at least one lexem")
            }
            this.lexemes.addAll(lexemes)
        }

        override fun containsAll(elements: Collection<Lexeme>): Boolean {
            return lexemes.containsAll(elements)
        }

        override fun contains(element: Lexeme): Boolean {
            return lexemes.contains(element)
        }

        operator fun plus(lexeme: Lexeme): Product {
            add(lexeme)
            return this
        }
        operator fun plus(product: Product): Product {
            return this.also {
                it.lexemes.addAll(product.lexemes)
            }
        }
        operator fun plus(optional: Optional): OptionalProduct {
            return OptionalProduct(this, optional)
        }
        override operator fun get(index: Int) = lexemes[index]
        override fun isEmpty(): Boolean {
            return lexemes.isEmpty()
        }

        override fun iterator(): Iterator<Lexeme> {
            return lexemes.iterator()
        }

        override fun listIterator(): ListIterator<Lexeme> {
            return lexemes.listIterator()
        }

        override fun listIterator(index: Int): ListIterator<Lexeme> {
            return lexemes.listIterator(index)
        }

        override fun subList(fromIndex: Int, toIndex: Int): List<Lexeme> {
            return lexemes.subList(fromIndex, toIndex)
        }

        override fun lastIndexOf(element: Lexeme): Int {
            return lexemes.lastIndexOf(element)
        }

        override fun indexOf(element: Lexeme): Int {
            return lexemes.indexOf(element)
        }

        private fun add(lexeme: Lexeme) {
            lexemes.add(lexeme)
        }
        fun getTokens(): List<Lexeme> = lexemes
        infix fun or(product: Product): ProductList {
            return ProductList(listOf(this, product))
        }
        infix fun or(lexeme: Lexeme): ProductList {
            return ProductList(listOf(this, Product(lexeme)))
        }
        infix fun or(product: OptionalProduct): ProductList {
            return ProductList(listOf(this) + product.produce())
        }

        override fun produce(): List<Product> {
            return listOf(this)
        }

        override fun toString(): String {
            return "Product(tokens=$lexemes)"
        }
    }

    operator fun Optional.plus(lexeme: Lexeme): OptionalProduct {
        return OptionalProduct(null, this) + lexeme
    }
    class OptionalProduct(product: Product?, optional: Optional): ProductProducer {
        private var products = mutableListOf<List<Lexeme>>()

        init {
            if (product != null) {
                products.add(product.getTokens())
            } else {
                products.add(emptyList())
            }
            this + optional
        }

        operator fun plus(lexeme: Lexeme): OptionalProduct {
            products = products.map { it + lexeme } as MutableList<List<Lexeme>>
            return this
        }

        operator fun plus(optional: Optional): OptionalProduct {
            optional.forEach {
                products.indices.forEach {i ->
                    products.add(products[i] + it.getTokens())
                }
            }
            return this
        }

        override fun produce(): List<Product> {
            return products.map { Product(*it.toTypedArray()) }
        }

        infix fun or(lexeme: Lexeme): ProductList {
            return this or Product(lexeme)
        }

        infix fun or(product: ProductProducer): ProductList {
            return ProductList(produce() + product.produce())
        }
    }
    class ProductList(private val products: List<Product>): List<Product> by products, ProductProducer{
        infix fun or(product: Product): ProductList {
            return ProductList(products.toMutableList().also { it.add(product) })
        }

        infix fun or(lexeme: Lexeme): ProductList {
            return this or Product(lexeme)
        }

        infix fun or(product: OptionalProduct): ProductList {
            return ProductList(products + product.produce())
        }

        override fun produce(): List<Product> {
            return products
        }
    }

    data class Rule(val left: Lexeme, val right: Product): Comparable<Rule> {
        override fun compareTo(other: Rule): Int {
            val left = this.left.compareTo(other.left)
            if (left != 0) {
                return left
            }
            (0 until min(right.size, other.right.size)).forEach {
                val diff = right[it].compareTo(other.right[it])
                if (diff != 0) {
                    return diff
                }
            }
            return right.size - other.right.size
        }
    }

    class Table(root: Lexeme, size: Int) {
        private val columns = mutableListOf<Column>()
        init {
            val rootCol: Column = AppendableSet()
            val rootState = State(Rule(Lexeme(), Product(root)), 0)
            rootCol.add(rootState)
            columns.add(rootCol)
            (1..size).forEach {
                columns.add(AppendableSet())
            }
        }
        operator fun get(index: Int): Column {
            (0..(index - columns.size)).forEach { _ ->
                columns.add(AppendableSet())
            }
            return columns[index]
        }
        fun last() = columns.last()
        val indices = columns.indices
        val size = columns.size
    }
    data class State(
        val rule: Rule,
        var origin: Int,
        var dot: Int = 0,
        val parent: State? = null,
    ) {
        var tree: Tree<State> = Tree(this, rule.right.size)

        fun isComplete() = dot == rule.right.size
        fun next(): Lexeme? {
            return if (!isComplete()) rule.right[dot] else null
        }
        fun prev(): Lexeme? {
            return if (dot > 0) rule.right[dot - 1] else null
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) return true

            other as State

            if (rule != other.rule) return false
            if (dot != other.dot) return false
            if (origin != other.origin) return false

            return true
        }

        override fun hashCode(): Int {
            var result = rule.hashCode()
            result = 31 * result + dot.hashCode()
            return result
        }
    }

    open class Tree<T>(val value: T, size: Int): Iterable<T> {
        val children = arrayOfNulls<Tree<T>>(size)
        override fun iterator(): Iterator<T> {
            return iterator(Order.DepthFirst)
        }
        fun iterator(order: Order): Iterator<T> {
            return TreeIterator(this, order)
        }

        fun copy(): Tree<T> {
            return Tree(value, children.size).also {
                it.children.indices.forEach { index ->
                    it.children[index] = children[index]
                }
            }
        }

        enum class Order {
            BreadthFirst, DepthFirst
        }
        private class TreeIterator<T>(private val tree: Tree<T>, private val order: Order): Iterator<T> {
            val nodeList = mutableListOf<T>()
            val listIterator: Iterator<T>

            init {
                val toProcess = mutableListOf<Tree<T>>(tree)
                while (toProcess.isNotEmpty()) {
                    val cur = toProcess.first()
                    toProcess.removeAt(0)
                    nodeList.add(cur.value)
                    cur.children.forEach {
                        if (it == null) {
                            return@forEach
                        }
                        when (order) {
                            Order.BreadthFirst -> toProcess.add(it)
                            Order.DepthFirst -> toProcess.add(0, it)
                        }
                    }
                }
                listIterator = nodeList.iterator()
            }

            override fun hasNext(): Boolean {
                return listIterator.hasNext()
            }

            override fun next(): T {
                return listIterator.next()
            }
        }
    }

    class AST(value: Lexeme, val parent: AST?, size: Int, val token: Token? = null): Tree<Lexeme>(value, size) {
        fun print(depth: Int = 0) {
            (0 until depth).forEach { print("|") }
            print("|-")
            println(value)
            children.forEach {
                if (it !is AST) {
                    throw IllegalArgumentException("AST children must be AST too")
                }
                it.print(depth + 1)
                if (it.token != null) {
                    (0 .. depth + 1).forEach { print("|") }
                    println("|-${it.token}")
                }
            }
        }
        fun dotGraph(): String {
            return DotPrinter(this).print()
        }

        private class DotPrinter(private val tree: AST) {
            private val countMap = mutableMapOf<Lexeme, Int>()
            private val usedMap = mutableMapOf<Lexeme, Int>()
            private val nameMap = mutableMapOf<AST, String>()

            init {
                tree.forEach {
                    if (countMap.containsKey(it)) {
                        countMap[it] = countMap[it]!! + 1
                        usedMap[it] = 0
                    } else {
                        countMap[it] = 1
                    }
                }
            }

            private fun name(tree: AST): String {
                return if (nameMap.containsKey(tree)) {
                    nameMap[tree]!!
                } else {
                    val res = tree.value.toString() +
                            if (countMap[tree.value]!! > 1) {
                                usedMap[tree.value] = usedMap[tree.value]!! + 1
                                " (${usedMap[tree.value]})"
                            } else ""
                    nameMap[tree] = res
                    res
                }
            }

            private fun dotConnections(tree: AST): List<String> {
                val left = name(tree)
                val token = tree.token.toString().replace(""""""", """""")
                val connections = tree.children.filterNotNull().map { """"$left" -> "${name(it as AST)}";""" }.toMutableList()
                if (tree.token != null) {
                    connections += """"$left" -> "${token} (${tree.token.index})";"""
                }
                val childConnections = tree.children.flatMap { dotConnections(it as AST) }
                return connections + childConnections
            }

            fun print(): String {
                val connections = dotConnections(tree)
                    .joinToString("\n    ")
                return "digraph {\n    $connections\n}"
            }
        }
    }
}

typealias Optional = Array<Parser.Product>
typealias Column = AppendableSet<Parser.State>

typealias Grammar = List<Parser.Rule>
class LazyGrammar(grammarSource: () -> Grammar): Grammar {
    private val grammar by GrammarDelegate(grammarSource)
    override val size: Int
        get() = grammar.size

    override fun contains(element: Parser.Rule) = grammar.contains(element)

    override fun containsAll(elements: Collection<Parser.Rule>) = grammar.containsAll(elements)

    override fun get(index: Int) = grammar.get(index)

    override fun indexOf(element: Parser.Rule) = grammar.indexOf(element)

    override fun isEmpty() = grammar.isEmpty()

    override fun iterator() = grammar.iterator()

    override fun lastIndexOf(element: Parser.Rule) = grammar.lastIndexOf(element)

    override fun listIterator() = grammar.listIterator()

    override fun listIterator(index: Int) = grammar.listIterator(index)

    override fun subList(fromIndex: Int, toIndex: Int) = grammar.subList(fromIndex, toIndex)

    private class GrammarDelegate(private val grammarSource: () -> Grammar) {
        private var grammar: Grammar? = null
        operator fun getValue(lazyGrammar: LazyGrammar, property: KProperty<*>): Grammar {
            if (grammar == null) {
                grammar = grammarSource()
            }
            return grammar!!
        }
    }
}

class AppendableSet<T>: MutableSet<T> {
    val set = mutableSetOf<T>()
    private var first: Element<T>? = null
    private var last: Element<T>? = null

    fun first() = set.first()
    fun last() = set.last()

    class Element<T>(val current: T?) {
        var next: Element<T>? = null
    }

    class AppendableIterator<T>(first: Element<T>): MutableIterator<T> {
        var current: Element<T>
        var prev: Element<T> = Element(null)

        init {
            current = Element(null)
            current.next = first
        }

        override fun hasNext(): Boolean {
            return current.next != null
        }

        override fun next(): T {
            val next = current.next
            if (next?.current == null) {
                throw NoSuchElementException()
            }
            prev = current
            current = next
            return next.current
        }

        override fun remove() {
            val next = current.next
            prev.next = next
            current = prev
        }
    }

    override fun add(element: T): Boolean {
        val res = set.add(element)
        if (res) {
            addElem(element)
        }
        return res
    }

    private fun addElem(element: T) {
        val elem = Element(element)
        if (first == null) {
            first = elem
        }
        if (last != null) {
            last!!.next = elem
        }
        last = elem
    }

    override fun addAll(elements: Collection<T>): Boolean {
        return set.addAll(elements)
    }

    override val size: Int
        get() = set.size

    override fun clear() {
        set.clear()
    }

    override fun isEmpty(): Boolean {
        return set.isEmpty()
    }

    override fun containsAll(elements: Collection<T>): Boolean {
        return set.containsAll(elements)
    }

    override fun contains(element: T): Boolean {
        return set.contains(element)
    }

    override fun iterator(): MutableIterator<T> {
        return AppendableIterator(first?: Element(null))
    }

    override fun retainAll(elements: Collection<T>): Boolean {
        return set.retainAll(elements.toSet())
    }

    override fun removeAll(elements: Collection<T>): Boolean {
        return set.removeAll(elements.toSet())
    }

    override fun remove(element: T): Boolean {
        return set.remove(element)
    }
}

data class Position(val line: Int = 0, val column: Int = 0): Comparable<Position> {
    override fun compareTo(other: Position): Int {
        val lineDiff = line - other.line
        return if (lineDiff == 0) column - other.column else lineDiff
    }

    override fun toString(): String {
        return "(${line + 1}:${column + 1})"
    }

}

var counter = 0
fun getCount(): Int {
    return counter++
}
