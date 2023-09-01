package language

import Code

public fun language(definition: LanguageContext.() -> Unit, root: Lexeme = Lexeme("root")): Language {
    val context = LanguageContext(root)
    val language = Language(context.define(definition), root)
    context.getCallbacks().forEach {
        it(language)
    }
    return language
}

/**
 * Describes lexical, grammatical and semantic rules of the language and also generators for it
 */
class Language(val rules: List<Rule>, val root: Lexeme) {
    fun subgrammar(root: Lexeme): Language {
        val ruleSet = mutableMapOf<Lexeme, MutableList<Rule>>()
        rules.forEach {
            if (!ruleSet.containsKey(it.left)) {
                ruleSet[it.left] = mutableListOf()
            }
            ruleSet[it.left]!!.add(it)
        }
        val current = mutableListOf<Lexeme>()
        current.add(root)
        val rulesSub = mutableMapOf<Lexeme, List<Rule>>()
        val termsSub = mutableSetOf<TerminalLexeme>()
        while (current.isNotEmpty()) {
            val left = current[0]
            ruleSet[left]?.let { rules ->
                rulesSub[left] = rules
                rules.forEach {rule ->
                    rule.right.forEach {
                        // TODO it !in current is O(N) and can be optimized
                        if (it !in rulesSub && it !in current) {
                            current.add(it)
                        }
                        if (it is TerminalLexeme) {
                            termsSub.add(it)
                        }
                    }
                }
            }
            current.removeAt(0)
        }
        val language = Language(rulesSub.values.flatten(), root)
        // todo decide whether we keep terminals in language or removing this commented terminal stuff
        //  (and all the previous logic for collecting this terminals)
        // language.terminals.addAll(terminals.filter { it in termsSub })
        return language
    }
}

class LanguageContext(val root: Lexeme) {

    private val rules: MutableList<Rule> = mutableListOf()
    private val callbacks = mutableListOf<(Language) -> Unit>()
    private val terminals = mutableSetOf<TerminalLexeme>()
    private val nonTerminals = mutableSetOf<Lexeme>()

    val newline = object: RegexLexeme(Regex("\\n\\s*"), "newline") {
        override fun trim(code: Code): Code {
            return code
        }
    }
    val end = object: TerminalLexeme("END_TERM") {
        override fun token(code: Code) = null
        override fun match(token: Token) = token.tags.contains("END_TERM")
    }

    fun define(definition: LanguageContext.() -> Unit): List<Rule> {
        definition()
        return rules
    }

    fun getCallbacks(): List<(Language) -> Unit> = callbacks

    fun nonTerm(desc: String = ""): Lexeme {
        val res = Lexeme(desc)
        nonTerminals.add(res)
        return res
    }
    fun term(content: String, desc: String? = null): TerminalLexeme {
        val token = ExactLexeme(content, desc?: content)
        terminals.add(token)
        return token
    }
    fun wordTerm(content: String, desc: String? = null): WordLexeme {
        val token = WordLexeme(content, desc?: content)
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
            rules.add(rule)
            rule
        }
    }
    fun rule(left: Lexeme, right: Lexeme): Rule {
        return rule(left, Product(right)).first()
    }

    operator fun Optional.plus(lexeme: Lexeme): OptionalProduct {
        return OptionalProduct(null, this) + lexeme
    }

    fun addOnLanguageConstructedCallback(callback: (Language) -> Unit) = callbacks.add(callback)
}
