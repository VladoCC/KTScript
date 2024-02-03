package language

import Code
import getCount

open class Lexeme(val desc: String = ""): Comparable<Lexeme> {
    protected val id = getCount()

    operator fun plus(lexeme: Lexeme): Product {
        return Product(this).also { it + lexeme }
    }
    operator fun plus(optional: Optional): OptionalProduct {
        return OptionalProduct(Product(this), optional)
    }
    operator fun plus(optional: Optionals): OptionalProduct {
        return OptionalProduct(Product(this), optional.optionals)
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

abstract class TerminalLexeme(desc: String, private val skipList: List<Skip>): Lexeme("`$desc`") {
    protected open fun skip(code: Code): Code {
        val codeStr = code.current()
        var pos = 0
        var search = true
        while (search) {
            search = false
            for (it in skipList) {
                val step = it.consume(codeStr, pos)
                if (step > 0) {
                    search = true
                    pos += step
                    break
                }
            }
        }
        return code.move(pos)
    }
    abstract fun token(code: Code): Token?
    abstract fun match(token: Token): Boolean
}
open class ExactLexeme(val content: String, desc: String = content, skipList: List<Skip>): TerminalLexeme(desc, skipList) {
    override fun token(code: Code): Token? {
        val trimmed = skip(code)
        return if (trimmed.current().startsWith(content)) {
            Token(content, trimmed.position(), emptySet(), desc)
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
class WordLexeme(content: String, desc: String = content, skipList: List<Skip>): ExactLexeme(content, desc, skipList) {
    override fun token(code: Code): Token? {
        // checking for content to match
        val exact = super.token(code)
        // checking that next symbol is not a letter or digit, i.e. not a part of the word
        return if (exact != null) {
            if (code.at(exact.position.index + exact.content.length).current()[0].isLetterOrDigit()) {
                null
            } else {
                exact
            }
        } else {
            null
        }
    }
}
open class RegexLexeme(pattern: String, private val name: String, skipList: List<Skip>): TerminalLexeme(name, skipList) {

    private val regex = Regex(pattern)

    override fun token(code: Code): Token? {
        val trimmed = skip(code)
        val result = regex.find(trimmed.current())
        if (result == null || result.range.first != 0) {
            return null
        }
        return RegexToken(result.value, trimmed.position(), setOf(name, "regex"), result)
    }

    override fun match(token: Token): Boolean {
        return token.tags.contains(name)
    }

    override fun toString(): String {
        return "RegexLexeme(regex='$regex', id='$id', name='$name')"
    }
}
class CustomLexeme(val tokenLambda: (Code) -> Token?,
                   val matchLambda: (Token) -> Boolean,
                   desc: String = "custom", skipList: List<Skip>): TerminalLexeme(desc, skipList) {
    override fun token(code: Code): Token? = tokenLambda(code)
    override fun match(token: Token): Boolean = matchLambda(token)
}