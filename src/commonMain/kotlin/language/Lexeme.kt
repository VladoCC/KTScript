package language

import Code
import Compiler
import Position
import getCount
import kotlin.native.concurrent.ThreadLocal

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
    protected open fun trim(code: Code): Code {
        val result = Regex("\\s+").find(code.current())
        return if (result == null || result.range.first != 0) code else code.move(result.value.length)
    }
    abstract fun token(code: Code): Token?
    abstract fun match(token: Token): Boolean
}
open class ExactLexeme(val content: String, desc: String = content): TerminalLexeme(desc) {
    override fun token(code: Code): Token? {
        return if (code.current().startsWith(content)) {
            Token(content, code.position(), emptySet(), desc)
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
class WordLexeme(content: String, desc: String = content): ExactLexeme(content, desc) {
    override fun token(code: Code): Token? {
        // checking for content to match
        val exact = super.token(code)
        // checking that next symbol is not a letter or digit, i.e. not a part of the word
        return if (exact != null && !code.current()[exact.content.length].isLetterOrDigit()) {
            exact
        } else {
            null
        }
    }
}
open class RegexLexeme(private val regex: Regex, private val name: String): TerminalLexeme(name) {
    override fun token(code: Code): Token? {
        val result = regex.find(code.current())
        if (result == null || result.range.first != 0) {
            return null
        }
        return RegexToken(result.value, code.position(), setOf(name, "regex"), result)
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