package language

import Position
import kotlin.native.concurrent.ThreadLocal

open class Token(val content: String, val position: Position, val tags: Set<String>, private val desc: String = content) {
    val index: Int = next()

    @ThreadLocal
    companion object {
        var counter = -1
        fun next(): Int {
            return ++counter
        }
    }

    override fun toString(): String {
        val descStr = desc.map { if (it.code == 10) "\\n" else it.toString() }.joinToString("")
        return "Token(content='$descStr', position=$position, tags=$tags)"
    }
}
class RegexToken(content: String, position: Position, tags: Set<String>, val result: MatchResult, desc: String = content):
    Token(content, position, tags, desc)