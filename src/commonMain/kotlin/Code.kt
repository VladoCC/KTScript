class Code private constructor(private val storage: CodeStorage, val pos: Int = 0) {
    constructor(code: String) : this(CodeStorage(code))

    fun current(): String {
        return storage.get(pos)
    }

    fun isConsumed() = pos == storage.text.length

    fun at(index: Int): Code {
        if (index > storage.text.length || index < 0) {
            throw IllegalArgumentException("Index $index is out of bounds, code length: ${storage.text.length}")
        }
        return Code(storage, index)
    }

    fun at(position: Position) = at(position.index)

    fun move(step: Int): Code {
        val newPos = pos + step
        if (newPos > storage.text.length || newPos < 0) {
            throw IllegalArgumentException("Index $newPos is out of bounds")
        }
        return Code(storage, newPos)
    }

    fun position(): Position {
        return storage.pos(pos)
    }

    fun part(from: Int, to: Int): String {
        return storage.get(from).take(to - from)
    }

    override fun toString(): String {
        val word = current().split(Regex("\\s")).firstOrNull { it.isNotBlank() }?: ""
        return "Code(word=`${word}`, pos=$pos (${position()}))"
    }


    class CodeStorage(val text: String) {
        private val storage = mutableMapOf<Int, String>(0 to text)
        private val posMap = mutableMapOf<Int, Position>(0 to Position())
        fun get(at: Int): String {
            return if (storage.containsKey(at)) {
                storage[at]!!
            } else {
                var i = 0
                val res = text.drop(at)
                storage[at] = res
                res
            }
        }

        fun pos(at: Int): Position {
            return if (posMap.containsKey(at)) {
                posMap[at]!!
            } else {
                val cut = text.take(at)
                val line = cut.count { it == '\n' }
                val col = cut.takeLastWhile { it != '\n' }.length
                Position(at, line, col)
            }
        }
    }
}

data class Position(val index: Int = 0, val line: Int = 0, val column: Int = 0): Comparable<Position> {
    override fun compareTo(other: Position): Int {
        val lineDiff = line - other.line
        return if (lineDiff == 0) column - other.column else lineDiff
    }

    override fun toString(): String {
        return "(${line + 1}:${column + 1})"
    }
}