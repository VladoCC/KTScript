import language.*
import language.Language.*

enum class Task {
    COMPILE, AST_DOT
}

fun compile(code: String, task: Task, compiler: Compiler) {
    when (task) {
        Task.COMPILE -> compiler.process(code)
        Task.AST_DOT -> {
            save(compiler.astDotString(code), "ast.dot")
        }
    }
}

fun parser(definition: LanguageContext.() -> Unit): Compiler {
    return Compiler(language(definition))
}

// TODO rename correctly
// must have a separate Language class and a separate RypeCompiler class probably
// one for just describing rules and other for all the heavy lifting
// TODO decide whether compiler is one-shot thing (i.e. one object for compiling one string) and move `code: String` into constructor
// or multi-use and then remove process spicific global variables
open class Compiler(val language: Language) {
    private lateinit var table: Table
    private val forest = mutableMapOf<State, MutableList<Pair<Tree<State>, Int>>>()

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

    private fun tokenize(code: Code): List<Token> {
        val tokens = mutableListOf<Token>()
        var error = false
        var unexpectedPosition: Int? = null
        while (!code.isConsumed()){
            var found = false
            for (lex in language.terminals) {
                val token = lex.token(code)
                if (token != null) {
                    code.move(token.content.length)
                    //code.consumeSpaces()
                    tokens.add(token)
                    println("Created token `${token.content}` for $lex at ${token.position}")
                    found = true
                    break
                }
            }
            if (!found) {
                if (unexpectedPosition == null) {
                    unexpectedPosition = code.pos
                    error = true
                }
                //code.consumeChar()
            } else if (unexpectedPosition != null) {
                val unexpectedText = code.part(unexpectedPosition, code.pos)
                println("Unresolved token: `$unexpectedText` at $unexpectedPosition")
                unexpectedPosition = null
            }
        }
        return if (error) emptyList() else tokens
    }

    fun parse(tokens: List<Token>): AST? {
        table = Table(language.root, tokens.size)
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
        val result = table.last().firstOrNull {
            it.rule.right.size == 1 && it.rule.right[0] == language.root && it.isComplete()
        }
        if (result != null) {
            result.let { state ->
                if (state == null) {
                    return null
                }
                val tree = state.tree
                val ast = tree.toAST(tokens.toList().toMutableList())
                ast.print()
                return ast
            }
        } else {
            println("Unable to parse the code")
            return null
        }
    }

    private fun Tree<State>.toAST(tokens: List<Token>, parent: AST? = null): AST {
        with(tokens.toMutableList()) {
            return AST(value.rule.left, parent, children.size).also {
                children.forEachIndexed { index, tree ->
                    it.children[index] = if (tree == null) {
                        val leaf = AST(value.rule.right[index], it, 0, first())
                        if (isNotEmpty()) {
                            removeAt(0)
                        }
                        leaf
                    } else {
                        tree.toAST(this, it)
                    }
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
                    "  From [${table[col].indexOf(state)}] $state\n" +
                    "  And state[${state.origin}][$index] ${it}\n" +
                    "  Rule: ${it.rule}")
            table[col].add(new)
            println("  $new")
            new.connect(forest[state]!!.first { it.second == col }, it.dot)
        }
    }
    private fun scanner(state: State, col: Int, token: Token) {
        val next = state.next()
        if (next !is TerminalLexeme) {
            throw IllegalArgumentException()
        }

        val success = next.match(token)
        println("Scan[${col + 1}][${table[col+1].size}]\n" +
                "  Scanning `${token}` at col ${col} for $next\n" +
                "  From state[$col][${table[col].indexOf(state)}] ${state.rule}")
        println("  Success: $success")
        if (success) {
            //println("  Matched ${success.tokens!!.current.content} to $next")
            val new = state.copy(dot = state.dot + 1)
            new.tree = state.tree.copy()
            println("  $new")
            table[col + 1].add(new)
            createTree(new, col + 1)
        }
    }
    private fun predictor(state: State, col: Int) {
        language.rules.mapIndexed { index, rule -> index to rule }
            .filter { it.second.left == state.next() }
            .forEach { pair ->
                val (index, it) = pair
                val new = State(it, col)
                println("Predicted[$col][${table[col].size}]\n" +
                        "  For [${table[col].indexOf(state)}] ${state.rule}\n" +
                        "  From [$index] $it")
                println("  $new")
                table[col].add(new)
            }
    }
    private fun State.connect(child: Pair<Tree<State>, Int>, pos: Int) {
        val state = this
        val prevState = state.parent
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

    class Table(root: Lexeme, size: Int) {
        private val columns = mutableListOf<Column>()
        init {
            val rootCol: Column = AppendableSet()
            val rootState = State(Rule(Lexeme(), Product(root)), 0)
            rootCol.add(rootState)
            columns.add(rootCol)
        }
        operator fun get(index: Int): Column {
            (0..(index - columns.size)).forEach { _ ->
                columns.add(AppendableSet())
            }
            return columns[index]
        }
        fun last() = columns.last()
        val indices
        get() = columns.indices
        val size
        get() = columns.size
    }
    data class State(
        val rule: Rule,
        var origin: Int,
        var dot: Int = 0,
        val parent: State? = null
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
            result = 31 * result + origin
            result = 31 * result + dot
            return result
        }

        override fun toString(): String {
            return "State(rule=$rule, origin=$origin, dot=$dot)"
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

typealias Column = AppendableSet<Compiler.State>

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
        val res = set.addAll(elements)
        if (res) {
            elements.forEach { addElem(it) }
        }
        return res
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

var counter = 0
fun getCount(): Int {
    return counter++
}
