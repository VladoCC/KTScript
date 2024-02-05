import language.*

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

fun parser(skip: List<Skip> = emptyList(), definition: LanguageContext.() -> Unit): Compiler {
    return Compiler(language(definition, skip))
}

// TODO rename correctly
// must have a separate Language class and a separate RypeCompiler class probably
// one for just describing rules and other for all the heavy lifting
// TODO decide whether compiler is one-shot thing (i.e. one object for compiling one string) and move `code: String` into constructor
// or multi-use and then remove process spicific global variables
open class Compiler(val language: Language) {
    private val nodeMap = mutableMapOf<Lexeme, Node>()
    private val unfoldMap = mutableMapOf<Lexeme, List<Jump>>()
    private val steps = mutableMapOf<Node, StepInfo>()

    fun process(code: String): AST? {
        return parse(Code(code))
    }

    fun astDotString(code: String): String {
        return parse(Code(code))?.dotGraph() ?: ""
    }


    /**
     * * fun match starts with list of one element, which is a starting node representing some non-terminal lexeme with its rules unmatched
     * * for each node in the list match follows every connection of the node, removes node from the list, adds new nodes to the end of the list
     * * if following a terminal connection just check if code-text starts with it
     * * if connection lexeme is null (finish node found), store it and code position at which it was found, rewrite previous ones
     * (MIGHT BE INCORRECT FOR SOME GRAMMARS, NEEDS TESTING, PROBLEM WITH OVER-CONSUMING CODE FOLLOWING ON LEXEME, WHEN THIS PART IS NECESSARY FOR THE NEXT LEXEME)
     * * otherwise call match for this code-text and a lexeme stored in connection
     * * when list of nodes is empty, return stored code position or null if none was found
     * * returning null means that match was unsuccessful, returning position means that path successfully matched and requires to consume code-text
     * until this point to continue
     */
    fun parse(code: Code): AST? {
        val result = match(getNode(language.root), code)

        if (result != null) {
            println("success")
        } else {
            println("ERROR: Have not reached a final state.")
        }
        return result
    }

    private fun match(node: Node, code: Code): AST? {
        val positions = mutableListOf(code.pos)
        val contexts = mutableMapOf(code.pos to mutableSetOf(node))
        var result: Position? = null

        while (contexts.isNotEmpty()) {
            val pos = positions.removeFirst()
            val inProcess = contexts[pos]!!
            contexts.remove(pos)
            val connections = connections(inProcess)
            val current = code.at(pos)
            println("Searching match at position ${current.position()}: $current")

            connections.forEach { connection ->
                if (connection.lexeme == null) {
                    // todo collect results
                    println("Success")
                    result = current.position()
                    return@forEach
                }

                val token = connection.lexeme.token(current)
                if (token != null) {
                    // todo this logic isn't consuming whitespaces and will get stuck on it
                    //  though whitespaces should be approached with a great foresight because there are
                    //  cases in which whitespace is a delimeter and neccessary token, but it's followed
                    //  by other whitespaces that are to be omitted
                    connection.target.forEach {
                        it.token = token
                        var index = positions.size - 1
                        val posNew = token.position.index + token.content.length
                        if (!contexts.containsKey(posNew)) {
                            while (index >= 0 && positions[index] >= posNew) {
                                index--
                            }
                            if (index == positions.size - 1) {
                                positions.add(posNew)
                            } else {
                                positions.add(index + 1, posNew)
                            }
                        }
                        contexts.getOrPut(posNew) { mutableSetOf() }.add(it)
                    }
                }
            }

            if (current.isConsumed()) {
                break
            }
        }
        if (result == null || code.at(result!!).current().isNotBlank()) {
            return null
        }


        val tree = buildAST(node.end())
        return tree
    }

    private fun buildAST(node: Node): AST {
        return AST(node.rules.first().left).also {
            var current: Node = node
            while (current.dot > 1) {
                current = current.prev!!
            }
            while (true) {
                if (current.child != null) {
                    it.addChild(buildAST(current.child!!))
                } else if (current.token != null) {
                    it.addChild(AST(current.token!!))
                } else {
                    throw IllegalStateException("Branch ended without a leaf")
                }

                if (current.dot == node.dot) {
                    break
                }

                if (current.next == null) {
                    throw IllegalArgumentException("Node chain must have a reachable ending\nExpected: ${node}\nLast found: $current")
                }
                current = current.next!!
            }
        }
    }

    private object EmptyStack: Stack(Node(emptyList(), -1, emptySet(), null, ""), nodes = mutableListOf())

    private fun getNode(lexeme: Lexeme): Node {
        return nodeMap.getOrPut(lexeme) {
            val rules = language.ruleMap[lexeme]
            if (rules == null) {
                throw IllegalArgumentException("Unable to find rules for lexeme `${lexeme.desc}`")
            }

            Node(listOf(EmptyStack), 0, rules, null, "lex")
        }
    }

    private fun unfold(lexeme: Lexeme, encountered: MutableSet<Lexeme> = mutableSetOf()): List<Jump> {
        val stored = unfoldMap[lexeme]
        if (stored != null) {
            //println("Unfold ${lexeme.desc} from map")
            return stored
        }

        println("Unfold ${lexeme.desc} started")
        if (lexeme in encountered) {
            throw IllegalStateException("Encountered lexeme cyclic reference for ${lexeme.desc}")
        }

        val result = mutableListOf<Jump>()
        if (lexeme is TerminalLexeme) {
            throw IllegalStateException("Unable to unfold Terminal Lexeme")
        }

        val node = getNode(lexeme)
        val grouped = mutableMapOf<Lexeme, MutableSet<Rule>>()
        node.rules.forEach {
            grouped.getOrPut(it.right[0]) { mutableSetOf() }.add(it)
        }
        grouped.forEach { (lex, rules) ->
            when (lex) {
                is TerminalLexeme -> result.add(Jump(lex, emptyList(), rules, node))
                else -> {
                    encountered.add(lexeme)
                    val jumps = unfold(lex, encountered)
                    jumps.forEach {
                        val offset = List(it.offset.size + 1) { i ->
                            if (i == 0) {
                                NodeContent(node.dot + 1, rules, node, "jump1")
                            } else {
                                it.offset[i - 1]
                            }
                        }
                        result.add(Jump(it.lexeme, offset, it.rules, it.from))
                    }
                }
            }
        }

        unfoldMap[lexeme] = result
        //println("Unfold ${lexeme.desc} finished")
        return result
    }

    private fun connections(nodes: Set<Node>): List<Connection> {
        // todo replace with map keyed by Node.content
        val toProcess = nodes.toMutableList()
        val encountered = mutableSetOf<Node>()
            .also { it.addAll(nodes) }
        val mapping = mutableMapOf<TerminalLexeme, MutableMap<NodeContent, MutableList<Stack>>>()
        val result = mutableListOf<Connection>()
        while (toProcess.isNotEmpty()) {
            val current = toProcess.removeFirst()
            val step = steps.getOrPut(current) {
                var nullConnection = false
                val lexemes = mutableMapOf<Lexeme, MutableSet<Rule>>()
                current.rules.forEach {
                    if (current.dot < it.right.size) {
                        lexemes.getOrPut(it.right[current.dot]) {
                            mutableSetOf()
                        }.add(it)
                    } else {
                        nullConnection = true
                    }
                }


                val nodeMapping = mutableMapOf<TerminalLexeme, MutableMap<NodeContent, MutableList<Stack>>>()
                lexemes.forEach { (lex, rules) ->
                    val next = Node(current.stacks, current.dot + 1, rules, current, "term")
                    //current.next = next
                    when (lex) {
                        is TerminalLexeme -> {
                            nodeMapping.getOrPut(lex) { mutableMapOf() }
                                .getOrPut(next.content) { mutableListOf() }.addAll(next.stacks)
                        }

                        else -> {
                            unfold(lex).forEach { jump ->
                                //println("Creating stack for jump ${lex.desc} -> ${jump.lexeme.desc}")
                                var stack = Stack(next)
                                jump.offset.mapIndexed { index, it ->
                                    stack = stack.add(Node(stack, it.dot, it.rules, it.prev, it.reason))
                                }
                                val content = NodeContent(1, jump.rules, jump.from, "jump2")
                                nodeMapping.getOrPut(jump.lexeme) { mutableMapOf() }
                                    .getOrPut(content) { mutableListOf() }.add(stack)
                            }
                        }
                    }
                }

                StepInfo(nullConnection, nodeMapping)
            }
            step.mapping.forEach { entry ->
                val nodeMap = mapping.getOrPut(entry.key) { mutableMapOf() }
                entry.value.forEach {
                    nodeMap.getOrPut(it.key) { mutableListOf() }.addAll(it.value)
                }
            }

            if (step.stepUp) {
                val ret = current.stacks
                var chain: Node? = current
                while (chain != null) {
                    chain.mark()
                    chain = chain.prev
                }
                ret.forEach {
                    if (it == EmptyStack) {
                        result.add(Connection(null, Node(emptyList(), -1, emptySet(), null, "")))
                    } else {
                        val node = it.get()!!
                        node.child = current
                        // since we know this rule to be correct, we mark every node of it to have forward-linking
                        if (node !in encountered) {
                            toProcess.add(node)
                            encountered.add(node)
                        }
                    }
                }
            }
        }
        mapping.forEach {
            val nodes = it.value.map {
                Node(it.value, it.key.dot, it.key.rules, it.key.prev, it.key.reason)
            }
            result.add(Connection(it.key, nodes))
        }

        return result
    }

    private class Node(val stacks: List<Stack>, dot: Int, rules: Set<Rule>, prev: Node?, reason: String) {
        val content = NodeContent(dot, rules, prev, reason)
        val dot: Int
            get() = content.dot
        val rules
            get() = content.rules
        var child: Node? = null
        var next: Node? = null
        val prev: Node?
            get() = content.prev
        var token: Token? = null

        constructor(stack: Stack, dot: Int, rules: Set<Rule>, prev: Node?, reason: String): this(listOf(stack), dot, rules, prev, reason)

        fun end(): Node {
            var current = this
            while (current.next != null) {
                current = current.next!!
            }
            return current
        }

        fun mark() {
            prev?.next = this
        }

        override fun toString(): String {
            return "Node(stacks=$stacks, dot=$dot, rules=$rules)"
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other == null || this::class != other::class) return false

            other as Node

            if (stacks != other.stacks) return false
            if (content != other.content) return false

            return true
        }

        override fun hashCode(): Int {
            var result = stacks.hashCode()
            result = 31 * result + content.hashCode()
            return result
        }
    }

    private data class Connection(val lexeme: TerminalLexeme?, val target: List<Node>) {
        constructor(lexeme: TerminalLexeme?, target: Node): this(lexeme, listOf(target))
    }

    private open class Stack(
        private val root: Node,
        private val nodes: MutableList<Node> = mutableListOf()
    ) {
        private var index: Int = nodes.size - 1

        fun get(): Node? {
            return if (index >= 0) {
                nodes[index]
            } else if (index == -1) {
                root
            } else {
                null
            }
        }

        fun add(node: Node): Stack = Stack(root, nodes.also { it.add(node) })
    }

    private data class Jump(val lexeme: TerminalLexeme, val offset: List<NodeContent>, val rules: Set<Rule>, val from: Node)

    private data class StepInfo(
        val stepUp: Boolean,
        val mapping: Map<TerminalLexeme, MutableMap<NodeContent, MutableList<Stack>>>
    )

    private data class NodeContent(val dot: Int, val rules: Set<Rule>, val prev: Node?, val reason: String) {
        // todo including prev into node content and then removing it from equals looks sloppy
        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other == null || this::class != other::class) return false

            other as NodeContent

            if (dot != other.dot) return false
            if (rules != other.rules) return false

            return true
        }

        override fun hashCode(): Int {
            var result = dot
            result = 31 * result + rules.hashCode()
            return result
        }
    }

    open class Tree<T>(val content: T) : Iterable<T> {
        protected val children = mutableListOf<Tree<T>>()
        val childCount
            get() = children.size

        fun addChild(child: Tree<T>) {
            children.add(child)
        }

        override fun iterator(): Iterator<T> {
            return iterator(Order.DepthFirst)
        }

        fun iterator(order: Order): Iterator<T> {
            return TreeIterator(this, order)
        }

        fun copy(): Tree<T> {
            return Tree(content).also {
                it.children.indices.forEach { index ->
                    it.children[index] = children[index]
                }
            }
        }

        enum class Order {
            BreadthFirst, DepthFirst
        }

        private class TreeIterator<T>(tree: Tree<T>, private val order: Order) : Iterator<T> {
            val nodeList = mutableListOf<T>()
            val listIterator: Iterator<T>

            init {
                val toProcess = mutableListOf(tree)
                while (toProcess.isNotEmpty()) {
                    val cur = toProcess.first()
                    toProcess.removeAt(0)
                    nodeList.add(cur.content)
                    cur.children.forEach {
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

    class AST(value: AST.Node, val parent: AST? = null) : Tree<AST.Node>(value) {
        constructor(value: Lexeme, parent: AST? = null) : this(BranchNode(value), parent)
        constructor(value: Token, parent: AST? = null) : this(LeafNode(value), parent)

        fun dotGraph(): String {
            return DotPrinter(this).print()
        }

        private class DotPrinter(private val tree: AST) {
            private val countMap = mutableMapOf<Lexeme, Int>()
            private val usedMap = mutableMapOf<Lexeme, Int>()
            private val nameMap = mutableMapOf<AST, String>()

            init {
                tree.forEach {
                    if (it is BranchNode) {
                        val lexeme = it.lexeme
                        if (countMap.containsKey(lexeme)) {
                            countMap[lexeme] = countMap[lexeme]!! + 1
                            usedMap[lexeme] = 0
                        } else {
                            countMap[lexeme] = 1
                        }
                    }
                }
            }

            private fun name(tree: AST): String {
                return nameMap.getOrPut(tree) {
                    var res = tree.content.toString()

                    if (tree.content is BranchNode) {
                        val lexeme = tree.content.lexeme
                        if (countMap[lexeme]!! > 1) {
                            usedMap[lexeme] = usedMap[lexeme]!! + 1
                            res += " (${usedMap[lexeme]})"
                        }
                    }

                    // escaping quotations because dot does not like them
                    val name = res.replace(""""""", """\"""")
                    nameMap[tree] = name
                    name
                }
            }

            private fun dotConnections(tree: AST): List<String> {
                val left = name(tree)
                val connections =
                    tree.children.toList().map { """"$left" -> "${name(it as AST)}";""" }.toMutableList()

                /*if (tree.content is LeafNode) {
                    val token = tree.content.token

                    val name = token.toString()
                    connections += """"$left" -> "${name} (${token.index})";"""
                }*/

                val childConnections = tree.children.flatMap { dotConnections(it as AST) }
                return connections + childConnections
            }

            fun print(): String {
                val connections = dotConnections(tree)
                    .joinToString("\n    ")
                return "digraph {\n    $connections\n}"
            }
        }

        sealed class Node
        class BranchNode(val lexeme: Lexeme) : Node() {
            override fun toString(): String {
                return lexeme.toString()
            }
        }

        class LeafNode(val token: Token) : Node() {
            override fun toString(): String {
                return token.toString()
            }
        }
    }
}

var counter = 0
fun getCount(): Int {
    return counter++
}
