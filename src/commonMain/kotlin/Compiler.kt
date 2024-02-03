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
    private val connections = mutableMapOf<Set<Node>, List<Connection>>()

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
        return null
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
                        it.mark()
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


        val tree = buildTree(node)
        return null
    }

    fun buildTree(node: Node): Tree<Node> {
        val result = Tree(node.end())
        // todo using array list for queue is expansive
        val queue = mutableListOf(result)
        val set = mutableSetOf<Node>()

        while (queue.isNotEmpty()) {
            val tree = queue.removeFirst()
            var current: Node = tree.content
            while (current.dot > 1) {
                current = current.prev!!
            }
            while (true) {
                current.child?.let {
                    val child = Tree(it)
                    // todo remove debug code
                    if (it in set) {
                        var a = 1
                        a++
                    } else {
                        set.add(it)
                    }
                    queue.add(child)
                    tree.addChild(child)
                }

                if (current == tree.content) {
                    break
                }
                if (current.next == null) {
                    throw IllegalArgumentException("Node chain must have a reachable ending\nExpected: ${tree.content}\nLast found: $current")
                }
                current = current.next!!
            }
        }

        return result
    }

    object EmptyStack: Stack(Node(emptyList(), -1, emptySet(), null, ""), nodes = mutableListOf())

    private fun getNode(lexeme: Lexeme): Node {

            val rules = language.ruleMap[lexeme]
            if (rules == null) {
                throw IllegalArgumentException("Unable to find rules for lexeme `${lexeme.desc}`")
            }

        return Node(listOf(EmptyStack), 0, rules, null, "lex")

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
        val saved = connections[nodes]
        if (saved != null) {
            return saved
        }

        val toProcess = nodes.toMutableList()
        val encountered = mutableSetOf<Node>()
            .also { it.addAll(nodes) }
        val mapping = mutableMapOf<TerminalLexeme, MutableMap<NodeContent, MutableList<Stack>>>()
        val result = mutableListOf<Connection>()
        while (toProcess.isNotEmpty()) {
            val current = toProcess.removeFirst()

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


            lexemes.forEach { (lex, rules) ->
                val next = Node(current.stacks, current.dot + 1, rules, current, "term")
                //current.next = next
                when (lex) {
                    is TerminalLexeme -> {
                        mapping.getOrPut(lex) { mutableMapOf() }
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
                            mapping.getOrPut(jump.lexeme) { mutableMapOf() }
                                .getOrPut(content) { mutableListOf() }.add(stack)
                        }
                    }
                }
            }

            if (nullConnection) {
                val ret = current.stacks
                ret.forEach {
                    if (it == EmptyStack) {
                        result.add(Connection(null, Node(emptyList(), -1, emptySet(), null, "")))
                    } else {
                        val node = it.get()!!
                        node.child = current
                        node.mark()
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

        connections[nodes] = result
        return result
    }

    /*class Node(val rules: List<Rule>, val depth: Int) {
        private val connectionInfo: Pair<Map<Lexeme, List<Rule>>, Boolean> by lazy {
            var nullConnection = false
            val map = mutableMapOf<Lexeme, MutableList<Rule>>()
            rules.forEach {
                if (depth < it.right.size) {
                    map.getOrPut(it.right[depth]) {
                        mutableListOf()
                    }.add(it)
                } else {
                    nullConnection = true
                }
            }
            return@lazy map to nullConnection
        }

        val lexemes: Map<Lexeme, List<Rule>>
            get() = connectionInfo.first
        val nullConnection: Boolean
            get() = connectionInfo.second
    }*/

    class Node(val stacks: List<Stack>, dot: Int, rules: Set<Rule>, prev: Node?, reason: String) {
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

        fun start(): Node {
            var current = this
            while (current.prev != null) {
                current = current.prev!!
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

    data class Connection(val lexeme: TerminalLexeme?, val target: List<Node>) {
        constructor(lexeme: TerminalLexeme?, target: Node): this(lexeme, listOf(target))
    }

    open class Stack(
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

    data class Jump(val lexeme: TerminalLexeme, val offset: List<NodeContent>, val rules: Set<Rule>, val from: Node)

    data class NodeContent(val dot: Int, val rules: Set<Rule>, val prev: Node?, val reason: String) {
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

    data class Context(val node: Node, val position: Int)

    /*private fun Tree<State>.toAST(tokens: List<Token>, parent: AST? = null): AST {
        with(tokens.toMutableList()) {
            return AST(value.rule.left, children.size, parent).also {
                children.forEachIndexed { index, tree ->
                    it.children[index] = if (tree == null) {
                        val leaf = AST(value.rule.right[index], 0, it, first())
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
    }*/

    /*private fun completer(state: State, col: Int) {
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
            //new.connect(forest[state]!!.first { it.second == col }, it.dot)
        }
    }*/
    /* private fun scanner(state: State, col: Int, token: Token) {
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
             //new.tree = state.tree.copy()
             println("  $new")
             table[col + 1].add(new)
             createTree(new, col + 1)
         }
     }*/
    /*
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
    */
    /*private fun State.connect(child: Pair<Tree<State>, Int>, pos: Int) {
        val state = this
        val prevState = state.parent
        val parent = if (forest.containsKey(prevState)) {
            if (!forest.containsKey(state)) {
                forest[state] = mutableListOf()
            }

            val prev = forest[prevState]!!.first { it.second < child.second }
            val new = Tree(state, state.rule.right.size)
            prev.first.forEachIndexed { index, tree ->
                new.children[index] = tree
            }
            val newPair = new to child.second
            forest[state]!!.add(newPair)
            new
        } else {
            createTree(state, child.second)
        }
        parent.children[pos] = child.first
    }*/
    /*private fun createTree(state: State, token: Int): Tree<State> {
        if (!forest.containsKey(state)) {
            forest[state] = mutableListOf()
        }

        val tree = Tree(state)
        forest[state]!!.add(tree to token)
        return tree
    }*/

    open class Tree<T>(val content: T) : Iterable<T> {
        protected val children = mutableListOf<Tree<T>>()
        var childCount = 0
            private set

        fun addChild(child: Tree<T>) {
            if (childCount < children.size) {
                children[childCount++] = child
            } else {
                children.add(child)
                childCount++
            }
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

        val value
            get() = when (content) {
                is BranchNode -> content.lexeme
                is LeafNode -> null
            }

        constructor(value: Lexeme, parent: AST? = null) : this(BranchNode(value), parent)
        constructor(value: Token, parent: AST? = null) : this(LeafNode(value), parent)

        fun print(depth: Int = 0) {
            (0 until depth).forEach { print("|") }
            print("|-")
            println(value)
            children.forEach {
                if (it !is AST) {
                    throw IllegalArgumentException("AST children must be an AST too")
                }
                it.print(depth + 1)
                /* todo I'm not sure whether this functionality is completely replaced by new nodes, which
                    would print tokens because token is a value
                    have to double check and test it out
                 */
                /*if (it.token != null) {
                    (0 .. depth + 1).forEach { print("|") }
                    println("|-${it.token}")
                }*/
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
                return if (nameMap.containsKey(tree)) {
                    nameMap[tree]!!
                } else {
                    var res = tree.value.toString()

                    if (tree.content is BranchNode) {
                        val lexeme = tree.content.lexeme
                        if (countMap[lexeme]!! > 1) {
                            usedMap[lexeme] = usedMap[lexeme]!! + 1
                            res += " (${usedMap[lexeme]})"
                        }
                    }

                    nameMap[tree] = res
                    res
                }
            }

            private fun dotConnections(tree: AST): List<String> {
                val left = name(tree)
                val connections =
                    tree.children.filterNotNull().map { """"$left" -> "${name(it as AST)}";""" }.toMutableList()

                if (tree.content is LeafNode) {
                    val token = tree.content.token
                    // removing quotations
                    val name = token.toString().replace(""""""", """""")
                    connections += """"$left" -> "${name} (${token.index})";"""
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
