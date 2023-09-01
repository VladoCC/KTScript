package language

import kotlin.math.min
import kotlin.native.concurrent.ThreadLocal

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

typealias Optional = Array<Product>

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
class ProductList(private val products: List<Product>): List<Product> by products, ProductProducer {
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
    val index: Int = next()

    @ThreadLocal
    companion object {
        var counter = -1
        fun next(): Int {
            return ++counter
        }
    }

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

    override fun toString(): String {
        return "Rule(index=$index, left=$left, right=$right)"
    }
}
