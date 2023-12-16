import language.Language
import language.LanguageContext
import language.Lexeme

fun main() {
    val context = LanguageContext(Lexeme("root")) {
        val b = nonTerm("(")
        val c = nonTerm("c")
        val d = nonTerm(",")
        val e = nonTerm(")")
        val zeroPlus = zeroOrMore(d + c)
        val optD = optional(d)
        val optCont = c + zeroPlus + optD
        val optOut = optional(optCont)
        rule(root, b + optOut + e)
    }
    val test = 0;
}