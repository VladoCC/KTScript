import java.io.File

fun main() {
    run(File("test").readText(), Task.AST_DOT)
}