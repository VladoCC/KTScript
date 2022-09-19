import java.io.File

fun main() {
    compile(File("test").readText(), Task.COMPILE)
}