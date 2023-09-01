import java.io.File

fun main() {
    run(File("test").readText(), Task.COMPILE)
}