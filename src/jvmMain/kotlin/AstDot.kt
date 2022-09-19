import com.lordcodes.turtle.shellRun
import java.io.File

fun main() {
    compile(File("test").readText(), Task.AST_DOT)
}