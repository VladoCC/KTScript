import java.io.File

actual fun save(text: String, filepath: String) {
    val file = File(filepath)
    println(file.absolutePath)
    if (!file.exists()) {
        file.createNewFile()
    }
    file.writeText(text)
}