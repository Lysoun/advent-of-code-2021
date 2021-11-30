import java.io.File
import java.nio.file.Paths

val sessionIdPath = ".config/session_id.txt"

fun loadSessionId(): String = Paths.get(sessionIdPath).toFile().readText()