import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

/**
 * Create folder if possible, return boolean depending on whether it was created
 */
fun createFolder(folderName: String): Boolean {
    println("Try creating folder $folderName")

    if (File(folderName).exists()) {
        println("Folder $folderName was not created because it already existed")
        return false
    }

    println("Create folder $folderName")
    Files.createDirectory(Paths.get(folderName))

    return true
}

/**
 * Create file using template file if possible, return boolean depending on whether it was created
 */
fun createFileUsingTemplate(fileName: String, templateFilePath: String): Boolean {
    println("Try creating file $fileName")

    if (File(fileName).exists()) {
        println("File $fileName was not created because it already existed")
        return false
    }

    println("Create file $fileName")
    Files.copy(Paths.get(templateFilePath), Paths.get(fileName))
    return true
}

fun writeToFile(fileName: String, content: String) {
    Paths.get(fileName).toFile().writeText(content)
}

fun fileExists(fileName: String): Boolean = Paths.get(fileName).toFile().exists()