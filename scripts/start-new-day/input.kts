@file:Include("../utils/util-http.kts")
@file:Include("../utils/util-file.kts")
@file:Include("../utils/util-git.kts")
@file:Include("../utils/util-config.kts")

val inputFileName = "input.txt"

fun buildInputFilePath(folderName: String): String {
    return "$folderName/$inputFileName"
}

fun retrieveInput(day: Int, sessionId: String): String? {
    val url = "https://adventofcode.com/2020/day/$day/input"
    return get(url, sessionId)
}

fun writeInput(folderName: String, input: String) {
    writeToFile(
        buildInputFilePath(folderName),
        input
    )
}

fun inputFileExists(folderName: String): Boolean = fileExists(buildInputFilePath(folderName))

fun loadInput(day: Int, folderName: String) {
    println("""
        
        #####################
        ### INPUT LOADING ###
        #####################
        
    """.trimIndent())

    println("# Check if input file exists")

    if(!inputFileExists(folderName)) {
        println("Input file does not exist yet")
        println("# Retrieve input from AOC site")
        val input = retrieveInput(currentDateDay, loadSessionId())

        if (input == null) {
            println("Input was empty; no file was written")
        } else {
            println("# Write input")
            println("Writing input for day $currentDateDay into file")
            writeInput(folderName, input)

            println("Commit input for day $currentDateDay")
            commitInput(currentDateDay)
        }
    } else {
        println("Input file already exists")
    }
}

