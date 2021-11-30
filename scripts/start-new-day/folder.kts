@file:Include("../utils/util-file.kts")

fun createDayFolder(day: Int): String {
    println("""
        #####################
        ### CREATE FOLDER ###
        #####################
        
    """.trimIndent())
    val folderName = "day$day"
    createFolder(folderName)
    return folderName
}