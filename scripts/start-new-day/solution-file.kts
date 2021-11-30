@file:Include("../utils/util-file.kts")
@file:Include("../utils/util-git.kts")

fun createPart1File(folderName: String): Boolean = createFileUsingTemplate(
    fileName = "${folderName}/part1.ml",
    templateFilePath = "scripts/templates/solution_template.ml"
)

fun createPart1File(folderName: String, day: Int) {
    println("""
        
        ##########################
        ### CREATE PART 1 FILE ###
        ##########################

    """.trimIndent())
    val part1FileCreated = createPart1File(folderName)
    if (part1FileCreated) {
        println("Commit files for day $currentDateDay")
        commitDaySolutionsInit(currentDateDay)
    }
}