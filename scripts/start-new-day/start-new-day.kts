@file:Include("input.kts")
@file:Include("folder.kts")
@file:Include("solution-file.kts")

import java.time.LocalDate

val currentDateDay = LocalDate.now().dayOfMonth
val folderName = createDayFolder(currentDateDay)
createPart1File(folderName, currentDateDay)
loadInput(currentDateDay, folderName)