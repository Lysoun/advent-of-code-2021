#!/usr/bin/env kscript

import java.io.File

fun runCommand(
    command: List<String>,
    workingDir: File = File(".")
): String? = try {
    ProcessBuilder(command)
        .directory(workingDir)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.PIPE)
        .start()
        .inputStream.bufferedReader().readText().trimEnd()
} catch (e: java.io.IOException) {
    e.printStackTrace()
    null
}