@file:MavenRepository("mavenCentral", "https://repo.maven.apache.org/maven2")

@file:DependsOn("com.github.kittinunf.fuel:fuel:2.3.1")

import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result
import com.github.kittinunf.fuel.core.Headers

fun get(url: String, sessionId: String): String? {
    val (request, response, result) = url
        .httpGet()
        .header(Headers.COOKIE, "session=$sessionId")
        .responseString()
    return when (result) {
        is Result.Failure -> {
            println("An error occured during http call:")
            val ex = result.getException()
            println(ex)
            null
        }
        is Result.Success -> {
            result.get()
        }
    }
}