/**
 * Created by Domenico on 24/11/2015.
 */
package kgrep

import java.io.File
import java.util.concurrent.Executors
import kotlin.concurrent.thread

val sharedThreadPool = Executors.newCachedThreadPool()

fun main(args: Array<String>) {
    try {
        val filter = args[0]
        // Drop args[0]
        args.drop(1)
        // Only search files, walk top down the tree
        args.forEach { path -> File(path).walkTopDown().filterNot { elem -> !elem.isFile }.forEach { elem -> matches(elem, filter) } }
    } catch(e: ArrayIndexOutOfBoundsException) {
        println("Usage: <pattern> <files>")
    }
}

/**
 * start a thread that searches in a file for lines that contain the filter
 * @param file the file to be scanned
 * @param filter the filter to be used for matching
 */
fun matches(file: File, filter: String) {
    sharedThreadPool.submit {
        thread(start = true, daemon = false, contextClassLoader = null, name = "match", priority = -1, block = {
            file.bufferedReader().lines()
                    .filter { line -> line.contains(filter) }.toArray()
                    .forEachIndexed { i, line -> println("${file.absolutePath}:${i + 1} $line") }
        })
    }
}