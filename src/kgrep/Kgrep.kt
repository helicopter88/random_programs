/**
 * Created by Domenico on 24/11/2015.
 */
package kgrep

val sharedThreadPool = Executors.newCachedThreadPool()

fun main(args: Array<String>) {
    if (args.size < 2) {
        println("Usage: kgrep <pattern> <folder1, folder2 ..>")
        return
    }
    val filter = args[0]
    // Drop args[0]
    args.drop(1)
    // Only search files, walk top down the tree
    args.forEach { path ->
        File(path).walkTopDown()
                .filterNot { elem -> !elem.isFile }
                .forEach { elem -> matches(elem, filter) }
    }
}

/**
 * start a thread that searches in a file for lines that contain the filter
 * @param file the file to be scanned
 * @param filter the filter to be used for matching
 */
fun matches(file: File, filter: String) {
    sharedThreadPool.submit {
        thread(block = {
            file.bufferedReader().lines()
                    .filter { line -> line.contains(filter) }.toArray()
                    .forEachIndexed { i, line -> println("${file.absolutePath}:${i + 1} $line") }
        })
    }
}