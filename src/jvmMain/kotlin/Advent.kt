import java.io.File

fun main() {
    val calibrationDocument = File("input").readText().lines()
    val result = calibrationDocument.map { input ->
        var sumPartTwo = 0
        val digitMap = mapOf(
            "one" to '1', "two" to '2', "three" to '3', "four" to '4', "five" to '5',
            "six" to '6', "seven" to '7', "eight" to '8', "nine" to '9'
        )

        val matches = Regex(digitMap.keys.joinToString("|")).findAll(input)
        val foundDigits = mutableListOf<Int>()
        val matchedWords = matches.map { it.value }.toList()
        val nonDigits = input.split(Regex("[a-zA-Z]+")).filter { it.isNotEmpty() }
        var nonDigitIndex = 0

        matchedWords.forEach { word ->
            if (word in digitMap) {
                foundDigits.add(digitMap[word]!!.toInt())
            } else {
                word.forEach { char ->
                    val digit = char.toString().toIntOrNull()
                    if (digit != null) {
                        foundDigits.add(digit)
                    }
                }
            }
            if (nonDigitIndex < nonDigits.size) {
                foundDigits.addAll(nonDigits[nonDigitIndex].mapNotNull { it.toString().toIntOrNull() })
                nonDigitIndex++
            }
        }

        if (foundDigits.size >= 1) {
            val firstDigit = foundDigits.first()
            val lastDigit = foundDigits.last()
            sumPartTwo += (firstDigit * 10) + lastDigit
        }
        sumPartTwo
    }
    result.forEach {
        println(it)
    }
    println(result.sum())
}

fun part2() {
    val regex = Regex("[0-9]|one|two|three|four|five|six|seven|eight|nine")
    val result = File("input").readText().lines().map {
        var first = -1
        var cur = it
        while (cur.isNotEmpty()) {
            val res = regex.matchAt(cur, 0)
            if (res != null) {
                first = toNum(res.value)
                break
            }
            cur = cur.drop(1)
        }
        var last = -1
        cur = it
        var pos = cur.length
        while (pos > -1) {
            val res = regex.matchAt(cur, pos)
            if (res != null) {
                last = toNum(res.value)
                break
            }
            pos -= 1
        }
        first * 10 + last
    }
    result.forEach {
        println(it)
    }
    println(result.sum())
}

fun toNum(str: String): Int {
    try {
        return str.toInt()
    } catch (e: NumberFormatException) {

    }
    return when (str) {
        "one" -> 1
        "two" -> 2
        "three" -> 3
        "four" -> 4
        "five" -> 5
        "six" -> 6
        "seven" -> 7
        "eight" -> 8
        "nine" -> 9
        else -> 0
    }
}

fun part1() {
    val result = File("input").readText().lines().map { it.filter { it.isDigit() } }
        .map { (it.first().toString() + it.last()).toInt() }
    println(result.sum())
}