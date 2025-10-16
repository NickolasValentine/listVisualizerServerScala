package list

import data._
import java.io._
import java.util
import java.util.regex.{Pattern, Matcher}
import scala.jdk.CollectionConverters._

class SingleLinkedList(val prototype: UserType) {

  private class Node(var value: AnyRef) {
    var next: Node = null
  }

  private var head: Node = null
  private var size: Int = 0
  private val elementClass: Class[_] = {
    val sample = prototype.create
    if (sample != null) sample.getClass else null
  }

  private def checkAcceptable(obj: AnyRef): Unit = {
    if (obj == null) return
    if (elementClass != null && !elementClass.isAssignableFrom(obj.getClass)) {
      throw new IllegalArgumentException(
        s"Object of class ${obj.getClass} is not acceptable for list of type ${prototype.typeName} (expected $elementClass)"
      )
    }
  }

  def getSize: Int = size

  def add(obj: AnyRef): Unit = {
    checkAcceptable(obj)
    val n = new Node(obj)
    if (head == null) {
      head = n
    } else {
      var cur = head
      while (cur.next != null) cur = cur.next
      cur.next = n
    }
    size += 1
  }

  def get(index: Int): AnyRef = {
    checkIndex(index)
    var cur = head
    for (_ <- 0 until index) cur = cur.next
    cur.value
  }

  def insert(index: Int, obj: AnyRef): Unit = {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(s"index=$index")
    checkAcceptable(obj)
    val n = new Node(obj)
    if (index == 0) {
      n.next = head
      head = n
    } else {
      var cur = head
      for (_ <- 0 until index - 1) cur = cur.next
      n.next = cur.next
      cur.next = n
    }
    size += 1
  }

  def remove(index: Int): AnyRef = {
    checkIndex(index)
    val removed: AnyRef =
      if (index == 0) {
        val v = head.value
        head = head.next
        v
      } else {
        var cur = head
        for (_ <- 0 until index - 1) cur = cur.next
        val v = cur.next.value
        cur.next = cur.next.next
        v
      }
    size -= 1
    removed
  }

  private def checkIndex(index: Int): Unit = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(s"index=$index, size=$size")
  }

  def forEach(action: DoWith): Unit = {
    var cur = head
    while (cur != null) {
      action.doWith(cur.value)
      cur = cur.next
    }
  }

  def firstThat(test: TestIt): AnyRef = {
    var cur = head
    while (cur != null) {
      if (test.testIt(cur.value)) return cur.value
      cur = cur.next
    }
    null
  }

  def sort(comp: Comparator): Unit = {
    if (size <= 1) return
    val arr = new Array[AnyRef](size)
    var cur = head
    for (i <- arr.indices) {
      arr(i) = cur.value
      cur = cur.next
    }
    quickSort(arr, 0, arr.length - 1, comp)
    cur = head
    for (i <- arr.indices) {
      cur.value = arr(i)
      cur = cur.next
    }
  }

  private def quickSort(a: Array[AnyRef], lo: Int, hi: Int, comp: Comparator): Unit = {
    if (lo >= hi) return
    val p = partition(a, lo, hi, comp)
    quickSort(a, lo, p - 1, comp)
    quickSort(a, p + 1, hi, comp)
  }

  private def partition(a: Array[AnyRef], lo: Int, hi: Int, comp: Comparator): Int = {
    val pivot = a(hi)
    var i = lo - 1
    for (j <- lo until hi) {
      if (comp.compare(a(j), pivot) <= 0) {
        i += 1
        val tmp = a(i)
        a(i) = a(j)
        a(j) = tmp
      }
    }
    val tmp = a(i + 1)
    a(i + 1) = a(hi)
    a(hi) = tmp
    i + 1
  }

  @throws[IOException]
  def saveToFile(filename: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(filename))
    try {
      bw.write("{")
      bw.newLine()
      bw.write(s"""  "type": "${jsonEscape(prototype.typeName)}",""")
      bw.newLine()
      bw.write("  \"items\": [")
      bw.newLine()
      var cur = head
      var first = true
      while (cur != null) {
        val sval = prototype.serialize(cur.value)
        val escaped = jsonEscape(sval)
        if (!first) bw.write(",")
        bw.write(s"""    "$escaped"""")
        bw.newLine()
        first = false
        cur = cur.next
      }
      bw.write("  ]")
      bw.newLine()
      bw.write("}")
    } finally {
      bw.close()
    }
  }

  @throws[IOException]
  def saveToBinaryFile(filename: String): Unit = {
    val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))
    try {
      dos.writeUTF(prototype.typeName)
      dos.writeInt(size)
      var cur = head
      while (cur != null) {
        if (cur.value == null) {
          dos.writeBoolean(true)
        } else {
          dos.writeBoolean(false)
          dos.writeUTF(prototype.serialize(cur.value))
        }
        cur = cur.next
      }
      dos.flush()
    } finally {
      dos.close()
    }
  }

  def toArrayList: util.ArrayList[AnyRef] = {
    val res = new util.ArrayList[AnyRef]()
    var cur = head
    while (cur != null) {
      res.add(cur.value)
      cur = cur.next
    }
    res
  }

  def printList(): Unit = {
    print("[")
    var cur = head
    var first = true
    while (cur != null) {
      if (!first) print(", ")
      print(cur.value)
      first = false
      cur = cur.next
    }
    println("]")
  }

  // --- Utility methods (moved inside class or companion) ---

  private def jsonEscape(s: String): String = {
    if (s == null) return ""
    val sb = new java.lang.StringBuilder()
    for (i <- s.indices) {
      val c = s.charAt(i)
      c match {
        case '\\' => sb.append("\\\\")
        case '"'  => sb.append("\\\"")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case _ =>
          if (c < 0x20 || c > 0x7E) sb.append(f"\\u$c%04x")
          else sb.append(c)
      }
    }
    sb.toString
  }
}

// --- Companion object with static methods ---
object SingleLinkedList {

  @throws[IOException]
  def loadFromFile(filename: String, factory: UserFactory): SingleLinkedList = {
    val content = {
      val br = new BufferedReader(new FileReader(filename))
      try {
        val lines = new java.util.ArrayList[String]()
        var line: String = null
        while ({line = br.readLine(); line != null}) {
          lines.add(line)
        }
        lines.asScala.mkString("\n")
      } finally {
        br.close()
      }
    }

    val pType = Pattern.compile("\"type\"\\s*:\\s*\"([^\"]*)\"", Pattern.CASE_INSENSITIVE)
    val mType = pType.matcher(content)
    if (!mType.find()) throw new IOException("Invalid JSON: type not found")
    val tname = unescapeJson(mType.group(1))
    val builder = factory.getBuilderByName(tname)
    if (builder == null) throw new IOException(s"Unknown type in file: $tname")

    val pItems = Pattern.compile("\"items\"\\s*:\\s*\\[(.*?)\\]", Pattern.DOTALL | Pattern.CASE_INSENSITIVE)
    val mItems = pItems.matcher(content)
    val items = if (mItems.find()) {
      parseJsonStringList(mItems.group(1))
    } else {
      throw new IOException("Invalid JSON: items array not found")
    }

    val lst = new SingleLinkedList(builder)
    items.asScala.foreach { s =>
      val un = unescapeJson(s)
      val obj = builder.deserialize(un)
      lst.add(obj)
    }
    lst
  }

  @throws[IOException]
  def loadFromBinaryFile(filename: String, factory: UserFactory): SingleLinkedList = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(filename)))
    try {
      val tname = dis.readUTF()
      val builder = factory.getBuilderByName(tname)
      if (builder == null) throw new IOException(s"Unknown type in file: $tname")
      val n = dis.readInt()
      val lst = new SingleLinkedList(builder)
      for (_ <- 0 until n) {
        if (dis.readBoolean()) {
          lst.add(null)
        } else {
          val s = dis.readUTF()
          val obj = builder.deserialize(s)
          lst.add(obj)
        }
      }
      lst
    } finally {
      dis.close()
    }
  }

  private def unescapeJson(s: String): String = {
    if (s == null) return null
    val sb = new java.lang.StringBuilder()
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c == '\\' && i + 1 < s.length) {
        val nxt = s.charAt(i + 1)
        (nxt: @scala.annotation.switch) match {
          case '\\' => sb.append('\\'); i += 1
          case '"'  => sb.append('"');  i += 1
          case '/'  => sb.append('/');  i += 1
          case 'b'  => sb.append('\b'); i += 1
          case 'f'  => sb.append('\f'); i += 1
          case 'n'  => sb.append('\n'); i += 1
          case 'r'  => sb.append('\r'); i += 1
          case 't'  => sb.append('\t'); i += 1
          case 'u' =>
            if (i + 5 < s.length) {
              val hx = s.substring(i + 2, i + 6)
              try {
                val code = Integer.parseInt(hx, 16)
                sb.append(code.toChar)
                i += 5
              } catch {
                case _: NumberFormatException =>
                  sb.append("\\u")
                  i += 1
              }
            } else {
              sb.append('\\')
            }
          case _ =>
            sb.append(nxt)
            i += 1
        }
      } else {
        sb.append(c)
      }
      i += 1
    }
    sb.toString
  }

  private def parseJsonStringList(s: String): util.ArrayList[String] = {
    val res = new util.ArrayList[String]()
    var i = 0
    val n = s.length
    while (i < n) {
      val c = s.charAt(i)
      if (Character.isWhitespace(c) || c == ',') {
        i += 1
      } else if (c == '"') {
        val cur = new java.lang.StringBuilder()
        i += 1
        var escaped = false
        while (i < n) {
          val ch = s.charAt(i)
          if (!escaped && ch == '\\') {
            escaped = true
            cur.append(ch)
          } else if (!escaped && ch == '"') {
            i += 1
            res.add(cur.toString)
            escaped = false
            return res // break out
          } else {
            cur.append(ch)
            escaped = false
          }
          i += 1
        }
        res.add(cur.toString)
      } else {
        val start = i
        while (i < n && s.charAt(i) != ',') i += 1
        val token = s.substring(start, i).trim
        if (token.nonEmpty) res.add(token)
      }
    }
    res
  }
}