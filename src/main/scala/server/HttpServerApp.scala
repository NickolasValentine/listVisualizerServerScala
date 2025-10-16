package server

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import data.{UserFactory, UserType}
import list.SingleLinkedList
import java.io._
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util
import java.util.regex.Pattern
import scala.jdk.CollectionConverters._

object HttpServerApp {
  private val factory = new UserFactory
  private var currentList: SingleLinkedList = null
  private var currentPrototype: UserType = null
  private val listLock = new Object

  @throws[Exception]
  def start(): Unit = {
    val port = 5865
    val addr = new InetSocketAddress("127.0.0.1", port)
    val server = HttpServer.create(addr, 0)
    println(s"Backend starting on http://127.0.0.1:$port")
    server.createContext("/health", handleHealth)
    server.createContext("/types", handleTypes)
    server.createContext("/list", handleListRoot)
    server.createContext("/list/items", handleListItems)
    server.createContext("/list/add", handleAdd)
    server.createContext("/list/insert", handleInsert)
    server.createContext("/list/remove", handleRemove)
    server.createContext("/list/get", handleGet)
    server.createContext("/list/find", handleFind)
    server.createContext("/list/sort", handleSort)
    server.createContext("/list/save", handleSave)
    server.createContext("/list/load", handleLoad)
    server.setExecutor(java.util.concurrent.Executors.newCachedThreadPool())
    server.start()
    println("Ready.")
  }

  // --- Utility methods ---

  @throws[IOException]
  private def readBody(ex: HttpExchange): String = {
    val is = ex.getRequestBody
    val baos = new ByteArrayOutputStream()
    val buf = new Array[Byte](4096)
    var r = 0
    while ({ r = is.read(buf); r > 0 }) {
      baos.write(buf, 0, r)
    }
    new String(baos.toByteArray, StandardCharsets.UTF_8)
  }

  @throws[IOException]
  private def sendJson(ex: HttpExchange, code: Int, json: String): Unit = {
    val b = json.getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.set("Content-Type", "application/json; charset=utf-8")
    ex.sendResponseHeaders(code, b.length)
    val os = ex.getResponseBody
    os.write(b)
    os.close()
  }

  @throws[IOException]
  private def sendError(ex: HttpExchange, code: Int, msg: String): Unit = {
    val json = s"""{"error":"${jsonEscape(msg)}"}"""
    sendJson(ex, code, json)
  }

  // --- Handlers ---

  private def handleHealth(ex: HttpExchange): Unit = {
    if (!"GET".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    sendJson(ex, 200, """{"ok":true}""")
  }

  private def handleTypes(ex: HttpExchange): Unit = {
    if (!"GET".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val names = factory.getTypeNameList
    val sb = new java.lang.StringBuilder()
    sb.append("[")
    var first = true
    for (s <- names.asScala) {
      if (!first) sb.append(",")
      sb.append("\"").append(jsonEscape(s)).append("\"")
      first = false
    }
    sb.append("]")
    sendJson(ex, 200, sb.toString)
  }

  private def handleListRoot(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val tpe = extractJsonField(body, "type")
    if (tpe == null) {
      sendError(ex, 400, "no type")
      return
    }
    val ut = factory.getBuilderByName(tpe)
    if (ut == null) {
      sendError(ex, 400, s"unknown type: $tpe")
      return
    }
    listLock.synchronized {
      currentPrototype = ut
      currentList = new SingleLinkedList(ut)
    }
    sendJson(ex, 200, """{"ok":true}""")
  }

  private def handleListItems(ex: HttpExchange): Unit = {
    if (!"GET".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val (list, proto) = listLock.synchronized {
      (currentList, currentPrototype)
    }
    if (list == null || proto == null) {
      sendError(ex, 400, "list not initialized")
      return
    }
    val items = list.toArrayList
    val sb = new java.lang.StringBuilder()
    sb.append("[")
    var first = true
    for (o <- items.asScala) {
      val s = proto.serialize(o)
      if (!first) sb.append(",")
      sb.append("\"").append(jsonEscape(s)).append("\"")
      first = false
    }
    sb.append("]")
    sendJson(ex, 200, sb.toString)
  }

  private def handleAdd(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val value = extractJsonField(body, "value")
    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      val obj = try {
        currentPrototype.deserialize(if (value == null) "" else value)
      } catch {
        case parseEx: Exception =>
          val typeName = Option(currentPrototype).map(_.typeName).getOrElse("unknown")
          val msg = s"""Invalid value for type "$typeName": ${Option(parseEx.getMessage).getOrElse("")}"""
          sendError(ex, 400, msg)
          return
      }
      try {
        currentList.add(obj)
        sendJson(ex, 200, """{"ok":true}""")
      } catch {
        case ia: IllegalArgumentException =>
          sendError(ex, 400, ia.getMessage)
        case _: Exception =>
          sendError(ex, 500, "Internal error on add")
      }
    }
  }

  private def handleInsert(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val sval = extractJsonField(body, "value")
    val sidx = extractJsonField(body, "index")
    if (sidx == null || sidx.trim.isEmpty) {
      sendError(ex, 400, "Index is required")
      return
    }
    val idx = try {
      sidx.trim.toInt
    } catch {
      case _: NumberFormatException =>
        sendError(ex, 400, "Invalid index: must be integer")
        return
    }

    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      val size = currentList.getSize // ← ИСПРАВЛЕНО: было .size
      if (idx < 0 || idx > size) {
        sendError(ex, 400, s"Index out of range: must be between 0 and $size")
        return
      }
      val obj = try {
        currentPrototype.deserialize(if (sval == null) "" else sval)
      } catch {
        case parseEx: Exception =>
          val typeName = Option(currentPrototype).map(_.typeName).getOrElse("unknown")
          val msg = s"""Invalid value for type "$typeName": ${Option(parseEx.getMessage).getOrElse("")}"""
          sendError(ex, 400, msg)
          return
      }
      try {
        currentList.insert(idx, obj)
        sendJson(ex, 200, """{"ok":true}""")
      } catch {
        case iob: IndexOutOfBoundsException =>
          sendError(ex, 400, iob.getMessage)
        case ia: IllegalArgumentException =>
          sendError(ex, 400, ia.getMessage)
        case _: Exception =>
          sendError(ex, 500, "Internal error on insert")
      }
    }
  }

  private def handleRemove(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val sidx = extractJsonField(body, "index")
    val idx = try {
      if (sidx == null) -1 else sidx.toInt
    } catch {
      case _: Exception => -1
    }

    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      try {
        val removed = currentList.remove(idx)
        val serialized = currentPrototype.serialize(removed)
        sendJson(ex, 200, s"""{"ok":true, "removed":"${jsonEscape(serialized)}"}""")
      } catch {
        case iob: IndexOutOfBoundsException =>
          sendError(ex, 400, iob.getMessage)
      }
    }
  }

  private def handleGet(ex: HttpExchange): Unit = {
    if (!"GET".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val q = ex.getRequestURI.getQuery
    val qm = parseQuery(q)
    val sidx = qm.get("index")
    val idx = try {
      if (sidx == null) -1 else sidx.toInt
    } catch {
      case _: Exception => -1
    }

    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      try {
        val value = currentList.get(idx)
        sendJson(ex, 200, s"""{"value":"${jsonEscape(currentPrototype.serialize(value))}"}""")
      } catch {
        case iob: IndexOutOfBoundsException =>
          sendError(ex, 400, iob.getMessage)
      }
    }
  }

  private def handleFind(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val value = Option(extractJsonField(body, "value")).getOrElse("")

    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      val items = currentList.toArrayList
      val it = items.iterator()
      var i = 0
      while (it.hasNext) {
        val o = it.next()
        val s = Option(currentPrototype.serialize(o)).getOrElse("")
        if (s == value) {
          sendJson(ex, 200, s"""{"index":$i}""")
          return
        }
        i += 1
      }
      sendJson(ex, 200, """{"index":-1}""")
    }
  }

  private def handleSort(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      currentList.sort(currentPrototype.getTypeComparator)
      sendJson(ex, 200, """{"ok":true}""")
    }
  }

  private def handleSave(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val filename = extractJsonField(body, "filename")
    val format = Option(extractJsonField(body, "format")).getOrElse("json")
    if (filename == null || filename.trim.isEmpty) {
      sendError(ex, 400, "filename required")
      return
    }
    val safeName = sanitizeFilename(filename)
    listLock.synchronized {
      if (currentList == null || currentPrototype == null) {
        sendError(ex, 400, "list not initialized")
        return
      }
      try {
        if ("bin".equalsIgnoreCase(format)) {
          currentList.saveToBinaryFile(safeName)
        } else {
          currentList.saveToFile(safeName)
        }
        sendJson(ex, 200, """{"ok":true}""")
      } catch {
        case io: IOException =>
          sendError(ex, 500, s"IO error: ${io.getMessage}")
      }
    }
  }

  private def handleLoad(ex: HttpExchange): Unit = {
    if (!"POST".equalsIgnoreCase(ex.getRequestMethod)) {
      ex.sendResponseHeaders(405, -1)
      return
    }
    val body = readBody(ex)
    val filename = extractJsonField(body, "filename")
    val format = Option(extractJsonField(body, "format")).getOrElse("json")
    if (filename == null || filename.trim.isEmpty) {
      sendError(ex, 400, "filename required")
      return
    }
    val candidate = new File(filename)
    val inFile = if (candidate.isAbsolute) candidate else new File(sanitizeFilename(filename))
    if (!inFile.exists()) {
      sendError(ex, 400, s"file not found: ${inFile.getAbsolutePath}")
      return
    }

    try {
      val loaded = if ("bin".equalsIgnoreCase(format)) {
        SingleLinkedList.loadFromBinaryFile(inFile.getAbsolutePath, factory)
      } else {
        SingleLinkedList.loadFromFile(inFile.getAbsolutePath, factory)
      }
      listLock.synchronized {
        currentList = loaded
        currentPrototype = if (loaded != null) loaded.prototype else null
      }
      val typeName = Option(currentPrototype).map(_.typeName).getOrElse("")
      sendJson(ex, 200, s"""{"ok":true, "type":"${jsonEscape(typeName)}"}""")
    } catch {
      case io: IOException =>
        sendError(ex, 500, s"IO error: ${io.getMessage}")
      case se: SecurityException =>
        sendError(ex, 500, s"Security error: ${se.getMessage}")
    }
  }

  // --- Utilities ---

  private def sanitizeFilename(fname: String): String = {
    if (fname == null || fname.trim.isEmpty) "data.json"
    else fname.trim
  }

  private def extractJsonField(body: String, field: String): String = {
    if (body == null) return null
    var m = Pattern.compile(s""""$field"\\s*:\\s*"([^"]*)"""").matcher(body)
    if (m.find()) return m.group(1)
    m = Pattern.compile(s""""$field"\\s*:\\s*(-?\\d+)""").matcher(body)
    if (m.find()) return m.group(1)
    null
  }

  private def parseQuery(q: String): util.Map[String, String] = {
    val map = new util.HashMap[String, String]()
    if (q != null) {
      for (part <- q.split("&")) {
        val kv = part.split("=", 2)
        if (kv.length == 2) map.put(kv(0), kv(1))
      }
    }
    map
  }

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