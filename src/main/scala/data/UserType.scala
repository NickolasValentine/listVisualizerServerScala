package data

import java.io.IOException
import java.io.InputStreamReader

trait UserType {
  def typeName: String

  def create: AnyRef

  def cloneObject(obj: AnyRef): AnyRef

  @throws[IOException]
  def readValue(in: InputStreamReader): AnyRef

  def parseValue(ss: String): AnyRef

  def getTypeComparator: Comparator

  def serialize(obj: AnyRef): String

  def deserialize(s: String): AnyRef
}