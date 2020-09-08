package com.rtjvm.scala.oop.files

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def hasEntry(entry: String): Boolean = ???

  def getAllFoldersInPath: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

  def findDescendant(path: List[String]): Directory = ???

  def addEntry(newEntry: DirEntry): Directory = ???

  def findEntry(entryName: String): DirEntry = ???

  def replaceEntry(entryName: String, directory: Directory): Directory = ???

  def asDirectory: Directory = this
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"
  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())

  def ROOT: Directory = Directory.empty("", "")
}