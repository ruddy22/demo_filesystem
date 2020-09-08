package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd

    if (wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      // mkdir smth/innerDir/anotherInnerDir
      state.setMessage(s"$name must not contain separators!")
    } else if (checkIllegal(name)) {
      // mkdir ..aFolder
      state.setMessage(s"$name: illegal entry name!")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(str: String): Boolean = {
    name.contains(".") // TODO "./\ and etc"
  }

  def doMkdir(state: State, str: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }

      /**
       * illustration
       * /a/b
       *     (contents)
       *     (add new entry) /e
       *
       * newRoot = updateStructure(/(root), ["a", "b"], /e)
       *   => path.isEmpty? // no
       *   => oldEntry <= /a
       *   root.replaceEntry("a", updateStructure(/a, ["b"], /e)
       *     => path.isEmpty? // no
       *     => oldEntry <= /b
       *     /a.replaceEntry("b", updateStructure(/b, [], /e)
       *       => path.isEmpty? // yes
       *       => /b.addEntry(/e)
       */
    }

    val wd = state.wd

    // 1. all the directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath

    // 2. create new directory entry in the wd
    val newDir = Directory.empty(wd.path, name)

    // 3. update whole directory structure starting from the root
    // (the directory structure is IMMUTABLE)
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    // 4. find new working directory INSTANCE given wd's full path, in the NEW directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }
}
