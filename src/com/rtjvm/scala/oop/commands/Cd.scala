package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    // 1. find root
    val root = state.root
    val wd = state.wd

    // 2. find absolute path of folder I want to cd to
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // 3. find a directory to cd to, given path
    val destinationDirectory = doFindEntry(root, absolutePath);

    // 4. change state given the new directory
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory") // failure
    else
      State(root, destinationDirectory.asDirectory) // success
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) nextDir
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      /**
       * /a/b => List("a", "b")
       * path.isEmpty =>
       *   cRT(["b"], result <= List :+ "a" => ["a"]) =>
       *     path.isEmpty =>
       *       cRT([], result <= ["a"] :+ "b" => ["a", "b"]) =>
       *         path.isEmpty => result
       *
       *  /a/.. => ["a", ".."]
       *  path.isEmpty =>
       *    crt([".."], [] :+ "a")
       *      path.isEmpty =>
       *        crt([], []) =>
       *          path.isEmpty => []
       *
       *  /a/b/.. => ["a", "b", ".."]
       *  path.isEmpty =>
       *    crt(["b", ".."], [] :+ "a")
       *      path.isEmpty =>
       *        crt([".."], ["a", "b"]) =>
       *          path.isEmpty =>
       *            crt([], ["a"]) =>
       *              path.isEmpty => ["a"]
       */
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    // 1. tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // 1.5. collapse the relative tokens
    val newTokens = collapseRelativeTokens(tokens, List())

    // 2. navigate to the correct entry
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
