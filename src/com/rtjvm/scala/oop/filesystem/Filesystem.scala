package com.rtjvm.scala.oop.filesystem

import java.util.Scanner

import com.rtjvm.scala.oop.commands.Command
import com.rtjvm.scala.oop.files.Directory

object Filesystem extends App {
  val root = Directory.ROOT
  /**
   * TODO : improvement
   * more functional way - use infinite sequences (i.e. streams)
   * ```
   * io.Source.stdin.getLines().foldLeft(State(root, root))((curState, newLine) => {
   *   curState.show
   *   Command.from(newLine).apply(curState)
   *   if ("exit".equals(newLine)) {
   *     System.exit(status = 0)
   *   }
   * })
   * ```
   */
  val scanner = new Scanner(System.in)

  var state = State(root, root)

  while(true) {
    state.show()
    val input = scanner.nextLine()
    state = Command.from(input).apply(state)
    if ("exit".equals(input)) {
      System.exit(0)
    }
    // eq to
    // val command = Command.from(input)
    // state = command(state)
  }
}
