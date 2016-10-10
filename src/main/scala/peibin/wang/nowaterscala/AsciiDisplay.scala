package peibin.wang.nowaterscala

import scala.annotation.tailrec

/**
  * Created by admin on 2016/10/10.
  */
object AsciiDisplay {

  case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    asciiDisplay(root, true)
  }

  def asciiDisplay(node: TreeNode[String], isLastChildren: Boolean): Seq[String] = {
    if (node.children == Nil) Seq(s"+-${node.data}")
    else {
      val init = node.children.init
      val last = node.children.last
      val prefix = if (!isLastChildren) "| " else "  "
      val lastItem = if (!isLastChildren) Some("| ") else None
      init.foldLeft(Seq(s"+-${node.data}"))((x, y) => {
        val temp = asciiDisplay(y, false).map(s => prefix + s)
        x ++ temp
      }) ++ asciiDisplay(last, true).map(s => prefix + s) ++ lastItem
    }
  }

  def main(args: Array[String]): Unit = {
    asciiDisplay(TreeNode("Root",
      children = List(
        TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).foreach(println)

    println("---------------------------------------------------")

    asciiDisplay(TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).foreach(println)
  }
}
