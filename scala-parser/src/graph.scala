// mulang-bin-sources: scala

object Graph {

  def unshift[T](payload: T, nexts: IndexedSeq[Edge[T]], prevCount: Int):
    (IndexedSeq[Edge[T]], Node[T]) = {
    val node = Node[T](payload, nexts, prevCount);
    val edges = (0 until prevCount).map { i =>
      Edge[T](node, i);
    }
    (edges, node);
  }

  def unshiftEdges[T](payload: T, nexts: IndexedSeq[Edge[T]], prevCount: Int):
    IndexedSeq[Edge[T]] = {
    unshift(payload, nexts, prevCount)._1;
  }

  def unshiftNode[T](payload: T, nexts: IndexedSeq[Edge[T]], prevCount: Int = 0):
    Node[T] = {
    unshift(payload, nexts, prevCount)._2;
  }

  case class Node[+T](payload: T, nexts: IndexedSeq[Edge[T]], prevCount: Int);

  case class Edge[+T](next: Node[T], id: Int);

  def heads[T](lasts: IndexedSeq[Node[T]]): IndexedSeq[T] = {
    heads(lasts, Nil, Vector.empty);
  }

  @scala.annotation.tailrec
  private def heads[T](nodes1: IndexedSeq[Node[T]], nodes2: List[Node[T]], result: IndexedSeq[T]):
    IndexedSeq[T] = {
    if (nodes1.isEmpty) {
      result;
    } else {
      val h = nodes1.head;
      if (nodes2.contains(h)) {
        heads(nodes1.tail, nodes2, result);
      } else if (h.nexts.isEmpty) {
        heads(nodes1.tail, h :: nodes2, result :+ h.payload);
      } else {
        heads(h.nexts.map(_.next) ++ nodes1.tail, h :: nodes2, result);
      }
    }
  }

}

