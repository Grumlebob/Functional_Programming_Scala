package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    this match
      case Leaf(_) => 0
      case Branch(l, r) =>
        val leftDepth = l.depth
        val rightDepth = r.depth
        1 + leftDepth.max(rightDepth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B =
    // 'f' is a transformation function that takes a value of type A (the type of the elements in the structure)
    // and converts it into a value of type B (the type we want to accumulate or produce).
    //
    // 'g' is a combining function that takes two values of type B and combines them into a single value of type B.
    // It is used to merge the results from the left and right branches of the structure.
    this match
      case Leaf(a) => f(a) // If the current element is a Leaf, apply the transformation function 'f' to the value 'a'
      case Branch(l, r) => g(l.fold(f, g), r.fold(f, g)) // If the current element is a Branch, recursively fold
    // the left and right branches, then combine the results
    // using the combining function 'g'

  def sizeViaFold: Int =
    def leafSize(a: A): Int = 1
    def branchSize(left: Int, right: Int): Int = 1 + left + right
    fold(leafSize, branchSize)

  def depthViaFold: Int =
    def leafDepth(a: A): Int = 0
    def branchDepth(left: Int, right: Int): Int = 1 + left.max(right)
    fold(leafDepth, branchDepth)

  def mapViaFold[B](f: A => B): Tree[B] =
    def leafMap(a: A): Tree[B] = Leaf(f(a))
    def branchMap(left: Tree[B], right: Tree[B]): Tree[B] = Branch(left, right)
    fold(leafMap, branchMap)

object Tree:

  def size[A](t: Tree[A]): Int = t.size

  extension (t: Tree[Int])
    def firstPositive: Int =
      t match
        case Tree.Leaf(i) => i
        case Tree.Branch(l, r) =>
          val lpos = l.firstPositive
          if lpos > 0 then lpos else r.firstPositive

    def maximum: Int =
      t match
        case Tree.Leaf(n) => n
        case Tree.Branch(l, r) => l.maximum.max(r.maximum)

    def maximumViaFold: Int =
      def max(a: Int, b: Int): Int = a.max(b)
      t.fold(identity, max)


object TreeTest:

  def testSize(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    assert(tree.size == 5) // 3 leaves + 2 branches = 5 nodes
    assert(Tree.size(tree) == 5) // Testing the object method
  }

  def testDepth(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    assert(tree.depth == 2) // Depth of the tree is 2
  }

  def testMap(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    val expected = Tree.Branch(
      Tree.Branch(Tree.Leaf(2), Tree.Leaf(4)),
      Tree.Leaf(6)
    )
    assert(tree.map(_ * 2) == expected)
  }

  def testFold(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    val sum = tree.fold(identity, _ + _)
    assert(sum == 6) // Sum of all values in the tree
  }

  def testSizeViaFold(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    assert(tree.sizeViaFold == 5) // 3 leaves + 2 branches = 5 nodes
  }

  def testDepthViaFold(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    assert(tree.depthViaFold == 2) // Depth of the tree is 2
  }

  def testMapViaFold(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)),
      Tree.Leaf(3)
    )
    val expected = Tree.Branch(
      Tree.Branch(Tree.Leaf(2), Tree.Leaf(4)),
      Tree.Leaf(6)
    )
    assert(tree.mapViaFold(_ * 2) == expected)
  }

  def testMaximum(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(5)),
      Tree.Leaf(3)
    )
    assert(tree.maximum == 5) // Maximum value in the tree is 5
  }

  def testMaximumViaFold(): Unit = {
    val tree = Tree.Branch(
      Tree.Branch(Tree.Leaf(1), Tree.Leaf(5)),
      Tree.Leaf(3)
    )
    assert(tree.maximumViaFold == 5) // Maximum value in the tree is 5
  }

  def runTests(): Unit = {
    testSize()
    testDepth()
    testMap()
    testFold()
    testSizeViaFold()
    testDepthViaFold()
    testMapViaFold()
    testMaximum()
    testMaximumViaFold()
    println("All tests passed!")
  }

@main def runTreeTests(): Unit =
  TreeTest.runTests()
