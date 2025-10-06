// Exercise2.scala
//
// A single Scala 3 source that implements every exercise in the PDF.
// It contains all required functions, helpers, and a generic binary‑search tree.
//
// ────────────────────────────────────────────────────────────────

object Exercise2 {
  // question 1
  private type NodeId = Int
  private type DirectedEdge = (NodeId, NodeId)
  private type DirectedGraph = List[DirectedEdge]

  private def triangles(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] =
    for (x1, x2) <- edges; (y1, y2) <- edges; (z1, z2) <- edges
        if x2 == y1 && y2 == z1 && z2 == x1 && x1 < y1 && x1 < z1
    yield (x1, y1, z1)

  // question 2
  private enum Expr:
    case Number(x: Int)
    case Var(name: String)
    case Sum(e1: Expr, e2: Expr)
    case Prod(e1: Expr, e2: Expr)

  import Expr.*

  private def deriv(expr: Expr, v: String): Expr = expr match {
    case Number(x) => Number(0)
    case Var(name) => Number(if v == name then 1 else 0)
    case Sum(e1, e2) => Sum(deriv(e1, v), deriv(e2, v))
    case Prod(e1, e2) => Sum(Prod(deriv(e1, v), e2), Prod(deriv(e2, v), e1))
  }

  // question 3
  private def simplify(expr: Expr): Expr = {
    expr match {
      case Number(_) | Var(_) => expr
      case Sum(e1, e2) =>
        def collect_terms(expr: Sum): List[Expr] = expr match {
          case Sum(e1: Sum, e2: Sum) => collect_terms(e1) ::: collect_terms(e2)
          case Sum(e1: Sum, e2) => e2 :: collect_terms(e1)
          case Sum(e1, e2: Sum) => e1 :: collect_terms(e2)
          case Sum(e1, e2) => List(e1, e2)
        }

        var terms = collect_terms(Sum(e1, e2)).groupMapReduce(identity)(Function.const(1))(_ + _)

        def multiply_and_simplify_terms(terms: Map[Expr, Int]): Map[Expr, Int] =
          (terms.foldLeft((0, Map[Expr, Int]().withDefaultValue(0)))((left, right) => right match {
            case (Number(x), times) => (left._1 + x * times, left._2)
            case (expr, times) =>
              val expr2 = simplify(expr)
              (left._1, left._2 + (expr -> (left._2(expr) + times)))
          }) match {
            case (0, terms) => terms
            case (constant, terms) => terms + (Number(constant) -> (terms(Number(constant)) + 1))
          }).map((expr, times) => Prod(Number(times), expr)).map(simplify).map {
            case Prod(Number(times), expr) => expr -> times
            case Prod(expr, Number(times)) => expr -> times
            case expr => expr -> 1
          }.groupMapReduce((expr, _) => expr)((_, times) => times)(_ + _).filter((_, times) => times != 0)

        var prevReduced = Map[Expr, Int]()
        while terms != prevReduced do
          prevReduced = terms
          terms = multiply_and_simplify_terms(terms)
        terms.map((expr, times) => Prod(Number(times), expr)).map(simplify).reduceLeftOption(Sum.apply).getOrElse(Number(0))
      case Prod(e1, e2) =>
        def collect_terms(expr: Prod): List[Expr] = expr match {
          case Prod(e1: Prod, e2: Prod) => collect_terms(e1) ::: collect_terms(e2)
          case Prod(e1: Prod, e2) => e2 :: collect_terms(e1)
          case Prod(e1, e2: Prod) => e1 :: collect_terms(e2)
          case Prod(e1, e2) => List(e1, e2)
        }

        val terms = collect_terms(Prod(e1, e2)).map(simplify)
          .foldLeft((1, List[Expr]()))((left, right) => right match {
            case Number(x) => (left._1 * x, left._2)
            case expr => (left._1, expr :: left._2)
          }) match {
          case (0, terms) => List(Number(0))
          case (1, terms) => terms
          case (constant, terms) => Number(constant) :: terms
        }
        terms.reduceLeftOption(Prod.apply).getOrElse(Number(1))
    }
  }

  @main
  def main(): Unit = {
    // question 1
    println(triangles(List((1, 2), (2, 3), (3, 1)))) // List((1, 2, 3))
    println(triangles(List((1, 2), (2, 3), (3, 1), (2, 4), (4, 1)))) // List((1, 2, 3), (1, 2, 4))
    println(triangles(List((4, 2), (2, 3), (3, 4), (2, 1), (1, 4)))) // List((2, 3, 4), (1, 4, 2))
    // question 2
    println(deriv(Sum(Prod(Var("x"), Var("x")), Var("y")), "x")) // Sum(Sum(Prod(Number(1),Var(x)),Prod(Number(1),Var(x))),Number(0))
    // question 3
    println(simplify(deriv(Sum(Prod(Var("x"), Var("x")), Var("y")), "x"))) // Prod(Number(2),Var(x))
    println(simplify(Sum(Number(1), Sum(Var("x"), Number(2))))) // Sum(Number(3),Var(x))
    println(simplify(Sum(Sum(Number(1), Number(2)), Sum(Number(3), Number(4))))) // Number(10)
    println(simplify(Sum(Prod(Number(-1), Var("x")), Prod(Sum(Number(2), Number(3)), Sum(Var("x"), Var("x")))))) // Prod(Number(9),Var(x))
    println(simplify(Sum(Prod(Number(-1), Prod(Var("x"), Var("x"))), Prod(Sum(Number(2), Number(3)), Prod(Var("x"), Var("x")))))) // does not work: Sum(Prod(Prod(Number(5),Var(x)),Var(x)),Prod(Prod(Number(-1),Var(x)),Var(x)))
  }
}
