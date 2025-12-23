// `Exercise4.scala`
// Scala exercise scaffolding WITHOUT answers.
// Fill in the TODOs as per the instructions in your exercise sheet.

object Exercise4:

  // ---------------------------
  // Question 1: Expressions + BinOps
  // ---------------------------

  private enum BinOpName:
    case Minus
    case Plus
    case Times
    case LessEq
    case Modulo

  private enum Expr:
    // Core expression AST
    case Constant(value: Int) // Numeric constants
    case Name(name: String) // Reference to a name
    case BinOp(op: BinOpName, arg1: Expr, arg2: Expr) // Primitive binary operation
    case IfNonzero(cond: Expr, caseTrue: Expr, caseFalse: Expr) // Conditional
    case Call(function: Expr, arg: Expr) // Function call
    case Fun(param: String, body: Expr) // Function definition

    // Question 2 additions: Lists and pattern matching on lists
    case Cons(head: Expr, tail: Expr) // Cons list
    case EmptyList // Empty of a Cons list
    // Matches a list
    case Match(scrutinee: Expr, caseEmpty: Expr, headName: String, tailName: String, caseCons: Expr)

    // Question 3 additions: Read/Write memory cells
    case Read(idx: Expr) // Read from position `idx`
    // Write the `value` to position `idx` and then evaluates and returns the `andThen` expression
    case Write(idx: Expr, value: Expr, andThen: Expr)

  import BinOpName.*
  import Expr.*

  // Helper constructors for primitive operations
  private def minus(e1: Expr, e2: Expr) = BinOp(Minus, e1, e2)

  private def plus(e1: Expr, e2: Expr) = BinOp(Plus, e1, e2)

  private def leq(e1: Expr, e2: Expr) = BinOp(LessEq, e1, e2) // 1 if e1 <= e2; 0 otherwise

  private def times(e1: Expr, e2: Expr) = BinOp(Times, e1, e2)

  private def modulo(e1: Expr, e2: Expr) = BinOp(Modulo, e1, e2)

  // ---------------------------
  // Global environment example
  // ---------------------------

  // Example: "div" defined in terms of primitive operations and recursion
  // NOTE: Provided as an example; do not modify unless needed.
  private val divBinding: (String, Expr) = "div" -> Fun("x", Fun("y", IfNonzero(BinOp(LessEq, Name("y"), Name("x")), plus(Constant(1), Call(Call(Name("div"), minus(Name("x"), Name("y"))), Name("y"))), Constant(0))))

  // ---------------------------
  // Question 1: Implement gcd in Expr
  // ---------------------------
  // Hint (Scala): def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

  // TODO: Implement the "gcd" binding as an Expr using the provided AST.
  // Replace ??? with a proper expression using Fun, IfNonzero, Call, modulo, etc.
  private val gcdBinding: (String, Expr) = "gcd" -> ??? // TODO implement me

  // ---------------------------
  // Question 2: Lists, Map, FoldLeft
  // ---------------------------

  // Map:
  // Hint (Scala):
  // def map(ls: List[Int])(f: Int => Int): List[Int] = ls match
  //   case Nil => Nil
  //   case x :: xs => f(x) :: map(xs)(f)

  // TODO: Implement the "map" binding as an Expr using Match, Cons, EmptyList, Fun, Call.
  private val mapBinding: (String, Expr) = "map" -> ??? // TODO implement me

  // FoldLeft:
  // Hint (Scala):
  // def foldLeft(ls: List[Int])(acc: Int)(f: (Int, Int) => Int): Int = ls match
  //   case Nil => acc
  //   case x :: xs => foldLeft(xs)(f(acc, x))(f)

  // TODO: Implement the "foldLeft" binding as an Expr using Match, Fun, Call, etc.
  private val foldLeftBinding: (String, Expr) = "foldLeft" -> ??? // TODO implement me

  // ---------------------------
  // Question 3: CAS (Compare-And-Swap)
  // ---------------------------

  // Assume we have a global array of memory that can be accessed by index:
  // val mem: Array[Int] = ???

  // Hint (Scala):
  // def CAS(idx: Int)(old: Int)(nw: Int): Int =
  //   if mem(idx) != old then 0
  //   else { mem(idx) = nw; 1 }

  // NOTE: In this exercise language, use Read and Write forms to encode CAS semantics.
  // Read(idx) should be used to compare with `old`. Write(idx, nw, andThen) writes and then returns `andThen`.
  // TODO: Implement the "CAS" binding as an Expr using Fun, Read, Write, IfNonzero, etc.
  private val casBinding: (String, Expr) = "CAS" -> ??? // TODO implement me

  // ---------------------------
  // Optional: aggregate global environment
  // ---------------------------

  // You can include all top-level bindings in a global environment sequence.
  // All definitions can reference all names in this global environment.
  private val globalEnv: Seq[(String, Expr)] = Seq(divBinding, gcdBinding, // TODO implemented by you
    mapBinding, // TODO implemented by you
    foldLeftBinding, // TODO implemented by you
    casBinding // TODO implemented by you
  )
