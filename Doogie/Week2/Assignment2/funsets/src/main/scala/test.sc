/**
  * 2. Purely Functional Sets.
  */
/**
  * We represent a set by its characteristic function, i.e.
  * its `contains` predicate.
  */
type Set = Int => Boolean

/**
  * Indicates whether a set contains a given element.
  */
def contains(s: Set, elem: Int): Boolean = s(elem)

/**
  * Returns the set of the one given element.
  */
def singletonSet(elem: Int): Set = {
  x: Int => Boolean
    if (x == elem) true
    else false
}

/**
  * Returns the union of the two given sets,
  * the sets of all elements that are in either `s` or `t`.
  */
def union(s: Set, t: Set): Set = {
  x: Int => Boolean
    if (contains(s, x) || contains(t, x)) true
    else false
}

/**
  * Returns the intersection of the two given sets,
  * the set of all elements that are both in `s` and `t`.
  */
def intersect(s: Set, t: Set): Set = {
  x: Int => Boolean
    if (contains(s, x) && contains(t, x)) true
    else false
}

/**
  * Returns the difference of the two given sets,
  * the set of all elements of `s` that are not in `t`.
  */
def diff(s: Set, t: Set): Set = {
  x: Int => Boolean
    if (contains(s, x) && !contains(t, x)) true
    else false
}

/**
  * Returns the subset of `s` for which `p` holds.
  */
def filter(s: Set, p: Int => Boolean): Set = {
  x: Int => Boolean
    if (contains(s, x) && p(x)) true
    else false
}

/**
  * The bounds for `forall` and `exists` are +/- 1000.
  */
val bound = 1000

/**
  * Returns whether all bounded integers within `s` satisfy `p`.
  */
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (contains(s, a) && !p(a)) false
    else if (a == 1001) true
    else iter(a + 1)
  }
  iter(-1000)
}

/**
  * Returns whether there exists a bounded integer within `s`
  * that satisfies `p`.
  */
def exists(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (contains(s, a) && p(a)) true
    else if (a == 1001) false
    else iter(a + 1)
  }
  iter(-1000)
}

/**
  * Returns a set transformed by applying `f` to each element of `s`.
  */
def map(s: Set, f: Int => Int): Set = {
  def iter(set: Set, a: Int): Set = {
    if (contains(s, a)) iter(union(set, singletonSet(f(a))), a + 1)
    else if (a == 1001) set
    else iter(set, a + 1)
  }
  iter(null, -1000)
}

/**
  * Prints the contents of a set on the console.
  */
def printSet(s: Set) {
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  println(toString(s))
}

val s = union(singletonSet(3), singletonSet(1))
val s2 = union(singletonSet(2), singletonSet(4))
val s3 = union(s, s2)
contains(s3,3)

