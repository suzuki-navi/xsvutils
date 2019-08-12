// mulang-bin-sources: scala

sealed abstract class Either2[+A, +B] extends Product with Serializable;

case class Option2A[+A, +B](value: A) extends Either2[A, B];
case class Option2B[+A, +B](value: B) extends Either2[A, B];

sealed abstract class Either3[+A, +B, +C] extends Product with Serializable;

case class Option3A[+A, +B, +C](value: A) extends Either3[A, B, C];
case class Option3B[+A, +B, +C](value: B) extends Either3[A, B, C];
case class Option3C[+A, +B, +C](value: C) extends Either3[A, B, C];

sealed abstract class Either4[+A, +B, +C, +D] extends Product with Serializable;

case class Option4A[+A, +B, +C, +D](value: A) extends Either4[A, B, C, D];
case class Option4B[+A, +B, +C, +D](value: B) extends Either4[A, B, C, D];
case class Option4C[+A, +B, +C, +D](value: C) extends Either4[A, B, C, D];
case class Option4D[+A, +B, +C, +D](value: D) extends Either4[A, B, C, D];

sealed abstract class Either5[+A, +B, +C, +D, +E] extends Product with Serializable;

case class Option5A[+A, +B, +C, +D, +E](value: A) extends Either5[A, B, C, D, E];
case class Option5B[+A, +B, +C, +D, +E](value: B) extends Either5[A, B, C, D, E];
case class Option5C[+A, +B, +C, +D, +E](value: C) extends Either5[A, B, C, D, E];
case class Option5D[+A, +B, +C, +D, +E](value: D) extends Either5[A, B, C, D, E];
case class Option5E[+A, +B, +C, +D, +E](value: E) extends Either5[A, B, C, D, E];

