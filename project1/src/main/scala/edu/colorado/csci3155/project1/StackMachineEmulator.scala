package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction

case class LoadIns(s: String) extends StackMachineInstruction

case class StoreIns(s: String) extends StackMachineInstruction

case class PushIns(f: Double) extends StackMachineInstruction

case object AddIns extends StackMachineInstruction

case object SubIns extends StackMachineInstruction

case object MultIns extends StackMachineInstruction

case object DivIns extends StackMachineInstruction

case object ExpIns extends StackMachineInstruction

case object LogIns extends StackMachineInstruction

case object SinIns extends StackMachineInstruction

case object CosIns extends StackMachineInstruction

case object PopIns extends StackMachineInstruction


object StackMachineEmulator {


  /* Function emulateSingleInstruction
      Given a list of doubles to represent a stack
            a map from string to double precision numbers for the environment
      and   a single instruction of type StackMachineInstruction
      Return a tuple that contains the
            modified stack that results when the instruction is executed.
            modified environment that results when the instruction is executed.

      Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
      being executed. Division by zero, log of a non negative number
      Throw an exception or assertion violation when error happens.
   */
  def emulateSingleInstruction(stack: List[Double],
                               env: Environment.t,
                               ins: StackMachineInstruction): (List[Double], Environment.t) = {
    ins match {
      case StoreIns(s) => (List(env.filter { case (name, _) => name == s }.head._2) ++ stack, env)
      case PushIns(s) => (List(s) ++ stack, env)
      case LoadIns(s) =>
        val (head: Double) :: rest = stack
        (rest, env.filter { case (name, _) => name != s } ++ List((s, head)))
      case PopIns =>
        val (_: Double) :: rest = stack
        (rest, env)
      case AddIns =>
        val (a: Double) :: (b: Double) :: rest = stack
        ((b + a) :: rest, env)
      case SubIns =>
        val (a: Double) :: (b: Double) :: rest = stack
        ((b - a) :: rest, env)
      case MultIns =>
        val (a: Double) :: (b: Double) :: rest = stack
        ((b * a) :: rest, env)
      case DivIns =>
        val (a: Double) :: (b: Double) :: rest = stack
        ((b / a) :: rest, env)
      case LogIns =>
        val (a: Double) :: rest = stack
        (math.log(a) :: rest, env)
      case ExpIns =>
        val (a: Double) :: rest = stack
        (math.exp(a) :: rest, env)
      case SinIns =>
        val (a: Double) :: rest = stack
        (math.sin(a) :: rest, env)
      case CosIns =>
        val (a: Double) :: rest = stack
        (math.cos(a) :: rest, env)
    }
  }

  /* Function emulateStackMachine
     Execute the list of instructions provided as inputs using the
     emulateSingleInstruction function.
     Use foldLeft over list of instruction rather than a for loop if you can.
     Return value must be a double that is the top of the stack after all instructions
     are executed.
   */
  def emulateStackMachine(instructionList: List[StackMachineInstruction]): Environment.t = {
    instructionList.foldLeft((List[Double](), Environment.empty)) { case ((stack, env), ins) => emulateSingleInstruction(stack, env, ins) }._2
  }
}