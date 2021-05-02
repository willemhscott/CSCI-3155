package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Let(ident, e1, e2) => compileToStackMachineCode(e1) ++ List(LoadIns(ident)) ++ compileToStackMachineCode(e2)
            case Ident(s) => List(StoreIns(s))
            case Minus(e1, e2) => compileToStackMachineCode(e1)  ++ compileToStackMachineCode(e2) ++ List(SubIns)
            case Plus(e1, e2) => compileToStackMachineCode(e1)  ++ compileToStackMachineCode(e2) ++ List(AddIns)
            case Mult(e1, e2) => compileToStackMachineCode(e1)  ++ compileToStackMachineCode(e2) ++ List(MultIns)
            case Div(e1, e2) => compileToStackMachineCode(e1)  ++ compileToStackMachineCode(e2) ++ List(DivIns)
            case Const(f) => List(PushIns(f))
            case Exp(e) => compileToStackMachineCode(e) ++ List(ExpIns)
            case Log(e) => compileToStackMachineCode(e) ++ List(LogIns)
            case Sine(e) => compileToStackMachineCode(e) ++ List(SinIns)
            case Cosine(e) => compileToStackMachineCode(e) ++ List(CosIns)
        }
    }
}
