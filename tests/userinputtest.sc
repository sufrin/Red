import Red.UserInput._

// Strings that denote UserInput values

apply(".NumPad-5|ACSM")
apply(".NumPad-0|ACSM")
apply("Numpad.Key-5|ACSM")
apply(".Find|ACSM")
apply(".Dead Tilde|ACSM")
apply(".Back Quote|ACSM")
apply(".\\x0065|ACSM")


apply("'A'|CS")
apply(".A|CS")
apply("'.'|CS")

apply("Left.'.'|CS")
apply("'.'|M")
apply("'.'|C")

apply("';'|CS")
apply("';'|S")

apply("':'|S")
apply(".Colon|CS")
apply("'9'|CS")
apply("'\\"+"u0100'")
apply("'\\"+"u0100'|C")
apply(".All Candidates|C")
apply("'F'")


apply(".Quote|C")
apply(".'\"'|C")
apply(".Page Down|C")
apply(".\\xde|C")
apply(".Quote|C")
apply(".'Q'|C")

apply(".Right|C")
apply(".RightParenthesis|C")
apply("Standard.\\x0030|C")
apply(".Enter|CS")

apply(".D|ACS")
apply(".F1|ACSM")
apply(".F2|ACSM")
apply(".F21|ACSM")
apply(".F22|ACSM")

apply("Left.Meta")
apply("Right.Meta")
apply(".Meta")

apply("Numpad.'.'")
apply("'.'")
apply("'\u0015'")

apply("Unknown.F22|ACSM")
apply("Numpad.F22|ACSM")
apply("Numpad.|ACSM")
apply("Standard.'`'|ACSM")


for { k <- scala.swing.event.Key.values } println(f"$k%s \\x${k.id}%x")
