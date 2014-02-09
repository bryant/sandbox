from prettytable import PrettyTable

INPUT = "input"
OUTPUT = "output"

pinouts = PrettyTable(("Pin Name", "Direction"))
pinouts.add_row(("Jog North", INPUT))
pinouts.add_row(("Jog South", INPUT))
pinouts.add_row(("Jog East", INPUT))
pinouts.add_row(("Jog West", INPUT))
pinouts.add_row(("Feedrate #0", INPUT))
pinouts.add_row(("Feedrate #1", INPUT))
pinouts.add_row(("Feedrate #2", INPUT))
pinouts.add_row(("Feedrate #3", INPUT))
pinouts.add_row(("Program Selection 0", INPUT))
pinouts.add_row(("Program Selection 1", INPUT))
pinouts.add_row(("LED, red", OUTPUT))
pinouts.add_row(("LED, green", OUTPUT))
print pinouts
