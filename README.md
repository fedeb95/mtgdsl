# mtgdsl
A DSL for Magic The Gathering simulations

This project is still under development. For now simply compile with `scalac *.scala` in project folder. 
Usage:
In a file `<filename>.deck` list the cards of the deck to use in the simulation as in the example provided.
In a file `<filename2>.mtg` specify your plays as in the file provided. You can have multiple turns, remember that the `.` matters and is needed unfortunatly.
Call the program with `scala mtg.dsl.MtgEval <LANGUAGE> <deck_filename> <simulation>.mtg`. You can specify multiple simulation files at once, remember that the deck filename is without extension as an argument to the program.
Languages supported: English (ENG as an argument), Italian (ITA as an argument).
Soon I'll provide a more clair explanation of the grammar, and hopefully add actions and a proper build system.
Issues and pull requests are welcomed.
