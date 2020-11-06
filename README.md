To starte the game just open the `index.html` file.

To recompile the game you must generate a new `main.js` file with:

```bash
elm make src/Main.elm --output=main.js
```

This is because there is custom javascript in the `index.html` file 
that is passing in an initial seed value for the random number generator.

Happy Codenames!