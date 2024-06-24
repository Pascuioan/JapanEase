# JapanEase
This is a basic vocabulary practice app built using [Brick](https://hackage.haskell.org/package/brick) in Haskell. The words are added to the `words.txt` file. I built the app with japanese in mind so each line must have the format: ```<english>#<hiragana\katakana>#<romaji>```. The first 2 fileds are used for displaying the question and the 3rd one is for validating the answer when propted to translate from english to japanese. You could use this app for other languages but keep in mind that when displaying a question from the 2nd field, the answer is checked angainst the 3rd field.

**Note**: The questions are picked completely random so you might get the same one 2 times in a row or might not get all the words in the file.

#### Example:

`flower#はな#hana` 

`flower#fleur#fleur`
### Usage
You must have the Haskell toolchain installed along with [Cabal](https://www.haskell.org/cabal/).
```
https://github.com/Pascuioan/JapanEase.git
cabal build
cabal run
```