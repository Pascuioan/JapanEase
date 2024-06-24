# JapanEase
This is a basic vocabulary practice app built using [Brick](https://hackage.haskell.org/package/brick) in Haskell. The words are added to the `words.txt` file. I built the app with Japanese in mind, so each line must have the format: ```<english>#<hiragana\katakana>#<romaji>```. The first two fields are used for displaying the question, and the third one is for validating the answer when prompted to translate from English to Japanese. You can use this app for other languages, but keep in mind that when displaying a question from the second field, the answer is checked against the third field.

**Note**: The questions are picked completely at random, so you might get the same one twice in a row or might not get all the words in the file.

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