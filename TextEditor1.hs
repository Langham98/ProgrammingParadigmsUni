import Data.List
import System.IO
data TextEditor = TextEditor ([Char], [Char], [Char], [Char]) deriving (Show) 
-- Sets the type for the string.

-- Insert text string
insertText :: TextEditor
insertText = TextEditor ("I went for a run ", "", "on a rainy day", []) 
-- This sets the text in the string that shall be analysed throughout the application.

-- Count length of string
countLength :: TextEditor -> Int
countLength (TextEditor (x, y, z, b)) = length x + length z 
-- This counts the characters to both the right and the left of the cursor in the string.

-- Cursor to left
cursorLeft :: TextEditor -> TextEditor
cursorLeft (TextEditor (x, y, z, b)) = (TextEditor((init x), [], ([last x] ++ z), b)) 
-- This drops the end character in the string, effectively shifting the cursor once to the left.

-- Cursor to right
cursorRight :: TextEditor -> TextEditor
cursorRight (TextEditor (x, y, z, b)) = (TextEditor((x ++ [head z]), [], (tail z), b)) 
-- This drops the first character in the string, effectively shifting the cursor once to the right.

-- Cursor to beginning of string
cursorToStart :: TextEditor -> TextEditor
cursorToStart (TextEditor (x, y, z, b)) = (TextEditor ([], [], x, z)) 
-- This moves the cursor to the beginning of the string.

-- Cursor to end of string
cursorToEnd :: TextEditor -> TextEditor
cursorToEnd (TextEditor (x, y, z, b)) = (TextEditor (x, z, [], [])) 
-- This moves the cursor all the way to the end of the string.


-- Highlight all characters in string
highlightAll :: TextEditor -> TextEditor
highlightAll (TextEditor (x, y, z, b)) = (TextEditor ([], (x++z),[], b)) 
-- This selects all of the characters in the string.

-- Highlight all characters to the right of the cursor
highlightAllRight :: TextEditor -> TextEditor
highlightAllRight (TextEditor (x, y, z, b)) = (TextEditor (x, z, [], b)) 
-- All characters to the right of the cursor are highlighted.

-- Highlight all characters to the left of the cursor
highlightAllLeft :: TextEditor -> TextEditor
highlightAllLeft (TextEditor (x, y, z, b)) = (TextEditor ([], x, z, b)) 
-- All characters to the left of the cursor are highlighted.

-- Cut function
cutText :: TextEditor -> TextEditor
cutText (TextEditor (x, y, z, b)) = (TextEditor (x, [], z, y)) 
-- Text in the string is highlighted and cut to the clipboard.


-- Copy function
copyText :: TextEditor -> TextEditor
copyText (TextEditor (x, y, z, b)) = (TextEditor (x, y, z, y)) 
-- Text in the string is highlighted and copied to the clipboard.


-- Paste function
pasteText :: TextEditor -> TextEditor
pasteText (TextEditor (x, y, z, b)) = (TextEditor ((x++b), [], z, b)) 
-- Text from the string that is saved to clipboard is pasted.

-- Delete function
deleteText :: TextEditor -> TextEditor
deleteText (TextEditor (x, y, z, b)) = (TextEditor(x, [], (tail z), b)) 
-- This function deletes all characters in the string.

-- Read file
retrieveFile = do
    let file = "textString.txt" -- This tells the application which file to retrieve.
    contents <- readFile file -- This reads the contents of the file.
    return contents -- this displays the contents of the file.

-- Write file
writeTextFile :: TextEditor -> IO()
writeTextFile (TextEditor (x, y, z, b)) = 
        writeFile "writtenTextFile.txt" (x ++ y ++ z)
 -- This function writes the string to a text file under the given name.
















