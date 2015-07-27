-- 8.1.1 Use ghci to explore what happens if you pass a malformed pattern, such as [, to globToRegex. Write a small function that calls globToRegex, and pass it a malformed pattern. What happens?


-- 8.1.2 While filesystems on Unix are usually sensitive to case (e.g. “G” vs. “g”) in file names, Windows filesystems are not. Add a parameter to the globToRegex and matchesGlob functions that allows control over case sensitive matching.