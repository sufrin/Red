---------------------------------------------------------
--
-- Module        : Text.Format.Para
-- Copyright     : Kevin Quick
-- License       : BSD3
--
-- Maintainer    : Kevin Quick <quick@sparq.org>
-- Stability     : Unstable
-- Portability   : portable
--
-- Formatting text into paragraphs of specified width.
---------------------------------------------------------

-- | A paragraph formatting utility.  Provided with input text that is
--   arbitrarily split amongst several strings, this utility will
--   reformat the text into paragraphs which do not exceed the
--   specified width.  Paragraphs are delimited by blank lines in the
--   input.
--
--   This function is roughly equivalent to the Unix `fmt` utility.
--
--   Features:
--
--   * An indentation/prefix text may be specified.  This prefix is
--     used on the first paragraph line and determines the standard
--     indentation for all subsequent lines.  If no indentation is
--     specified, the blank indentation of the first line of the first
--     paragraph becomes the default indentation for all paragraphs.
--
--   * Subsequent paragraphs may increase their indentation over the
--     default as determined by the indentation level of their first
--     line.  Indentation values less than that of the primary
--     paragraph are ignored.
--
--   * Paragraph text is reformatted to fit the paragraph layout.
--
--   * Extra whitespace is removed.
--
--   * \"French spacing\" is used: if the current word is capitalized
--     and the previous word ended in a punctuation character, then
--     two spaces are used between the words instead of a single space
--     which is the default elsewhere.
--
--   * Avoids orphan words.  The last line of a paragraph will usually
--     be formatted to contain at least 2 words, pulling from the line
--     above it.
--
--   * Recognizes lists of items, where each item starts with * or -
--     or alphanumeric characters followed by a ) or . character.
--     Uses list-oriented per-item indentation independent of
--     paragraph indentation.

module Para ( formatParas ) where

import Data.Maybe
import Data.List
import Data.Char


data LineType = Regular
              | ListMarker Int Int  -- first indent, subsequent indent
              | Verbatim   Int      -- no line breaks, preserve as is, single spaced, indent level
              | ParaStart  Int      -- 1st line of paragraph, indent level
                deriving Eq

getLineType :: String -> LineType
getLineType s = let w = words s
                    w1 = head w
                    fi = length $ takeWhile isSpace s
                    isLmark x = let endfirst = if null x' then 'X' else head x'
                                    x'       = dropWhile isAlphaNum x
                                    pm       = 1 == length x  && head x `elem` "*-"
                                    nm       = 1 == length x' && endfirst `elem` ")."
                                in pm || nm
                    isVerb x = 1 == length x && head x `elem` "$>"
                in if null w || not (isLmark w1 || isVerb w1) then Regular
                   else if isLmark w1 then ListMarker fi (fi + (length w1) + 1)
                        else Verbatim fi


-- | The 'formatParas' function accepts an arbitrarily-divided list of
--   Strings along with a width and optional indentation/prefix and
--   returns an array of strings representing paragraphs formatted to
--   fit the specified width and indentation.
formatParas :: Int -- ^ Width
           -> Maybe String -- ^ Prefix (defines indent), Nothing means
                           --   indent is taken from first input line
           -> [String] -- ^ Text to format in arbitrarily-divided
                       -- strings.  Blank lines separate paragraphs.
                       -- Paragraphs are indented the same as the
                       -- first line if second argument is Nothing.
           -> [String] -- ^ Formatted text
formatParas w i s =
    let minTextWidth = 10
        -- i' is the actual indent string, either from input or taken from first line
        i' = if isJust i then fromJust i else
                 if null s then "" else takeWhile isSpace $ head s
        -- w' is the actual paragraph text width to format paragraphs to
        w' = max minTextWidth $ w - length i'

        -- Each element of input s could have newlines in it already.  Split those out,
        -- then look for paragraph separators, then mash each para to do len-based line
        -- splitting.  Use the first non-blank line of the first paragraph as the basis
        -- for all other paragraph indentations.

        all_ls = concatMap lines s
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        parablks = filter (\pb -> length (concatMap words pb) > 0) $
                   groupBy (\_ -> not . is_parasep) all_ls
        is_parasep x = Regular /= getLineType x || 0 == (length $ trim x)
        paras = map (\p-> (pindent p, intercalate " " p)) parablks

        pindent' [] = 0
        pindent' ("":pbs) = pindent' pbs
        pindent' (p:ps) = let ls = takeWhile isSpace p
                        in if ls == p then pindent' ps else length ls
        pindent p = let l = getLineType $ head p
                    in if null p then ParaStart 0
                       else if Regular /= l then l else ParaStart $ max (pindent' p) 0
                              
        -- Now reconstruct paragraphs from the above paras, which is an array of paragraph
        -- entries, each has size of indent in input form and array of words in the
        -- paragraph.  Use "french spacing" (two spaces after a period before a
        -- capitalized word) and don't orphan words.

        paralines' :: [[String]]
        paralines' = [ p ++ (maybe [] (flip (:) []) psep)
                       | (p,psep) <- zip (map fmtpara paras) (tail paraseps ++ [Nothing]) ]

        paraseps :: [ Maybe String ]
        paraseps = snd $ mapAccumL septype Regular paras
            where
             septype :: LineType -> (LineType, String) -> (LineType, Maybe String)
             septype (Verbatim vli) (n@(Verbatim vli'), _) =
                     (n, if vli == vli' then Nothing else Just "")
             septype _ (t,_) = (t,Just "")

        indent n = (replicate (n + length i') ' ' ++)

        fmtpara :: (LineType, String) -> [String]
        fmtpara ((ListMarker fli sli), t) =
                let ls = lbreak (w' - fli) (words t) True
                in (indent fli (head ls) : map (indent sli) (tail ls))
        fmtpara ((Verbatim _), t)  = [indent 0 t]
        fmtpara ((ParaStart vli), t) = map (indent vli) (lbreak (w'-vli) (words t) False)
        fmtpara (_,t) = [t]

        lbreak :: Int -> [String] -> Bool -> [String]
        lbreak _ [] _ = []
        lbreak width wrds lm =
           let lens = scanl (+) 0 wlen
               dots = map ((==) '.' . last) wrds
               frsp = 0 : [ if d && (isUpper $ head e) && not m
                            then 2 else 1
                            | (e,d,m) <- zip3
                                         (tail wrds) dots $
                                         lm : repeat False ]
                                         
               unwordsFrsp fs ws = concat [ sp ++ wrd
                                            | (wrd,n) <- zip ws fs,
                                            let sp = replicate n ' ']
                                            
               wlen      = [ length e + sp | (e,sp) <- zip wrds frsp ]
               thisline' = takeWhile ((>=) width . snd) (zip wrds lens)

               thisline | 1 == length thisline' = thisline'
                        | width >= last lens    = thisline'
                        | (length thisline' == length wrds) &&
                          (length thisline' > 2) = init $ init thisline'
                        | otherwise = init thisline'
                        
           in unwordsFrsp frsp (map fst thisline) :
              lbreak width (drop (length thisline) wrds) False
              
        paralines = let l1 = i' ++ (drop (length i') $ head $ head paralines')
                    in case paralines' of
                      [] -> []
                      []:r -> [i'] : r
                      f:r -> (l1 : tail f) : r

    in concat paralines

-- $example
-- The following show example uses and output of the Para formatter.
--
-- Here is a simple program that takes 2 or more arguments.
--
--   * A width
--
--   * One or more filenames
--
-- The program will read the specified files and then use Para to
-- format them with the specified width and display them on stdout.
--
--        > import Text.Format.Para
--        > import System.Environment
--        > import Data.List
--        >
--        > main = do
--        >      args <- getArgs
--        >      let width = head args
--        >      bodies <- mapM readFile $ tail args
--        >      putStrLn $ unlines $ formatParas (read width) (Just "Example: ") $
--        >               intersperse "\n" bodies
--
-- This program is useable in a similar manner to the Unix `fmt'
-- application.  It also provides a convenient way to test and
-- experiment with the output of the Para module.
--
-- The following represents an example input file that demonstrates
-- most of the capabilities of the Para formatter:
--
-- > This is a test.
-- > This is line 2. Note: double spacing (a.k.a. french spacing) between sentences, but
-- >     elsewhere    only     single   spacing
-- > is used; i.e. whitespace compression is performed.
-- > 
-- > This is the second paragraph. Note that all indentation is based on
-- > the initial indentation string specified, although that string only
-- > introduces the first paragraph on the sequence.
-- > 
-- >    * Here is a list
-- >    * This is another list item.  It is fairly long, so when it wraps the subline should be indented.
-- > 
-- >    This is the third paragraph.Text.Format.Para
-- >    And it is indented.
-- >    It is followed by a command-line example:
-- >      $ ghc --make -o ptest ptest.hs
-- >      $ ./ptest
-- > 
-- >       The fourth paragraph
-- > is indented even more.
-- > 
-- > Birdtracks are verbatim, even if the line is long.
-- >    > main = do
-- >    >    args <- getArgs
-- >    >    putStrLn $ "Hello!  Hi. Greetings.   I think you said " ++ intercalate ", " args
-- > 
-- > The list can also be
-- > numbered or use other
-- > indicators:
-- > 
-- >     1) Here is a list
-- >     2) Item #2
-- >     10) Item 10
-- >     20a) This is a longer item with a mixed representation of the item count.
-- >     4. Can use standard decimals as well for numbering elements.
-- >     5. And it doesn't really matter if all elements of the list are the same.  Just as long as it's recognized as a list element.
-- >        a) But it does have to be at the same indentation?
-- >        b) Right.  Multi-level lists are supported.  Each list item is handled as a paragraph.
-- >     6. Level is based on initial character indentation. 
-- > 
-- > And that's it!
--
--
-- If this example is saved to an input file and then processed with
-- the test application above and a width of 80, the output might look
-- like the following:
--
-- > Example: This is a test.  This is line 2.  Note: double spacing (a.k.a. french
-- >          spacing) between sentences, but elsewhere only single spacing is used;
-- >          i.e. whitespace compression is performed.
-- > 
-- >          This is the second paragraph.  Note that all indentation is based on
-- >          the initial indentation string specified, although that string only
-- >          introduces the first paragraph on the sequence.
-- > 
-- >             * Here is a list
-- > 
-- >             * This is another list item.  It is fairly long, so when it wraps
-- >               the subline should be indented.
-- > 
-- >             This is the third paragraph.  And it is indented.  It is followed by
-- >             a command-line example:
-- > 
-- >               $ ghc --make -o ptest ptest.hs
-- >               $ ./ptest
-- > 
-- >                The fourth paragraph is indented even more.
-- > 
-- >          Birdtracks are verbatim, even if the line is long.
-- > 
-- >             > main = do
-- >             >    args <- getArgs
-- >             >    putStrLn $ "Hello!  Hi. Greetings.   I think you said " ++ intercalate ", " args
-- > 
-- >          The list can also be numbered or use other indicators:
-- > 
-- >              1) Here is a list
-- > 
-- >              2) Item #2
-- > 
-- >              10) Item 10
-- > 
-- >              20a) This is a longer item with a mixed representation of the
-- >                   item count.
-- > 
-- >              4. Can use standard decimals as well for numbering elements.
-- > 
-- >              5. And it doesn't really matter if all elements of the list are the
-- >                 same.  Just as long as it's recognized as a list element.
-- > 
-- >                 a) But it does have to be at the same indentation?
-- > 
-- >                 b) Right.  Multi-level lists are supported.  Each list item is
-- >                    handled as a paragraph.
-- > 
-- >              6. Level is based on initial character indentation.
-- > 
-- >          And that's it!
--
--
--
-- If this same input file was run with a a width of 50 instead then
-- the output would look like the following:
--
-- > Example: This is a test.  This is line 2.  Note:
-- >          double spacing (a.k.a. french spacing)
-- >          between sentences, but elsewhere only
-- >          single spacing is used; i.e. whitespace
-- >          compression is performed.
-- > 
-- >          This is the second paragraph.  Note that
-- >          all indentation is based on the initial
-- >          indentation string specified, although
-- >          that string only introduces the first
-- >          paragraph on the sequence.
-- > 
-- >             * Here is a list
-- > 
-- >             * This is another list item.  It is
-- >               fairly long, so when it wraps the
-- >               subline should be indented.
-- > 
-- >             This is the third paragraph.  And it
-- >             is indented.  It is followed by a
-- >             command-line example:
-- > 
-- >               $ ghc --make -o ptest ptest.hs
-- >               $ ./ptest
-- > 
-- >                The fourth paragraph is indented
-- >                even more.
-- > 
-- >          Birdtracks are verbatim, even if the line
-- >          is long.
-- > 
-- >             > main = do
-- >             >    args <- getArgs
-- >             >    putStrLn $ "Hello!  Hi. Greetings.   I think you said " ++ intercalate ", " args
-- > 
-- >          The list can also be numbered or use
-- >          other indicators:
-- > 
-- >              1) Here is a list
-- >              
-- >              2) Item #2
-- >              
-- >              10) Item 10
-- >              
-- >              20a) This is a longer item with a
-- >                   mixed representation of the
-- >                   item count.
-- >              
-- >              4. Can use standard decimals as well
-- >                 for numbering elements.
-- >              
-- >              5. And it doesn't really matter if
-- >                 all elements of the list are the
-- >                 same.  Just as long as it's
-- >                 recognized as a list element.
-- >                 
-- >                 a) But it does have to be at the
-- >                    same indentation?
-- >                 
-- >                 b) Right.  Multi-level lists are
-- >                    supported.  Each list item is
-- >                    handled as a paragraph.
-- >              
-- >              6. Level is based on initial
-- >                 character indentation.
-- >          
-- >          And that's it!

