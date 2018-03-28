#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package parsec
  --package tagsoup
-}

import Text.Parsec hiding (satisfy)
import Text.Parsec.Prim hiding (satisfy)
import Text.Parsec.Pos
import Text.HTML.TagSoup


htmlString = unlines [
    "<ul id=\"f-list\">"
  , "  <li id=\"lastmod\"> This page was last modified on 9 September 2013, at 22:38.</li>"
  , "  <li id=\"copyright\">Recent content is available under <a href=\"/HaskellWiki:Copyrights\" title=\"HaskellWiki:Copyrights\">a simple permissive license</a>.</li>"
  , "  <li id=\"privacy\"><a href=\"/HaskellWiki:Privacy_policy\" title=\"HaskellWiki:Privacy policy\">Privacy policy</a></li>"
  , "  <li id=\"about\"><a href=\"/HaskellWiki:About\" title=\"HaskellWiki:About\">About HaskellWiki</a></li>"
  , "  <li id=\"disclaimer\"><a href=\"/HaskellWiki:General_disclaimer\" title=\"HaskellWiki:General disclaimer\">Disclaimers</a></li>"
  , "</ul>"
  ]

htmlTags = parseTags htmlString

type HtmlTag = Tag String
type HtmlParser = Parsec [HtmlTag] ()

htmlUpdatePos s _ _ = incSourceColumn s 1

htmlMatchPredicate p t = if p t then Just t else Nothing

satisfy :: (HtmlTag -> Bool) -> HtmlParser HtmlTag
satisfy p = tokenPrim
            show
            htmlUpdatePos
            (htmlMatchPredicate p)

htmlTag :: HtmlTag -> HtmlParser HtmlTag
htmlTag t = satisfy (~== t)

htmlTagOpenName :: String -> HtmlParser HtmlTag
htmlTagOpenName s = htmlTag (TagOpen s [])

htmlTagCloseName :: String -> HtmlParser HtmlTag
htmlTagCloseName s = htmlTag (TagClose s)

htmlTagOpen :: HtmlParser HtmlTag
htmlTagOpen = satisfy (isTagOpen)

htmlTagClose :: HtmlParser HtmlTag
htmlTagClose = satisfy (isTagClose)

htmlTagText :: HtmlParser HtmlTag
htmlTagText = satisfy (isTagText)

htmlAnyTag :: HtmlParser HtmlTag
htmlAnyTag = anyToken

getTagName :: Tag String -> String
getTagName (TagOpen s as) = s
getTagName (TagClose s) = s
getTagName _ = "N/A"

htmlInnerTags :: HtmlParser [HtmlTag]
htmlInnerTags = do
  openTag <- htmlTagOpen
  let tagName = getTagName openTag
  inner <- many (satisfy (~/= (TagClose tagName)))
  htmlTagCloseName tagName
  return inner

htmlParseLi :: HtmlParser [HtmlTag]
htmlParseLi = between
                (htmlTagOpenName "li")
                (htmlTagCloseName "li" >> (skipMany htmlTagText))
                (many (satisfy (~/= "</li>")))

theParser = do
  htmlTagOpenName "ul"
  skipMany htmlTagText
  innerLis <- many htmlParseLi
  skipMany htmlTagText
  htmlTagCloseName "ul"
  return innerLis

main = do
  case (parse theParser "" htmlTags) of
        Left err  -> print err
        Right ts  -> print ts
