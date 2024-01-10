-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | The module offers a device tree specification reader for turning
device tree specifications into a 'DTS'. The parser is designed to
accept inputs if and only if they are accepted by the following call
of the open firmware device tree compiler
@[dtc](https://git.kernel.org/cgit/utils/dtc/dtc.git):@

@dtc -I dts -O dts -T \<input.dts\>@

Note that the aforementioned call of @dtc@ does not check whether

* required nodes like @\/memory@ or @/cpus@ exist,
* required properties of those nodes exist, or
* that the data of required properties is of the correct type.

Comments and node/value labels are elided, i.e., they are not part of
the returned 'DTS', as they do not capture any semantic meaning with
respect to the specification semantics.

This module implements [DeviceTree Specification Release
v0.3](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.3).
-}

module Bittide.Extra.DeviceTree.Parser
  ( DTS(..)
  , DtsValue(..)
  , readDTS
  ) where

import Prelude
import Control.Arrow (first, second)
import Control.Exception (assert)
import Control.Monad (void, when, foldM, forM, forM_)
import Control.Monad.State.Lazy (StateT, runStateT, get, put, lift)
import Data.Bits ((.&.), (.|.), complement, xor, shiftL, shiftR)
import Data.Bool (bool)
import Data.Either (lefts, rights, fromLeft)
import Data.Maybe (fromMaybe, isJust, catMaybes, fromJust, mapMaybe)
import Data.List (groupBy, find)
import Data.Tree (Tree(rootLabel, subForest), foldTree, unfoldTree)
import qualified Data.Tree as Tree (Tree(Node))
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (Lift)
import Text.Printf (printf)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
  ( (!), singleton, unions, mapKeys, lookup, insert, alter, alterF
  , empty, toList, foldlWithKey, elems, map)
import Data.Set (Set)
import qualified Data.Set as S
  (empty, lookupIndex, member, insert, union, fromList, notMember)

import Text.Parsec
  ( ParsecT, SourcePos, (<|>), (<?>)
  , runParserT, unexpected, getState, modifyState, getPosition
  , alphaNum, string, char, letter, hexDigit
  , optionMaybe, oneOf, noneOf, try, many, many1, sepBy1, option, eof
  , choice, lookAhead, manyTill, notFollowedBy
  )
import Text.Parsec.Expr
  ( Assoc(..), Operator(..), buildExpressionParser
  )
import Text.Parsec.Token
  ( GenLanguageDef(..), GenTokenParser(..), makeTokenParser
  )
import Text.Parsec.Error
  ( ParseError, Message(..), newErrorMessage
  )

-- | Container for the parsed device tree specification.
data DTS =
  DTS
    { version  :: Integer
    -- ^ the version @\<X\>@ as specified by the @\/dts-v\<X\>\/;@
    -- header
    , source   :: FilePath
    -- ^ the file path to the parsed device tree specification
    , reserved :: [(Integer, Integer)]
    -- ^ memory reservation requirements, where
    --
    -- * - the first element indicates the address and
    -- * - the second element the length
    , entries  :: [(String, [DtsValue])]
    -- ^ the paths of tree associated with their corresponding values
    }
  deriving (Show, Ord, Eq, Lift)

-- | Container for property values. Note that @\<u32\>@ and @\<u64\>@
-- integers are represented as singleton and two-element arrays,
-- respectively. Mixed property values are represented as 'DtsValue'
-- lists, where the empty list represents the @\<empty\>@ property
-- value.
data DtsValue =
    DtsString String
    -- ^ @"@...@"@-encoded values
  | DtsByteString [Word8]
    -- ^ @[@...@]@-encoded values (always non-empty)
  | DtsArray [Integer]
    -- ^ @\<@...@\>@-encoded values (always non-empty)
  deriving (Eq, Ord, Lift)

instance Show DtsValue where
  show = \case
    DtsString str     -> show str
    DtsByteString bs  -> show $ ByteString bs
    DtsArray xs       -> show $ Array (Left <$> xs)

-- | The used 'ParsecT' transformer
type Parser = ParsecT String (Set String, Set Integer) IO

-- | Extended version of 'DtsValue' (for internal use only).
data Value =
    Reference Reference
    -- ^ @&@... label reference
  | String String
    -- ^ @"@...@"@-encoded values
  | ByteString [Word8]
    -- ^ @[@...@]@-encoded values (always non-empty)
  | Array [Either Integer Reference]
    -- ^ @\<@...@\>@-encoded values (always non-empty)
  | DeleteProperty
    -- ^ for internal use only
  deriving (Eq, Ord)

instance Show Value where
  show = \case
    String str     -> show str
    Reference ref  -> show ref
    ByteString bs  -> "[" <> unwords (map (printf "%.2x") bs) <> "]"
    Array xs       -> "<" <> unwords (map prElem xs) <> ">"
    DeleteProperty -> "-> delete <-"
   where
    prElem = \case
      Left x  -> printf "0x%.8x" x
      Right x -> show x

-- | Container for a node label reference (for internal use only).
data Reference =
    -- | @&@\<label@\>@ encoded references (+ source position)
    Label
      { ref :: String
      , start :: SourcePos
      , end :: SourcePos
      }
    -- | @&{@\<path\>@}@ encoded reference (+ source position)
  | FullPath
      { ref :: String
      , start :: SourcePos
      , end :: SourcePos
      }
  deriving (Eq, Ord)

instance Show Reference where
  show = \case
    Label str _ _    -> '&' : str
    FullPath str _ _ -> "&{" <> str <> "}"

-- | Extended version of @DtsNode@ (for internal use only).
data Node =
    Node
      { nodeName :: String
        -- ^ name of the node
      , unitAddress :: Maybe String
        -- ^ optional unit address of the node
      , nodeLabels :: [(String, SourcePos, SourcePos)]
        -- ^ optional node labels of a node
      , properties :: Map String [Value]
        -- ^ properties of the node, where
        --
        -- * - the empty list represents the @\<empty\>@ value,
        -- * - a singleton list represents a single property value,
        -- * - lists with more than one element represent either a
        --     @\<stringlist\>@ (if all elements are @"@...@"@-encoded
        --     values) or a mixed-value list
      , pHandle :: Maybe Integer
        -- ^ optional phandle
      }
  | ReferenceNode
      { reference :: Reference
      , nodeLabels :: [(String, SourcePos, SourcePos)]
      , properties :: Map String [Value]
      , pHandle :: Maybe Integer
      }
  | DeleteNode
      { nodeName :: String
      , unitAddress :: Maybe String
      }
  | DeleteNodeAtReference
      { reference :: Reference
      }
  deriving (Show, Eq, Ord)

-- | Flattens the tree into a (@\<path\>\/\<property-name\>,
-- [DtsValue])@ list.
pathMap ::
  Tree Node ->
  [(String, [Value])]
pathMap = (M.toList .) . foldTree $ \Node{..} subs ->
  let
    prefix = (nodeName <>) $ case unitAddress of
      Nothing -> case nodeName of
        "/" -> ""
        _   -> "/"
      Just x  -> "@" <> x <> "/"

    props = case pHandle of
      Nothing -> properties
      Just ph -> M.insert "phandle" [Array [Left ph]] properties
  in
    M.unions (M.mapKeys (prefix <>) <$> (props : subs))

-- | Parses a device tree specification file and turns it into a
-- 'DTS'. Throws an error if the requested specification file is
-- invalid or does not exist.
readDTS ::
  FilePath ->
  IO DTS
readDTS source = do
  dtsContent <- readFile source
  dtsData <- runParserT dtsParser (S.empty, S.empty) source dtsContent
  let dts = do
            (version, reserved, trees) <- dtsData
            (tree, labels) <- mergeTrees trees
            entries <- mapMaybe (\(x, vs) -> (x,) <$>strip vs) . pathMap
                         <$> resolveValueReferences labels tree
            return DTS{..}
  case dts of
    Right x -> return x
    Left e  -> error $ unlines $ ("" :) $ fmap (replicate 8 ' ' <>)
                      $ lines $ show e
 where
  strip vs =
    if flip any vs $ \case
      DeleteProperty -> True
      _              -> False
    then Nothing
    else Just $ flip map vs $ \case
      String str    -> DtsString str
      ByteString bs -> DtsByteString bs
      Array xs      -> DtsArray $ map (fromLeft err) xs
      _             -> err
  err = error "internal: remaining reference"

dtsParser :: Parser (Integer, [(Integer, Integer)], [Tree Node])
dtsParser = whiteSpace >> (,,)
  <$> (head <$> many1 versionParser)
  <*> many memReserveParser
  <*> do
    firstEntry <- nodeParser True True
    remaining <- many (nodeParser False True)
    eof
    return (firstEntry : remaining)
 where
  commentStart    = "/*"
  commentEnd      = "*/"
  commentLine     = "//"
  nestedComments  = True
  caseSensitive   = True
  identStart      = letter
  identLetter     = alphaNum <|> oneOf ",._+-"
  opStart         = oneOf "=;:,?*/%+-~&|^<>!"
  opLetter        = oneOf "=&|<>"
  reservedNames   =
    ["/bits/", "/include/", "/memreserve/", "/delete-node/", "/delete-property/"]
  reservedOpNames =
    [ "=", ";", ":", ",", "?", "*", "/", "%", "+", "-", "~", "&", "|", "^"
    , ">>", "<<", "!", "&&", "||", ">", "<", "<=", ">=", "==", "!="
    ]

  TokenParser{..} = makeTokenParser LanguageDef{..}

  versionParser = try $ do
    v <- string "/dts-v" >> decimal
    lexeme (char '/') >> semi >> return v

  memReserveParser = do
    reserved "/memreserve/"
    addr <- natural
    len <- natural
    semi >> return (addr, len)

  nodeParser initial root = do
    -- node labels
    nodeLabels <- many $ try labelParser
    -- check for delete directive
    delete <-
      if initial
      then return False
      else isJust <$> optionMaybe (try $ reserved "/delete-node/")
    -- get the node reference
    ref <- if
      | not root    -> Left <$> lexeme nodeNameParser
      | delete      -> Right <$> referenceParser root 32
      | initial     -> Left . (, Nothing) <$> symbol "/"
      | otherwise   -> Left . (, Nothing) <$> symbol "/"
                         <|> Right <$> referenceParser root 32
    -- check for forbidden root labels
    when (ref == Left ("/", Nothing)) $ case nodeLabels of
      []          -> return ()
      (x,_,_) : _ -> unexpected $ "label: " <> x
                               <> " (root nodes cannot be labeled)"
    -- get the node content
    node <-
      if delete
      then pure $ (`Tree.Node` []) $ case ref of
        Left (nodeName, unitAddress) -> DeleteNode{..}
        Right reference              -> DeleteNodeAtReference{..}
      else do
        case ref of
          Left ((@) -> name) -> do
            duplicate <- S.member name . fst <$> getState
            if not root && duplicate
            then unexpected $ "node name: " <> name
                           <> " (duplicate)"
            else modifyState $ first $ S.insert name
          _ -> return ()
        (ps, subForest) <- braces $ (,)
          <$> (modifyState (first $ const S.empty)
                 >> (M.unions <$> many propertyParser))
          <*> (modifyState (first $ const S.empty)
                 >> many (nodeParser False False))
        let (properties, pHandle) = case M.lookup "phandle" ps of
              Just [Array [Left x]] ->
                if x > 0
                then (M.alter (const Nothing) "phandle" ps, Just x)
                else (ps, Nothing)
              _ -> (ps, Nothing)
        return $ Tree.Node
          { rootLabel = case ref of
              Left (nodeName, unitAddress) -> Node{..}
              Right reference              -> ReferenceNode{..}
          , ..
          }
    -- done
    semi >> return node

  nodeNameParser = (,)
    <$> ((:) <$> identStart <*> many identLetter)
    <*> optionMaybe (char '@' >> many identLetter)

  propertyParser = do
    -- ensure that this not a node declaration
    void $ try $ lookAhead $ manyTill
      (notFollowedBy (reserved "/delete-node/") >> noneOf "{}")
      (oneOf "=;")
    -- skip property labels
    void $ many $ try labelParser
    -- check for delete directive
    delete <- isJust <$> optionMaybe (try $ reserved "/delete-property/")
    name <- propertyNameParser
    if delete
    then semi >> return (M.singleton name [DeleteProperty])
    else (S.lookupIndex name . fst <$> getState) >>= \case
      Just{}  -> unexpected $ "property name: " <> name <> " (duplicate)"
      Nothing -> do
        modifyState $ first $ S.insert name
        values <- choice
          [ reservedOp "=" >> filter nonEmpty
              <$> ( if name == "phandle"
                    then pure <$> pHandleParser
                    else sepBy1 valueParser (try (many labelParser >> comma))
                  )
          , pure []
          ]
        -- ending labels are ignored
        void $ many labelParser
        semi >> return (M.singleton name values)

  nonEmpty = \case
    ByteString bs -> not $ null bs
    Array xs      -> not $ null xs
    _             -> True

  propertyNameParser =
    lexeme $ many1 (alphaNum <|> oneOf ",._+?#-")

  pHandleParser = choice
    [ try (labelParser >> pHandleParser) -- skip value labels
    , let p = angles $ skipLabelsBeforeAndAfter $ choice
                [ parens (exprParser 32) <|> numParser 32
                , referenceParser True 32 >> pure 0
                ]
      in do
        n <- lookAhead p
        ps <- snd <$> getState
        if S.notMember n ps
        then do
          modifyState $ second $ S.insert n
          Array . return . Left <$> p
        else (<?> "an unused phandle value") $
          unexpected $ "phandle: " <> show n <> " (duplicate)"
    ]

  valueParser = choice
    [ try (labelParser >> valueParser) -- skip value labels
    , String <$> stringLiteral
    , Reference <$> referenceParser True 32
    , ByteString <$> brackets (many hexByteParser)
    , arrayParser
    ]

  arrayParser = do
    bits <- fromMaybe 32 <$> optionMaybe (try bitsParser)
    xs <- angles $ many $ skipLabelsBeforeAndAfter $ choice
      [ Left  <$> (parens (exprParser bits) <|> numParser bits)
      , Right <$> referenceParser True bits
      ]
    return $
      if bits == 8
      then ByteString $ map fromInteger $ lefts xs
      else Array xs

  bitsParser =
    reserved "/bits/" >> choice
      [ symbol (show x) >> return x
      | x <- [8, 16, 32, 64]
      ]

  skipLabelsBeforeAndAfter :: Parser a -> Parser a
  skipLabelsBeforeAndAfter p = do
    x <- many labelParser >> p
    many labelParser >> return x

  labelParser = lexeme $ do
    start <- getPosition
    x <- labelNameParser
    void $ char ':'
    end <- getPosition
    return (x, start, end)

  labelNameParser =
    (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

  referenceParser fullPath bits = do
    start <- getPosition
    void $ char '&'
    case bits of
      32 -> choice $ map lexeme $ catMaybes
        [ if fullPath
          then Just $ do
            void $ symbol "{"
            ref <- pathParser
            void $ char '}'
            end <- getPosition
            return FullPath{..}
          else Nothing
        , Just $ do
            ref <- labelNameParser
            end <- getPosition
            return Label{..}
        ]
      _ -> fail "References are only allowed in arrays with 32-bit elements."

  pathParser = lexeme $ do
    root <- char '/'
    xr <- option [] $ do
      name <- (@) <$> nodeNameParser
      yr <- many ((:) <$> char '/' <*> ((@) <$> nodeNameParser))
      return $ concat $ name : yr
    return $ root : xr

  (@) (x, y) = x <> maybe "" ('@':) y

  hexByteParser =
    let hex = (+ (-(fromEnum '0'))) . fromEnum <$> hexDigit
    in skipLabelsBeforeAndAfter
         $ lexeme (toEnum <$> ((+) <$> ((16 *) <$> hex) <*> hex))

  numParser bits = do
    x <- lookAhead natural
    if x < 2 ^ (bits :: Integer)
    then natural
    else (<?> "<" <> show bits <> "-bit array element>") $
      unexpected $ "<" <> show bits <> "-bit array element: " <> show x
                <> " (value out of range)"

  exprParser bits = do
    x <- buildExpressionParser table (parens (exprParser bits) <|> numParser bits)
    choice
      [ (\a b -> bool a b (x == 0))
          <$> (reservedOp "?" >> exprParser bits)
          <*> (reservedOp ":" >> exprParser bits)
      , pure x
      ]

  table =
     [ [ binary "*" (*) AssocLeft
       , binary "/" div AssocLeft
       , binary "%" mod AssocLeft
       ]
     , [ binary "+" (+) AssocLeft
       , binary "-" (-) AssocLeft
       ]
     , [ prefix "~" complement
       ]
     , [ binary "&" (.&.) AssocLeft
       , binary "|" (.|.) AssocLeft
       , binary "^" xor AssocLeft
       , binary "<<" (castA2 shiftL) AssocLeft
       , binary ">>" (castA2 shiftR) AssocLeft
       ]
     , [ prefix "!" (liftI not)
       ]
     , [ binary "&&" (liftI2 (&&)) AssocLeft
       , binary "||" (liftI2 (||)) AssocLeft
       ]
     , [ binary "<" (liftR2 (<)) AssocLeft
       , binary ">" (liftR2 (>)) AssocLeft
       , binary "<=" (liftR2 (<=)) AssocLeft
       , binary ">=" (liftR2 (>=)) AssocLeft
       , binary "==" (liftR2 (==)) AssocLeft
       , binary "!=" (liftR2 (/=)) AssocLeft
       ]
     ]

  castA2 f x = f x . fromInteger

  prefix name op = Prefix (reservedOp name >> return op)
  binary name op = Infix  (reservedOp name >> return op)

  liftI  op x   = if op (x > 0)         then 1 else 0
  liftI2 op x y = if op (x > 0) (y > 0) then 1 else 0
  liftR2 op x y = if op x       y       then 1 else 0

mergeTrees ::
  [Tree Node] ->
  Either ParseError (Tree Node, Map String String)
mergeTrees ts = assert (not $ null ts)
  $ runStateT (foldM rootMerge (head ts) $ tail ts) M.empty
 where
  rootMerge x y =
    fromMaybe (Tree.Node (Node "/" Nothing [] M.empty Nothing) [])
      <$> merge "" x y

  merge path t1@(Tree.Node dts1 sf1) t2@(Tree.Node dts2 sf2) = case dts1 of
    Node nodeName unitAddress lbls props ph -> do
      -- insert all labels of the sub-tree in case we are at a root node
      when (path == "") $ collectLabels "/" t1
      case dts2 of
        Node nodeName' unitAddress' lbls' props' ph' ->
          assert (nodeName == nodeName') $
          assert (unitAddress == unitAddress') $ do
            let
              nodeLabels = lbls <> lbls'
              properties = (\f -> M.foldlWithKey f props props') $ \m k vs ->
                if flip any vs $ \case DeleteProperty -> True
                                       _              -> False
                then M.alter (const Nothing) k m
                else M.alter (const $ Just vs) k m
              pHandle = maybe ph Just ph'
            subForest <- foldM (mergeSubForest path) sf1 sf2
            return $ Just $ Tree.Node { rootLabel = Node{..}, .. }
        DeleteNode{} -> return Nothing
        _ -> assert (null path) $ case reference dts2 of
          FullPath "/" _ _ ->
            merge path t1 $ flip Tree.Node sf2 $ case dts2 of
              ReferenceNode{..} -> Node
                { nodeName =  "/"
                , unitAddress = Nothing
                , ..
                }
              _ -> DeleteNode "/" Nothing
          _ -> refToNode t2 >>= merge path t1
    _ -> error "internal: first tree is special"

  collectLabels ::
    String ->
    Tree Node ->
    StateT (Map String String) (Either ParseError) ()
  collectLabels path t@Tree.Node{..} = do
    path' <-
      let extended = (if path == "/" then "/" else path <> "/")
                  <> (if nodeRef t == "/" then "" else nodeRef t)
      in case rootLabel of
        Node{}       -> return extended
        DeleteNode{} -> return extended
        _ -> case reference rootLabel of
          FullPath{..} -> return ref
          Label{..}    -> (M.lookup ref <$> get) >>= \case
            Just p  -> return p
            Nothing -> lift $ Left $ flip newErrorMessage start
                         $ Message $ "reference to an unknown label: " <> ref
    case rootLabel of
      DeleteNode{} -> return ()
      DeleteNodeAtReference{} -> return ()
      _ -> forM_ (nodeLabels rootLabel) $ \(lbl, start,_) -> do
        m <- get
        m' <- (\f -> M.alterF f lbl m) $ \case
          Nothing -> return $ Just path'
          Just p
            | p == path' -> return $ Just p
            | otherwise  -> lift $ Left $ flip newErrorMessage start $ Message
                $ "duplicate label '" <> lbl <> "' on " <> path' <> " and " <> p
        put m'
    mapM_ (collectLabels path') subForest

  mergeSubForest path xs t = do
    (ys, noMatch) <- foldM (findMatch path t) ([], True) xs
    return $ reverse $ if noMatch then t : ys else ys

  findMatch path t (a, noMatch) x =
    if nodeRef t == nodeRef x
    then merge (path <> "/" <> nodeRef t) x t >>= \case
      Just tt -> return (tt : a, False)
      Nothing -> return (a, False)
    else return (x : a, noMatch)

  refToNode ::
    Tree Node ->
    StateT (Map String String) (Either ParseError) (Tree Node)
  refToNode t = case reference $ rootLabel t of
    Label{..} -> (M.lookup ref <$> get) >>= \case
      Nothing -> lift $ Left $ flip newErrorMessage start $
        Message $ "reference to an unknown label: " <> ref
      Just p  -> return $ unfoldTo p t
    FullPath{..} -> return $ unfoldTo ref t

  unfoldTo path Tree.Node{..} = assert (not $ null path)
    $ Tree.Node (Node "/" Nothing [] M.empty Nothing) . return
    $ flip unfoldTree (Left $ groupBy (const (/= '/')) path) $ \case
        Right (Tree.Node t fs) -> (t, Right <$> fs)
        Left xs ->
          let
            (nodeName, mAddr) = break (== '@') $ tail $ head xs
            unitAddress = case mAddr of
              '@':xr -> Just xr
              _      -> Nothing
          in case tail xs of
              xr@(_:_:_) ->
                ( Node { nodeLabels = []
                       , properties = M.empty
                       , pHandle = Nothing
                       , ..
                       }
                , [Left xr]
                )
              _ ->
                ( case rootLabel of
                    ReferenceNode{nodeLabels, properties, pHandle} -> Node{..}
                    _ -> DeleteNode{..}
                , Right <$> subForest
                )

resolveValueReferences ::
  Map String String ->
  Tree Node ->
  Either ParseError (Tree Node)
resolveValueReferences labels tree = do
  lbls <- foldM collectUsedValueReferences S.empty tree
  let existingHandles = foldl (\s -> maybe s (`S.insert` s) . pHandle) S.empty tree
      (_, phMap, _) = assignPHandles lbls "" (existingHandles, M.empty, 1) tree
  return $ updateTree phMap "" tree
 where
  collectUsedValueReferences s = \case
    Node{..} ->
      fmap (S.union s . S.fromList . concat) $
        forM (concat $ M.elems properties) $ \case
          Reference lbl@Label{..} -> case M.lookup ref labels of
            Just _   -> return []
              -- ^ it suffices that the reference is valid at this point,
              -- but it won't resolve to a phandle, so don't collect it
            Nothing  -> unknownLabel lbl
          Array xs -> forM (rights xs) $ \case
            lbl@(Label{..}) -> case M.lookup ref labels of
              Just p   -> return p
              Nothing  -> unknownLabel lbl
            FullPath{..} -> return ref
          _ -> return []
    _ -> deadEnd

  unknownLabel = \case
    Label{..} ->
      Left $ flip newErrorMessage start $
        Message $ "reference to an unknown label: " <> ref
    _ -> undefined

  assignPHandles xs path (ps, m, r) t@(Tree.Node n ts) = case n of
    Node{..} ->
      let
        path' = updPath (nodeRef t) path
        a = if S.member path' xs
            then case pHandle of
              Just ph -> (ps, M.insert path' ph m, r)
              Nothing ->
                let x = fromJust $ find (not . (`S.member` ps)) [r,r+1..]
                in (S.insert x ps, M.insert path' x m, x+1)
            else (ps, m, r)
      in
        foldl (assignPHandles xs path') a ts
    _ -> deadEnd

  updateTree phMap path t@(Tree.Node n ts) =
    let
      path' = updPath (nodeRef t) path
      n' = n
        { pHandle = case pHandle n of
            Just ph -> Just ph
            Nothing -> M.lookup path' phMap
        , properties = M.map (map $ resolve phMap) $ properties n
        }
    in
      Tree.Node n' $ map (updateTree phMap path') ts

  updPath suffix = \case
    ""   -> "/"
    "/"  -> "/" <> suffix
    path -> path <> "/" <> suffix

  resolve phMap = \case
    Reference (Label{..})   -> String $ labels M.! ref
    Reference (FullPath{..})-> String ref
    Array xs -> Array $ map (resolveV phMap) xs
    x -> x

  resolveV phMap = \case
    Right (Label{..})    -> Left $ phMap M.! (labels M.! ref)
    Right (FullPath{..}) -> Left $ phMap M.! ref
    x -> x

  deadEnd = error "internal: remaining node reference"

nodeRef :: Tree Node -> String
nodeRef t = case rootLabel t of
  ReferenceNode{} -> ""
  DeleteNodeAtReference{} -> ""
  x -> nodeName x <> maybe "" ('@' :) (unitAddress x)
