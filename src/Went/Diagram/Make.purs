module Went.Diagram.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import GoJS.Diagram.Constructors (newDiagram)
import GoJS.Diagram.Palette.Constructors (newPalette)
import GoJS.Diagram.Methods (addDiagramListener_, addGroupTemplate_, addLinkTemplate_, addNodeTemplate_, add_, attach_)
import GoJS.Diagram.Types (class IsDiagram, DiagramEvent_, Diagram_, Palette_)
import GoJS.GraphObject.Shape.Static (defineFigureGenerator_)
import GoJS.GraphObject.Types (class IsPanel, class IsPart, Link_, Shape_)
import Heterogeneous.Mapping (class HMap, hmap)
import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Went.Diagram (DiagramAttachOptions)
import Went.Diagram.Event (DiagramEvent)
import Went.FFI.Class (CreateFFIRecord(..), ffi)
import Went.Geometry.Geometry (Geometry)
import Went.Layout.Make (class LayoutM, layoutImp)
import Went.Model.Make (class ModelM, modelImp)
import Went.Settable (class Settable, setImp)
import Went.Template.Makers (Made(..), MadeGroup, MadeLink, MadeNode)
import Went.Tool.Make (class ToolM, insertToolAtImp, modifyToolImp, replaceToolImp)

newtype MakeDiagram (nodeData :: Row Type) (linkData :: Row Type) (d :: Type) a = MakeDiagram (ReaderT d Effect a)

derive newtype instance Functor (MakeDiagram nodeData linkData d)
derive newtype instance Apply (MakeDiagram nodeData linkData d)
derive newtype instance Applicative (MakeDiagram nodeData linkData d)
derive newtype instance Monad (MakeDiagram nodeData linkData d)
derive newtype instance Bind (MakeDiagram nodeData linkData d)

-- TODO: This instance is needed, without any constraints, in order to accomodate
-- deeply nested setting of values that *already exist* inside a diagram when it is
-- created. The most salient examples right now are all the things inside toolManager:
-- they are *pre-created* links, tools, nodes, graphobjects etc - our monadic interface
-- is prepared to handle CREATION of such values, not MODIFICATIONS. Solving this problem
-- then amounts to implementing a type-safe version of the library's capacity
-- to simply set values like diagram.toolManager.relinkingTool.link.opacity = "transparent".
-- The solution might be to have a separate monad to Make, called Modify, which could
-- be similar to the MakeTool monad and rely on already existing fields via ReaderT.
instance Settable (MakeDiagram nodeData linkData d) settable where
  set fields = MakeDiagram $ setImp fields

instance LayoutM (MakeDiagram nodeData linkData d) where
  layout layoutConstructor = MakeDiagram <<< layoutImp layoutConstructor

instance ModelM (MakeDiagram nodeData linkData d) nodeData linkData where
  model' modelConstructor = MakeDiagram <<< modelImp modelConstructor

instance ToolM (MakeDiagram nodeData linkData d) nodeData where
  replaceTool toolConstructor = MakeDiagram <<< replaceToolImp toolConstructor
  insertToolAt toolConstructor mouseActionType ix = MakeDiagram <<< insertToolAtImp toolConstructor mouseActionType ix
  modifyTool = MakeDiagram <<< modifyToolImp 

-- This is a function that passes in "init options" to the diagram, post its creation. From the docs:
--   DiagramInitOptions: Partial<Diagram | { [ P in DiagramEventName]?: (e: DiagramEvent) => void } | DiagramInitStrings>
--   DiagramInitOptions are used in the Diagram constructor to concisely initialize a Diagram by settings its properties,
--   and also set properties on its Tools, CommandHandler, AnimationManager, and set DiagramEvents.
-- DiagramAttachOptions here is analogous to the typescript declaration of DiagramInitOptions, except without DiagramEvent listeners.
attach
  :: forall rIn rOut rest nodeData linkData d
   . HMap CreateFFIRecord (Record rIn) (Record rOut)
  => IsDiagram d
  => Union rIn rest DiagramAttachOptions
  => Record rIn
  -> MakeDiagram nodeData linkData d Unit
attach options = MakeDiagram $ do
  handle <- ask
  _ <- liftEffect $ attach_ (hmap CreateFFIRecord options) handle
  pure unit

addNodeTemplate
  :: forall b nodeData linkData d
   . IsPanel b
  => IsDiagram d
  => String
  -> MadeNode nodeData b
  -> MakeDiagram nodeData linkData d Unit
addNodeTemplate templateName (Made howToMadeNode) = MakeDiagram $ do
  handle <- ask
  nodeTemplate <- liftEffect howToMadeNode
  liftEffect $ addNodeTemplate_ templateName nodeTemplate handle

class AllNodeTemps (rl :: RowList Type)

instance AllNodeTemps Nil
instance
  ( AllNodeTemps rest
  ) =>
  AllNodeTemps (Cons s (MadeNode nodeData b) rest)

-- TODO: Implement this as an alternative interface to addNodeTemplate
nodeTemplateMap
  :: forall (r :: Row Type) (rl :: RowList Type) nodeData linkData d
   . RowToList r rl
  => AllNodeTemps rl
  => Record r
  -> MakeDiagram nodeData linkData d Unit
nodeTemplateMap = unsafeThrow ""

addLinkTemplate
  :: forall nodeData linkData d
   . IsDiagram d
  => String
  -> MadeLink linkData Link_
  -> MakeDiagram nodeData linkData d Unit
addLinkTemplate templateName (Made howToMakeLinkemplate) = MakeDiagram $ do
  handle <- ask
  linkTemplate <- liftEffect howToMakeLinkemplate
  liftEffect $ addLinkTemplate_ templateName linkTemplate handle

addGroupTemplate
  :: forall b nodeData linkData d
   . IsPanel b
  => IsDiagram d
  => String
  -> MadeGroup nodeData b
  -> MakeDiagram nodeData linkData d Unit
addGroupTemplate templateName (Made howToMadeGroup) = MakeDiagram $ do
  handle <- ask
  groupTemplate <- liftEffect howToMadeGroup
  liftEffect $ addGroupTemplate_ templateName groupTemplate handle

addNode
  :: forall b nodeData linkData d
   . IsPart b
  => IsDiagram d
  => MadeNode nodeData b
  -> MakeDiagram nodeData linkData d Unit
addNode (Made howToMadeNode) = MakeDiagram $ do
  handle <- ask
  nodeTemplate <- liftEffect howToMadeNode
  liftEffect $ add_ nodeTemplate handle

addDiagramListener
  :: forall nodeData linkData diagramType subjectType
   . IsDiagram diagramType
  => DiagramEvent
  -> (DiagramEvent_ subjectType -> Effect Unit)
  -> MakeDiagram nodeData linkData diagramType Unit
addDiagramListener event handler = MakeDiagram $ do
  handle <- ask
  _ <- liftEffect $ addDiagramListener_ (show event) (ffi handler) handle
  pure unit

defineFigureGenerator
  :: forall nodeData linkData diagramType
   . IsDiagram diagramType
  => String
  -> (Shape_ -> Number -> Number -> Geometry)
  -> MakeDiagram nodeData linkData diagramType Unit
defineFigureGenerator name geometryMaker = MakeDiagram $ do
  liftEffect $ defineFigureGenerator_ name (ffi geometryMaker)

make :: forall nodeData linkData. String -> MakeDiagram nodeData linkData Diagram_ Unit -> Effect Unit
make div (MakeDiagram m) = do
  diag <- newDiagram div
  runReaderT m diag

makePalette :: forall nodeData linkData. String -> MakeDiagram nodeData linkData Palette_ Unit -> Effect Unit
makePalette div (MakeDiagram m) = do
  diag <- newPalette div
  runReaderT m diag
