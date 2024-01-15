module Went.Tool.Make where

import Prelude
import GoJS.Tool
  ( newActionTool
  , newClickCreatingTool
  , newClickSelectingTool
  , newContextMenuTool
  , newDragSelectingTool
  , newDraggingTool
  , newLinkReshapingTool
  , newLinkingTool
  , newPanningTool
  , newRelinkingTool
  , newResizingTool
  , newRotatingTool
  , newTextEditingTool
  )
import GoJS.Tool.Types
  ( class IsTool
  , ActionTool_
  , ClickCreatingTool_
  , ClickSelectingTool_
  , ContextMenuTool_
  , DragSelectingTool_
  , DraggingTool_
  , LinkReshapingTool_
  , LinkingTool_
  , PanningTool_
  , RelinkingTool_
  , ResizingTool_
  , RotatingTool_
  , TextEditingTool_
  )

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Class (class ClassName)
import GoJS.Collection (insertAt_)
import GoJS.Unsafe.Set (setUnsafe)
import GoJS.Unsafe (getUnsafe)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record (insert)
import Type.Prelude (class IsSymbol, Proxy(..))
import Went.Settable (class Settable, setImp)
import Went.Tool.ClickCreatingTool (ClickCreatingToolFields)
import Went.Tool.DragSelectingTool (DragSelectingToolFields)
import Went.Typelevel.LowercaseFirst (class LowercaseFirst)

newtype MakeTool (ffiType :: Type) (nodeData :: Row Type) (a :: Type) = MakeTool (ReaderT ffiType Effect a)

derive newtype instance Functor (MakeTool ffiType nodeData)
derive newtype instance Apply (MakeTool ffiType nodeData)
derive newtype instance Applicative (MakeTool ffiType nodeData)
derive newtype instance Bind (MakeTool ffiType nodeData)
derive newtype instance Monad (MakeTool ffiType nodeData)

-- | This typeclass exists so that we may define the tool functions
-- here instead of in the Diagram module. The reason that we give the
-- implementation as giving ReaderT instead of giving ReaderT an instance
-- is because we want to be able to import the instances, but not the
-- implementation. So someone running any old ReaderT will not be able to
-- call replaceTool etc; they *would* be able to call replaceToolImp, but
-- that function is only exposed to the library. -- actually there's no way
-- to make modules private in ps.
class ToolM m nodeData | m -> nodeData where
  replaceTool
    :: forall tool b n f parentField singleton
     . IsTool tool
    => ClassName tool n
    => LowercaseFirst n f
    => IsSymbol parentField
    => Append "toolManager." f parentField
    => Cons parentField tool () singleton
    => Effect tool
    -> MakeTool tool nodeData b
    -> m Unit
  modifyTool
    :: forall (toolType :: Type) (parent :: Type) (b :: Type) singleton className toolFieldName parentField
     . IsTool toolType
    => ClassName toolType className
    => LowercaseFirst className toolFieldName
    => IsSymbol parentField
    => Append "toolManager." toolFieldName parentField
    => Cons parentField toolType () singleton
    => MakeTool toolType nodeData b
    -> m Unit
  insertToolAt
    :: forall (parent :: Type) (b :: Type) (t :: Type)
     . IsTool t
    => Effect t
    -> MouseActionType
    -> Int
    -> MakeTool t nodeData b
    -> m Unit

replaceToolImp
  :: forall (toolType :: Type) parent nodeData (b :: Type) singleton className toolFieldName parentField
   . IsTool toolType
  => ClassName toolType className
  => LowercaseFirst className toolFieldName
  => IsSymbol parentField
  => Append "toolManager." toolFieldName parentField
  => Cons parentField toolType () singleton
  => Effect toolType
  -> MakeTool toolType nodeData b
  -> ReaderT parent Effect Unit
replaceToolImp constructor (MakeTool howToMakeTool) = do
  t <- liftEffect constructor
  madeTool <- liftEffect $ runReaderT (howToMakeTool *> ask) t
  parent <- ask
  liftEffect $ setUnsafe parent (insert (Proxy @parentField) madeTool {})

modifyToolImp
  :: forall (toolType :: Type) parent nodeData (b :: Type) singleton className toolFieldName parentField
   . IsTool toolType
  => ClassName toolType className
  => LowercaseFirst className toolFieldName
  => IsSymbol parentField
  => Append "toolManager." toolFieldName parentField
  => Cons parentField toolType () singleton
  => MakeTool toolType nodeData b
  -> ReaderT parent Effect Unit
modifyToolImp (MakeTool howToMakeTool) = do
  existingTool <- getUnsafe [ "toolManager" , reflectSymbol (Proxy @toolFieldName) ] <$> ask
  modifiedExistingTool <- liftEffect $ runReaderT (howToMakeTool *> ask) existingTool
  parent <- ask
  liftEffect $ setUnsafe parent (insert (Proxy @parentField) modifiedExistingTool {})
  pure unit

data MouseActionType = MouseDown | MouseMove | MouseUp

toolManagerField :: MouseActionType -> String
toolManagerField MouseDown = "mouseDownTools"
toolManagerField MouseMove = "mouseMoveTools"
toolManagerField MouseUp = "mouseUpTools"

-- Inserting new tools helpers
insertToolAtImp
  :: forall (nodeData :: Row Type) (m :: Type -> Type) (parent :: Type) (b :: Type) (t :: Type)
   . IsTool t
  => Effect t
  -> MouseActionType
  -> Int
  -> MakeTool t nodeData b
  -> ReaderT parent Effect Unit
insertToolAtImp constructor mouseWhichTools ix (MakeTool howToMakeTool) = do
  toolList <- getUnsafe [ "toolManager." <> toolManagerField mouseWhichTools ] <$> ask
  t <- liftEffect constructor
  madeTool <- liftEffect $ runReaderT (howToMakeTool *> ask) t
  liftEffect $ insertAt_ ix madeTool toolList

-- Replace existing tools helpers
replaceActionTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ActionTool_ nodeData b -> m Unit
replaceActionTool = replaceTool newActionTool

replaceLinkReshapingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool LinkReshapingTool_ nodeData b -> m Unit
replaceLinkReshapingTool = replaceTool newLinkReshapingTool

replaceRelinkingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool RelinkingTool_ nodeData b -> m Unit
replaceRelinkingTool = replaceTool newRelinkingTool

replaceResizingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ResizingTool_ nodeData b -> m Unit
replaceResizingTool = replaceTool newResizingTool

replaceRotatingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool RotatingTool_ nodeData b -> m Unit
replaceRotatingTool = replaceTool newRotatingTool

replaceDraggingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool DraggingTool_ nodeData b -> m Unit
replaceDraggingTool = replaceTool newDraggingTool

replaceDragSelectingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool DragSelectingTool_ nodeData b -> m Unit
replaceDragSelectingTool = replaceTool newDragSelectingTool

replaceLinkingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool LinkingTool_ nodeData b -> m Unit
replaceLinkingTool = replaceTool newLinkingTool

replacePanningTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool PanningTool_ nodeData b -> m Unit
replacePanningTool = replaceTool newPanningTool

replaceClickCreatingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ClickCreatingTool_ nodeData b -> m Unit
replaceClickCreatingTool = replaceTool newClickCreatingTool

replaceClickSelectingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ClickSelectingTool_ nodeData b -> m Unit
replaceClickSelectingTool = replaceTool newClickSelectingTool

replaceContextMenuTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ContextMenuTool_ nodeData b -> m Unit
replaceContextMenuTool = replaceTool newContextMenuTool

replaceTextEditingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool TextEditingTool_ nodeData b -> m Unit
replaceTextEditingTool = replaceTool newTextEditingTool

-- Modifying existing tools helpers
actionTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ActionTool_ nodeData b -> m Unit
actionTool = modifyTool

linkReshapingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool LinkReshapingTool_ nodeData b -> m Unit
linkReshapingTool = modifyTool

relinkingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool RelinkingTool_ nodeData b -> m Unit
relinkingTool = modifyTool

resizingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ResizingTool_ nodeData b -> m Unit
resizingTool = modifyTool

rotatingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool RotatingTool_ nodeData b -> m Unit
rotatingTool = modifyTool

draggingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool DraggingTool_ nodeData b -> m Unit
draggingTool = modifyTool

dragSelectingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool DragSelectingTool_ nodeData b -> m Unit
dragSelectingTool = modifyTool

linkingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool LinkingTool_ nodeData b -> m Unit
linkingTool = modifyTool

panningTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool PanningTool_ nodeData b -> m Unit
panningTool = modifyTool

clickCreatingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ClickCreatingTool_ nodeData b -> m Unit
clickCreatingTool = modifyTool

clickSelectingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ClickSelectingTool_ nodeData b -> m Unit
clickSelectingTool = modifyTool

contextMenuTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool ContextMenuTool_ nodeData b -> m Unit
contextMenuTool = modifyTool

textEditingTool :: forall (m :: Type -> Type) (nodeData :: Row Type) (b :: Type). ToolM m nodeData ⇒ MakeTool TextEditingTool_ nodeData b -> m Unit
textEditingTool = modifyTool

-- Insert new tools helpers
insertActionToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool ActionTool_ nodeData b -> m Unit
insertActionToolAt = insertToolAt @m newActionTool

insertLinkReshapingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool LinkReshapingTool_ nodeData b -> m Unit
insertLinkReshapingToolAt = insertToolAt @m newLinkReshapingTool

insertRelinkingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool RelinkingTool_ nodeData b -> m Unit
insertRelinkingToolAt = insertToolAt @m newRelinkingTool

insertResizingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool ResizingTool_ nodeData b -> m Unit
insertResizingToolAt = insertToolAt @m newResizingTool

insertRotatingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool RotatingTool_ nodeData b -> m Unit
insertRotatingToolAt = insertToolAt @m newRotatingTool

insertDraggingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool DraggingTool_ nodeData b -> m Unit
insertDraggingToolAt = insertToolAt @m newDraggingTool

insertDragSelectingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool DragSelectingTool_ nodeData b -> m Unit
insertDragSelectingToolAt = insertToolAt @m newDragSelectingTool

insertLinkingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool LinkingTool_ nodeData b -> m Unit
insertLinkingToolAt = insertToolAt @m newLinkingTool

insertPanningToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool PanningTool_ nodeData b -> m Unit
insertPanningToolAt = insertToolAt @m newPanningTool

insertClickCreatingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool ClickCreatingTool_ nodeData b -> m Unit
insertClickCreatingToolAt = insertToolAt @m newClickCreatingTool

insertClickSelectingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool ClickSelectingTool_ nodeData b -> m Unit
insertClickSelectingToolAt = insertToolAt @m newClickSelectingTool

insertContextMenuToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool ContextMenuTool_ nodeData b -> m Unit
insertContextMenuToolAt = insertToolAt @m newContextMenuTool

insertTextEditingToolAt :: forall nodeData b m. ToolM m nodeData ⇒ MouseActionType -> Int -> MakeTool TextEditingTool_ nodeData b -> m Unit
insertTextEditingToolAt = insertToolAt @m newTextEditingTool

-- TODO: The ToolFields class has to be richer because it needs to constrain not only the tool's settable
-- fields, but anything that *depends on nodeData* (like archetypeNodeData). As it stands, fields that construct
-- records based on nodeData are *completely untype-safe*.
class ToolFields (ffiType :: Type) (fields :: Row Type) | ffiType -> fields

instance ToolFields ClickCreatingTool_ (ClickCreatingToolFields nodeData)
instance ToolFields DragSelectingTool_ DragSelectingToolFields

instance (ToolFields ffiType settable) => Settable (MakeTool ffiType nodeData) settable where
  set fields = MakeTool $ setImp fields