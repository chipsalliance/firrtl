{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Scala where

import Java hiding (Iterable)
import Prelude hiding (Traversable)

data Function1 t r = Function1 (@scala.Function1 t r)
    deriving Class

foreign import java unsafe "@wrapper apply"
    fun1 :: (t <: Object, r <: Object)
         => (t -> Java (Function1 t r) r) -> Function1 t r
foreign import java unsafe "apply"
    fun1Apply :: (t <: Object, r <: Object) => Function1 t r -> t -> r

data Traversable a       = Traversable     (@scala.collection.Traversable a)       deriving Class
data GenIterableLike a b = GenIterableLike (@scala.collection.GenIterableLike a b) deriving Class
data IterableLike a b    = IterableLike    (@scala.collection.IterableLike a b)    deriving Class
data GenIterable a       = GenIterable     (@scala.collection.GenIterable a)       deriving Class
data Iterable a          = Iterable        (@scala.collection.Iterable a)          deriving Class
data GenSeqLike a b      = GenSeqLike      (@scala.collection.GenSeqLike a b)      deriving Class
data SeqLike a b         = SeqLike         (@scala.collection.SeqLike a b)         deriving Class
data Seq a               = Seq             (@scala.collection.Seq a)               deriving Class

type instance Inherits (Iterable a)     = '[Traversable a, IterableLike a (Iterable a),
                                            GenIterable a]
type instance Inherits (GenSeqLike a b) = '[GenIterableLike a b]
type instance Inherits (SeqLike a b)    = '[GenSeqLike a b]
type instance Inherits (Seq a)          = '[Iterable a, SeqLike a (Seq a)]

foreign import java unsafe "@interface foreach"
    foreach :: (a <: Object, t <: IterableLike a (Iterable a))
            => Function1 (Seq a) b -> Java t ()

foreign import java unsafe "@interface map"
    seqMap :: (a <: Object, b <: Object,
               t <: GenIterableLike a (Seq a), u <: GenIterableLike b (Seq b))
            => Function1 a b -> Java t u
