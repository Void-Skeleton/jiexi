{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Jiexi.Foreign(
  typstGetInput,
  typstSendOutput
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign
import GHC.Exts

#if defined(wasi_HOST_OS)
-- These two foreign imports are mandatory for interacting with Typst,
--  and they shouldn't be modified.
foreign import ccall "wasm_minimal_protocol_write_args_to_buffer"
  wasm_minimal_protocol_write_args_to_buffer :: Addr# -> IO ()

foreign import ccall "wasm_minimal_protocol_send_result_to_host"
  wasm_minimal_protocol_send_result_to_host :: Addr# -> Int# -> IO ()
#else
wasm_minimal_protocol_write_args_to_buffer :: Addr# -> IO ()
wasm_minimal_protocol_write_args_to_buffer = error "non-WASI platform"

wasm_minimal_protocol_send_result_to_host :: Addr# -> Int# -> IO ()
wasm_minimal_protocol_send_result_to_host = error "non-WASI platform"
#endif

typstGetInput :: Int -> IO ByteString
typstGetInput size = do
  allocaBytes size $ \ptr@(Ptr addr#) -> do
    wasm_minimal_protocol_write_args_to_buffer addr#
    BS.packCStringLen (ptr, size)

typstSendOutput :: ByteString -> IO ()
typstSendOutput bs = BS.useAsCStringLen bs $ \(Ptr addr#, I# len#) -> do
  wasm_minimal_protocol_send_result_to_host addr# len#

