
-- Lumen.Window -- Create and destroy native windows and associated OpenGL
-- rendering contexts
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

-- This code is covered by the ISC License:
--
-- Copyright (c) 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
-- SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
-- IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

package Lumen.Window is

   -- Handle for a native window
   type Handle is private;

   -- Handle for an OpenGL rendering context
   type Context_Handle is private;

   -- All the various settings a window can have
   type Configuration is null record;

   -- Local exceptions raised by these procedures
   Connection_Failed : exception;  -- can't connect to X server
   Context_Failed    : exception;  -- can't create or attach OpenGL context

   -- Create a native window
   procedure Create (Win     :    out Handle;
                     Options : in     Configuration);

   -- Destroy a native window, including its current rendering context.
   procedure Destroy (Win : in out Handle);

   -- Create an OpenGL rendering context; needed only when you want a second
   -- or subsequent context for a window, since Create makes one to start
   -- with
   procedure Create_Context (Win     : in     Handle;
                             Context :    out Context_Handle);

   -- Destroy an OpenGL rendering context
   procedure Destroy_Context (Win     : in     Handle;
                              Context :    out Context_Handle);

   -- Make a rendering context the current one for a window
   procedure Make_Current (Win     : in out Handle;
                           Context : in     Context_Handle);


private

   -- The (opaque) native window type
   type Window_Info;
   type Handle is access Window_Info;

   -- The (opaque) rendering context type
   type Context_Info;
   type Context_Handle is access Context_Info;

   pragma Linker_Options ("-lX11");
   pragma Linker_Options ("-lGL");

end Lumen.Window;
