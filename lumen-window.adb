
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

package body Lumen.Window is

   ---------------------------------------------------------------------------

   -- The (opaque) native window type
   type Window_Info is null record;

   ---------------------------------------------------------------------------

   -- The (opaque) rendering context type
   type Context_Info is null record;

   ---------------------------------------------------------------------------

   -- Create a native window
   procedure Create (Win     :    out Handle;
                     Options : in     Configuration) is
   begin  -- Create
      null;
   end Create;

   ---------------------------------------------------------------------------

   -- Destroy a native window, including its current rendering context.
   procedure Destroy (Win : in out Handle) is
   begin  -- Destroy
      null;
   end Destroy;

   ---------------------------------------------------------------------------

   -- Create an OpenGL rendering context; needed only when you want a second
   -- or subsequent context for a window, since Create makes one to start
   -- with
   procedure Create_Context (Win     : in     Handle;
                             Context :    out Context_Handle) is
   begin  -- Create_Context
      null;
   end Create_Context;

   ---------------------------------------------------------------------------

   -- Destroy an OpenGL rendering context
   procedure Destroy_Context (Win     : in     Handle;
                              Context :    out Context_Handle) is
   begin  -- Destroy_Context
      null;
   end Destroy_Context;

   ---------------------------------------------------------------------------

   -- Make a rendering context the current one for a window
   procedure Make_Current (Win     : in out Handle;
                           Context : in     Context_Handle) is
   begin  -- Make_Current
      null;
   end Make_Current;

   ---------------------------------------------------------------------------

end Lumen.Window;
