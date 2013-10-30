

package openGL.Server
--
--  Provides functions to query the GL server.
--
is

   type a_Version is
      record
         Major   : Integer;
         Minor   : Integer;
      end record;

   function Version return a_Version;
   function Version return String;

end openGL.Server;
