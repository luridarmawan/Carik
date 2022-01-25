unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses main;

initialization
  Route[ '/'] := TFacebookModule; // Main Module

end.

