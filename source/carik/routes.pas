unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler, loader_controller;

implementation

uses main, command_controller;

initialization
  Route[ '/command'] := TCommandController;
  Route[ '/loader'] := TLoaderController;
  Route[ '/'] := TCarikModule; // Main Module

end.

