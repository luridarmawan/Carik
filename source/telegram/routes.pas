unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses main, command_controller;

initialization
  Route['/command'] := TCommandController;
  Route['/'] := TTelegramModule;
  //Route['main'] := TTelegramModule;

end.

