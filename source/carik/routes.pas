unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

implementation

uses info_controller, main;

initialization
  Route.Add( 'main', TMainModule);
  Route.Add( 'info', TInfoModule);

end.

