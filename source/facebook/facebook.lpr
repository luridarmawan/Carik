program facebook;

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}
  cthreads,
  {$ENDIF}
  fpcgi, sysutils, fastplaz_handler, common, main, routes;

begin
  Application.Title:='Facebook';
  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,'webmaster@' + GetEnvironmentVariable('SERVER_NAME')));
  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;

  Application.Initialize;
  Application.Run;
end.
